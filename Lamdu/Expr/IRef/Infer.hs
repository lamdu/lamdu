{-# LANGUAGE OverloadedStrings #-}
-- | Infer expressions where GlobalId's are known to be DefI's
module Lamdu.Expr.IRef.Infer
    ( ExpressionSetter
    , M
    , loadInferScope
    , loadInferRecursive
    , run
    , Error(..), toEitherT
    ) where

import           Control.Lens (_Left)
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Either (EitherT(..), hoistEither)
import           Control.Monad.Trans.State (StateT(..), mapStateT)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Calc.Type.Scheme (Scheme(..))
import qualified Lamdu.Calc.Type.Scheme as Scheme
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import           Lamdu.Infer (Infer)
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Error as InferErr
import           Lamdu.Infer.Load (Loader(Loader))
import qualified Lamdu.Infer.Load as InferLoad
import           Lamdu.Infer.Unify (unify)
import qualified Lamdu.Infer.Update as Update
import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint.HughesPJClass (Pretty(..))

type T = Transaction

type ExpressionSetter def = Val () -> Val ()

data Error = UnexportedGlobalReferred | InferError InferErr.Error

instance Pretty Error where
    pPrint UnexportedGlobalReferred = PP.text "Unexported global referred"
    pPrint (InferError e) = pPrint e

unknownGlobalType :: Scheme
unknownGlobalType = Scheme.any

typeOfDefBody :: Definition.Body a -> Scheme
typeOfDefBody (Definition.BodyExpr defExpr) =
    case defExpr ^. Definition.exprType of
    Definition.ExportedType scheme -> scheme
    Definition.NoExportedType -> unknownGlobalType
typeOfDefBody (Definition.BodyBuiltin (Definition.Builtin _ scheme)) = scheme

loader :: Monad m => Loader (EitherT Error (T m))
loader =
    Loader
    { InferLoad.loadTypeOf =
        \globalId ->
        ExprIRef.defI globalId & Transaction.readIRef & lift <&> typeOfDefBody
    , InferLoad.loadNominal = lift . Load.nominal
    }

type M m = StateT Infer.Context (EitherT Error (T m))

toEitherT :: Monad m => Either InferErr.Error a -> EitherT Error m a
toEitherT = hoistEither . (_Left %~ InferError)

liftInfer :: Monad m => Infer a -> M m a
liftInfer = mapStateT toEitherT . Infer.run

loadInferScope ::
    Monad m => Infer.Scope -> Val a -> M m (Val (Infer.Payload, a))
loadInferScope scope val =
    InferLoad.loadInfer loader scope val & lift >>= liftInfer

loadInferInto ::
    Monad m => Infer.Payload -> Val a -> M m (Val (Infer.Payload, a))
loadInferInto pl val =
    do
        inferredVal <- loadInferScope (pl ^. Infer.plScope) val
        let inferredType = inferredVal ^. Val.payload . _1 . Infer.plType
        liftInfer $
            do
                unify inferredType (pl ^. Infer.plType)
                Update.inferredVal inferredVal & Update.liftInfer

loadInferRecursive ::
    Monad m => ExprIRef.DefI m -> Val a -> M m (Val (Infer.Payload, a))
loadInferRecursive defI val =
    do
        defType <- Infer.freshInferredVar Infer.emptyScope "r" & liftInfer
        let scope =
                Infer.insertTypeOf (ExprIRef.globalId defI) defType Infer.emptyScope
        loadInferInto (Infer.Payload defType scope) val

run :: M m a -> T m (Either Error (a, Infer.Context))
run = runEitherT . (`runStateT` Infer.initialContext)
