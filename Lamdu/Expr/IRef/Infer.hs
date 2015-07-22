-- | Infer expressions where GlobalId's are known to be DefI's
module Lamdu.Expr.IRef.Infer
    ( ExpressionSetter

    , M
    , loadInferScope
    , loadInferInto
    , loadInfer
    , loadNominal
    , Error(..), toEitherT
    ) where

import           Control.Lens (_Left)
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Either (EitherT(..), left, hoistEither)
import           Control.Monad.Trans.State (StateT(..), mapStateT)
import           Control.MonadA (MonadA)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Nominal (Nominal)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.Infer (Infer)
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Error as InferErr
import           Lamdu.Infer.Load (Loader(Loader))
import qualified Lamdu.Infer.Load as InferLoad
import qualified Lamdu.Infer.Recursive as Recursive
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

loader :: MonadA m => Loader (EitherT Error (T m))
loader =
    Loader
    { InferLoad.loadTypeOf = \globalId ->
        do
            defBody <- lift $ Transaction.readIRef $ ExprIRef.defI globalId
            case defBody of
                Definition.BodyExpr (Definition.Expr _ (Definition.ExportedType scheme)) ->
                    return scheme
                Definition.BodyBuiltin (Definition.Builtin _ scheme) -> return scheme
                _ -> left UnexportedGlobalReferred -- Reference to global with non-exported type!
    , InferLoad.loadNominal = lift . loadNominal
    }

loadNominal :: MonadA m => T.Id -> T m Nominal
loadNominal tid =
    do
        e <- Transaction.irefExists iref
        if e
            then Transaction.readIRef iref
            else fail "Missing Nominal Value"
    where
        iref = ExprIRef.nominalI tid

type M m = StateT Infer.Context (EitherT Error (T m))

toEitherT :: Monad m => Either InferErr.Error a -> EitherT Error m a
toEitherT = hoistEither . (_Left %~ InferError)

liftInfer :: Monad m => Infer a -> M m a
liftInfer = mapStateT toEitherT . Infer.run

loadInferScope ::
    MonadA m => Infer.Scope -> Val a -> M m (Val (Infer.Payload, a))
loadInferScope scope val =
    do
        inferAction <- lift $ InferLoad.loadInfer loader scope val
        liftInfer inferAction

loadInferInto ::
    MonadA m => Infer.Payload -> Val a -> M m (Val (Infer.Payload, a))
loadInferInto pl val =
    do
        inferredVal <- loadInferScope (pl ^. Infer.plScope) val
        let inferredType = inferredVal ^. V.payload . _1 . Infer.plType
        liftInfer $
            do
                unify inferredType (pl ^. Infer.plType)
                Update.inferredVal inferredVal & Update.liftInfer

loadInfer ::
    MonadA m => V.Var -> Val a ->
    EitherT Error (T m) (Val (Infer.Payload, a), Infer.Context)
loadInfer recurseVar val =
    liftInfer (Recursive.inferEnv recurseVar Infer.emptyScope)
    >>= (`loadInferInto` val)
    & (`runStateT` Infer.initialContext)

