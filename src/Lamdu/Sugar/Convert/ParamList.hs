{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}
-- | Manage, read, write lambda-associated param lists
module Lamdu.Sugar.Convert.ParamList
    ( ParamList, loadForLambdas
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Property as Property
import           Data.Tree.Diverse (Ann(..), annotations)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (assocFieldParamList)
import           Lamdu.Data.Meta (ParamList)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Infer (Infer)
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Trans as InferT
import           Lamdu.Infer.Unify (unify)
import           Lamdu.Infer.Update (update)
import qualified Lamdu.Infer.Update as Update
import qualified Lamdu.Sugar.Convert.Input as Input
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

class Monad (M a) => ParamListPayload a where
    type M a :: * -> *
    iref :: a -> ExprIRef.ValP (M a)
    inferPl :: Lens' a Infer.Payload

instance Monad m => ParamListPayload (Input.Payload m a) where
    type M (Input.Payload m a) = m
    iref x = x ^. Input.stored
    inferPl = Input.inferred

instance Monad m => ParamListPayload (Infer.Payload, ExprIRef.ValP m) where
    type M (Infer.Payload, ExprIRef.ValP m) = m
    iref (_, x) = x
    inferPl = _1

loadStored :: Monad m => ExprIRef.ValP m -> T m (Maybe ParamList)
loadStored = Property.getP . assocFieldParamList . Property.value

mkFuncType :: Infer.Scope -> ParamList -> Infer Type
mkFuncType scope paramList =
    T.TFun
    <$> (T.TRecord <$> foldr step (pure T.CEmpty) paramList)
    <*> Infer.freshInferredVar scope "l"
    where
        step tag rest = T.CExtend tag <$> Infer.freshInferredVar scope "t" <*> rest

loadForLambdas ::
    ParamListPayload a => Val a -> InferT.M (T (M a)) (Val a)
loadForLambdas x =
    do
        Lens.itraverseOf_ ExprLens.subExprPayloads loadLambdaParamList x
        x
            & annotations . inferPl . Infer.plType %%~ update
            >>= annotations . inferPl . Infer.plScope %%~ update
            & Update.run & State.gets
    where
        loadLambdaParamList (Ann _ V.BLam {}) pl = loadUnifyParamList pl
        loadLambdaParamList _ _ = pure ()

        loadUnifyParamList pl =
            do
                mParamList <- loadStored (iref pl) & InferT.liftInner
                case mParamList of
                    Nothing -> pure ()
                    Just paramList ->
                        do
                            funcType <-
                                mkFuncType (pl ^. inferPl . Infer.plScope) paramList
                            unify (pl ^. inferPl . Infer.plType) funcType
                        & InferT.liftInfer
