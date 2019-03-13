{-# LANGUAGE TypeFamilies #-}
-- | Manage, read, write lambda-associated param lists
module Lamdu.Sugar.Convert.ParamList
    ( ParamList, loadForLambdas
    ) where

import           AST (Ann(..), annotations)
import qualified Control.Lens as Lens
import qualified Data.Property as Property
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (assocFieldParamList)
import           Lamdu.Data.Meta (ParamList)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Infer (Infer)
import qualified Lamdu.Infer as Infer
import           Lamdu.Infer.Unify (unify)
import           Lamdu.Infer.Update (update)
import qualified Lamdu.Infer.Update as Update
import qualified Lamdu.Sugar.Convert.Input as Input
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

loadStored :: Monad m => ExprIRef.ValP m -> T m (Maybe ParamList)
loadStored = Property.getP . assocFieldParamList . Property.value

mkFuncType :: Infer.Scope -> ParamList -> Infer Type
mkFuncType scope paramList =
    T.TFun
    <$> (T.TRecord <$> foldr step (pure T.REmpty) paramList)
    <*> Infer.freshInferredVar scope "l"
    where
        step tag rest = T.RExtend tag <$> Infer.freshInferredVar scope "t" <*> rest

loadForLambdas ::
    Monad m => Val (Input.Payload m a) -> T m (Infer (Val (Input.Payload m a)))
loadForLambdas x =
    Lens.itraverseOf ExprLens.subExprPayloads loadLambdaParamList x
    <&> \exprWithLoadActions ->
    do
        exprWithLoadActions ^.. annotations & sequence_ -- runs all the load actions via mconcat
        x
            & annotations . Input.inferred . Infer.plType %%~ update
            >>= annotations . Input.inferred . Infer.plScope %%~ update
            & Update.liftInfer
    where
        loadLambdaParamList (Ann _ V.BLam {}) pl = loadUnifyParamList pl
        loadLambdaParamList _ _ = pure (pure ())

        loadUnifyParamList pl =
            loadStored (pl ^. Input.stored)
            <&> \case
            Nothing -> pure ()
            Just paramList ->
                do
                    funcType <-
                        mkFuncType (pl ^. Input.inferred . Infer.plScope) paramList
                    unify (pl ^. Input.inferred . Infer.plType) funcType
