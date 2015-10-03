{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
-- | Manage, read, write lambda-associated param lists
module Lamdu.Sugar.Convert.ParamList
    ( ParamList, loadForLambdas
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.State (mapStateT)
import qualified Control.Monad.Trans.State as State
import           Control.MonadA (MonadA)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Data.Anchors (ParamList, assocFieldParamList)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.IRef.Infer as IRefInfer
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.Infer (Infer)
import qualified Lamdu.Infer as Infer
import           Lamdu.Infer.Unify (unify)
import           Lamdu.Infer.Update (update)
import qualified Lamdu.Infer.Update as Update
import qualified Lamdu.Sugar.Convert.Input as Input

type T = Transaction

loadStored :: MonadA m => ExprIRef.ValIProperty m -> T m (Maybe ParamList)
loadStored = Transaction.getP . assocFieldParamList . Property.value

mkFuncType :: Infer.Scope -> ParamList -> Infer Type
mkFuncType scope paramList =
    T.TFun
    <$> (T.TRecord <$> foldr step (pure T.CEmpty) paramList)
    <*> Infer.freshInferredVar scope "l"
    where
        step tag rest = T.CExtend tag <$> Infer.freshInferredVar scope "t" <*> rest

loadForLambdas ::
    MonadA m => Val (Input.Payload m a) -> IRefInfer.M m (Val (Input.Payload m a))
loadForLambdas val =
    do
        Lens.itraverseOf_ ExprLens.subExprPayloads loadLambdaParamList val
        val
            & traverse . Input.inferredType %%~ update
            >>= traverse . Input.inferredScope %%~ update
            & Update.run & State.gets
    where
        loadLambdaParamList (V.Val _ V.BAbs {}) pl = loadUnifyParamList pl
        loadLambdaParamList _ _ = return ()

        loadUnifyParamList pl =
            case pl ^. Input.mStored of
            Nothing -> return ()
            Just stored ->
                do
                    mParamList <- loadStored stored & lift & lift
                    case mParamList of
                        Nothing -> return ()
                        Just paramList ->
                            do
                                funcType <-
                                    mkFuncType (pl ^. Input.inferredScope) paramList
                                unify (pl ^. Input.inferredType) funcType
                            & Infer.run
                            & mapStateT IRefInfer.toEitherT
