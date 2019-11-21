{-# LANGUAGE TypeFamilies #-}
-- | Manage, read, write lambda-associated param lists
module Lamdu.Sugar.Convert.ParamList
    ( ParamList, loadForLambdas
    ) where

import qualified Control.Lens as Lens
import qualified Data.Property as Property
import           Hyper (Tree, Pure(..), _HFlip, hfolded1)
import           Hyper.Type.AST.FuncType (FuncType(..))
import           Hyper.Type.AST.Row (RowExtend(..))
import           Hyper.Unify (UnifyGen, UVarOf, unify)
import           Hyper.Unify.Binding (UVar)
import           Hyper.Unify.New (newUnbound, newTerm)
import           Lamdu.Calc.Infer (PureInfer)
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Data.Anchors (assocFieldParamList)
import           Lamdu.Data.Meta (ParamList)
import           Lamdu.Expr.IRef (ValP)
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

mkFuncType ::
    (UnifyGen m Type, UnifyGen m T.Row) =>
    ParamList -> m (Tree (UVarOf m) Type)
mkFuncType paramList =
    FuncType
    <$> (foldr step (newTerm T.REmpty) paramList <&> T.TRecord >>= newTerm)
    <*> newUnbound
    <&> T.TFun
    >>= newTerm
    where
        step ::
            (UnifyGen m Type, UnifyGen m T.Row) =>
            T.Tag ->
            m (Tree (UVarOf m) T.Row) ->
            m (Tree (UVarOf m) T.Row)
        step tag rest =
            RowExtend tag <$> newUnbound <*> rest <&> T.RExtend >>= newTerm

loadForLambdas ::
    Monad m =>
    Val (ValP m, Tree UVar T.Type) -> T m (PureInfer (Tree V.Scope UVar) ())
loadForLambdas x =
    Lens.itraverseOf ExprLens.subExprPayloads loadLambdaParamList x
    <&> \exprWithLoadActions -> exprWithLoadActions ^.. Lens.from _HFlip . hfolded1 . Lens._Wrapped & sequence_
    where
        loadLambdaParamList (Pure V.BLam {}) pl = loadUnifyParamList pl
        loadLambdaParamList _ _ = pure (pure ())

        loadUnifyParamList ::
            Monad m =>
            (ValP m, Tree UVar T.Type) ->
            T m (PureInfer (Tree V.Scope UVar) ())
        loadUnifyParamList (stored, ires) =
            stored ^. Property.pVal & assocFieldParamList & Property.getP
            <&> \case
            Nothing -> pure ()
            Just paramList ->
                do
                    funcType <- mkFuncType paramList
                    () <$ unify ires funcType
