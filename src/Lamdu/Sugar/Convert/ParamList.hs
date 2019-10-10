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
import           Hyper.Unify (Unify, UVarOf, unify)
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
import qualified Lamdu.Expr.IRef as ExprIRef
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

loadStored :: Monad m => ExprIRef.ValP m -> T m (Maybe ParamList)
loadStored = Property.getP . assocFieldParamList . Property.value

mkFuncType ::
    (Unify m Type, Unify m T.Row) =>
    ParamList -> m (Tree (UVarOf m) Type)
mkFuncType paramList =
    FuncType
    <$> (foldr step (newTerm T.REmpty) paramList <&> T.TRecord >>= newTerm)
    <*> newUnbound
    <&> T.TFun
    >>= newTerm
    where
        step ::
            (Unify m Type, Unify m T.Row) =>
            T.Tag ->
            m (Tree (UVarOf m) T.Row) ->
            m (Tree (UVarOf m) T.Row)
        step tag rest =
            RowExtend tag <$> newUnbound <*> rest <&> T.RExtend >>= newTerm

loadForLambdas ::
    Monad m =>
    Val (ValP m, Tree V.IResult UVar) -> T m (PureInfer ())
loadForLambdas x =
    Lens.itraverseOf ExprLens.subExprPayloads loadLambdaParamList x
    <&> \exprWithLoadActions -> exprWithLoadActions ^.. Lens.from _HFlip . hfolded1 . Lens._Wrapped & sequence_
    where
        loadLambdaParamList (Pure V.BLam {}) pl = loadUnifyParamList pl
        loadLambdaParamList _ _ = pure (pure ())

        loadUnifyParamList ::
            Monad m =>
            (ValP m, Tree V.IResult UVar) ->
            T m (PureInfer ())
        loadUnifyParamList (stored, ires) =
            loadStored stored
            <&> \case
            Nothing -> pure ()
            Just paramList ->
                do
                    funcType <- mkFuncType paramList
                    () <$ unify (ires ^. V.iType) funcType
