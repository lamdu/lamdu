{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | Manage, read, write lambda-associated param lists
module Lamdu.Sugar.Convert.ParamList
    ( ParamList, loadForLambdas
    ) where

import           AST (Tree, Ann(..), annotations)
import           AST.Infer (IResult, irType)
import           AST.Term.FuncType (FuncType(..))
import           AST.Term.Row (RowExtend(..))
import           AST.Unify (Unify, UVarOf, unify, newUnbound, newTerm)
import           AST.Unify.Binding (UVar)
import qualified Control.Lens as Lens
import qualified Data.Property as Property
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
        step tag rest =
            RowExtend tag <$> newUnbound <*> rest <&> T.RExtend >>= newTerm

loadForLambdas ::
    Monad m =>
    Val (ValP m, IResult UVar V.Term) -> T m (PureInfer ())
loadForLambdas x =
    Lens.itraverseOf ExprLens.subExprPayloads loadLambdaParamList x
    <&> \exprWithLoadActions -> exprWithLoadActions ^.. annotations & sequence_
    where
        loadLambdaParamList (Ann _ V.BLam {}) pl = loadUnifyParamList pl
        loadLambdaParamList _ _ = pure (pure ())

        loadUnifyParamList (stored, ires) =
            loadStored stored
            <&> \case
            Nothing -> pure ()
            Just paramList ->
                do
                    funcType <- mkFuncType paramList
                    () <$ unify (ires ^. irType) funcType
