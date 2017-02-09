{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.DefExpr.OutdatedDefs
    ( scan
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Calc.Type (Type)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Scheme (Scheme, schemeType, alphaEq)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (ValI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.Types (DefinitionOutdatedType(..))

import           Lamdu.Prelude

type T = Transaction

data IsHoleArg = IsHoleArg | NotHoleArg deriving Eq

holeWrapped :: Lens.Traversal' (Val a) (Val a)
holeWrapped =
    ExprLens.valApply .
    Lens.filtered (Lens.has (V.applyFunc . ExprLens.valHole)) .
    V.applyArg

recursivelyFixExpr ::
    Monad m =>
    (IsHoleArg -> Val a -> Maybe ((IsHoleArg -> Val a -> m ()) -> m ())) ->
    Val a -> m ()
recursivelyFixExpr mFix =
    go NotHoleArg
    where
        go isHoleArg expr =
            case mFix isHoleArg expr of
            Just fix -> fix go
            Nothing -> recurse expr
        recurse val =
            case val ^? holeWrapped of
            Just arg -> go IsHoleArg arg
            Nothing -> traverse_ (go NotHoleArg) (val ^. Val.body)

changeFuncRes :: Monad m => V.Var -> Val (ValIProperty m) -> T m ()
changeFuncRes usedDefVar =
    recursivelyFixExpr mFix
    where
        mFix NotHoleArg (Val pl (V.BLeaf (V.LVar v)))
            | v == usedDefVar = DataOps.wrap pl & void & const & Just
        mFix isHoleArg
            (Val pl (V.BApp (V.Apply (Val _ (V.BLeaf (V.LVar v))) arg)))
            | v == usedDefVar =
            Just $
            \go ->
            do
                when (isHoleArg == NotHoleArg) (void (DataOps.wrap pl))
                go NotHoleArg arg
        mFix _ _ = Nothing

wrap :: Monad m => Val (ValIProperty m) -> T m ()
wrap val
    | Lens.has holeWrapped val
    || Lens.has ExprLens.valHole val = return ()
    | otherwise = val ^. Val.payload & DataOps.wrap & void

changeFuncArg :: Monad m => V.Var -> Val (ValIProperty m) -> T m ()
changeFuncArg usedDefVar =
    recursivelyFixExpr mFix
    where
        mFix NotHoleArg (Val pl (V.BLeaf (V.LVar v)))
            | v == usedDefVar = DataOps.wrap pl & void & const & Just
        mFix _ (Val _ (V.BApp (V.Apply (Val _ (V.BLeaf (V.LVar v))) arg)))
            | v == usedDefVar = Just $ \go -> wrap arg >> go IsHoleArg arg
        mFix _ _ = Nothing

isPartSame ::
    (Lens.Getting (Monoid.First Type) Type Type) -> Scheme -> Scheme -> Bool
isPartSame part preType newType =
    do
        prePart <- preType & schemeType %%~ (^? part)
        newPart <- newType & schemeType %%~ (^? part)
        alphaEq prePart newPart & guard
    & Lens.has Lens._Just

isFuncResChange :: Scheme -> Scheme -> Bool
isFuncResChange = isPartSame (T._TFun . _1)

isFuncArgChange :: Scheme -> Scheme -> Bool
isFuncArgChange = isPartSame (T._TFun . _2)

fixDefExpr ::
    Monad m => Scheme -> Scheme -> V.Var -> Val (ValIProperty m) -> T m ()
fixDefExpr prevType newType usedDefVar defExpr
    | isFuncResChange prevType newType = changeFuncRes usedDefVar defExpr
    | isFuncArgChange prevType newType = changeFuncArg usedDefVar defExpr
    | otherwise = SubExprs.onGetVars (DataOps.wrap <&> void) usedDefVar defExpr

updateDefType ::
    Monad m =>
    Scheme -> Scheme -> V.Var ->
    Def.Expr (Val (ValIProperty m)) -> (Def.Expr (ValI m) -> T m ()) ->
    T m ()
updateDefType prevType newType usedDefVar defExpr setDefExpr =
    do
        fixDefExpr prevType newType usedDefVar (defExpr ^. Def.expr)
        defExpr
            <&> (^. Val.payload . Property.pVal)
            & Def.exprFrozenDeps . Infer.depsGlobalTypes . Lens.at usedDefVar ?~ newType
            & setDefExpr

scan ::
    Monad m =>
    Def.Expr (Val (ValIProperty m)) -> (Def.Expr (ValI m) -> T m ()) ->
    T m (Map V.Var (DefinitionOutdatedType m))
scan defExpr setDefExpr =
    defExpr ^. Def.exprFrozenDeps . Infer.depsGlobalTypes
    & Map.toList & mapM (uncurry scanDef) <&> mconcat
    where
        scanDef globalVar usedType =
            ExprIRef.defI globalVar & Transaction.readIRef
            <&> Def.typeOfDefBody
            <&> processDef globalVar usedType
        processDef globalVar usedType defType
            | alphaEq usedType defType = Map.empty
            | otherwise =
                DefinitionOutdatedType
                { _defTypeWhenUsed = usedType
                , _defTypeCurrent = defType
                , _defTypeUseCurrent =
                    updateDefType usedType defType globalVar defExpr setDefExpr
                }
                & Map.singleton globalVar
