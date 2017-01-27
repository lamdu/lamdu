{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.DefExpr.OutdatedDefs
    ( scan
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Scheme (Scheme, schemeType, alphaEq)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Data.Ops.Subexprs as SubExprs
import           Lamdu.Expr.IRef (DefI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Types (DefinitionOutdatedType(..))

import           Lamdu.Prelude

type T = Transaction

changeFuncResult :: Monad m => V.Var -> Val (ValIProperty m) -> T m ()
changeFuncResult usedDefVar (Val pl (V.BLeaf (V.LVar v))) =
    when (v == usedDefVar) (void (DataOps.wrap pl))
changeFuncResult usedDefVar
    (Val pl (V.BApp (V.Apply (Val _ (V.BLeaf (V.LVar v))) arg)))
    | v == usedDefVar =
        do
            DataOps.wrap pl & void
            changeFuncResult usedDefVar arg
changeFuncResult usedDefVar val =
    val ^. Val.body & traverse_ (changeFuncResult usedDefVar)

isFuncResultChange :: Scheme -> Scheme -> Bool
isFuncResultChange prevType newType =
    do
        prevArg <- prevType & schemeType %%~ (^? T._TFun . _1)
        newArg <- newType & schemeType %%~ (^? T._TFun . _1)
        alphaEq prevArg newArg & guard
    & Lens.has Lens._Just

fixDefExpr ::
    Monad m => Scheme -> Scheme -> V.Var -> Val (ValIProperty m) -> T m ()
fixDefExpr prevType newType usedDefVar defExpr
    | isFuncResultChange prevType newType = changeFuncResult usedDefVar defExpr
    | otherwise = SubExprs.onGetVars (DataOps.wrap <&> void) usedDefVar defExpr

updateDefType ::
    Monad m =>
    Scheme -> Scheme -> V.Var ->
    Def.Expr (Val (ValIProperty m)) -> DefI m -> T m ()
updateDefType prevType newType usedDefVar defExpr usingDefI =
    do
        fixDefExpr prevType newType usedDefVar (defExpr ^. Def.expr)
        defExpr <&> (^. Val.payload) <&> Property.value
            & Def.exprUsedDefinitions . Lens.at usedDefVar .~ Just newType
            & Def.BodyExpr
            & Transaction.writeIRef usingDefI

scan ::
    Monad m =>
    Def.Expr (Val (ValIProperty m)) -> DefI m ->
    T m (Map V.Var (DefinitionOutdatedType m))
scan defExpr defI =
    defExpr ^. Def.exprUsedDefinitions
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
                    updateDefType usedType defType globalVar defExpr defI
                }
                & Map.singleton globalVar
