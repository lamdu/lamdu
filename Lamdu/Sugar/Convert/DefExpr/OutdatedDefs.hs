{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.DefExpr.OutdatedDefs
    ( scan
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Calc.Type.Scheme (Scheme, alphaEq)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Expr.IRef (DefI, ValIProperty)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Types (DefinitionOutdatedType(..))

import           Lamdu.Prelude

type T = Transaction

updateDefType ::
    Monad m =>
    Scheme -> Scheme -> V.Var ->
    Def.Expr (Val (ValIProperty m)) -> DefI m -> T m ()
updateDefType _prevType newType usedDefVar defExpr usingDefI =
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
