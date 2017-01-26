{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.DefExpr.OutdatedDefs
    ( scan
    ) where

import qualified Data.Map as Map
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Lamdu.Calc.Type.Scheme (Scheme, alphaEq)
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Types (DefinitionOutdatedType(..))

import           Lamdu.Prelude

type T = Transaction

scan ::
    Monad m => Map V.Var Scheme -> T m (Map V.Var (DefinitionOutdatedType m))
scan usedDefinitions =
    Map.toList usedDefinitions & mapM (uncurry scanDef)
    <&> mconcat
    where
        scanDef globalVar usedType =
            ExprIRef.defI globalVar & Transaction.readIRef
            <&> Definition.typeOfDefBody
            <&> processDef globalVar usedType
        processDef globalVar usedType defType
            | alphaEq usedType defType = Map.empty
            | otherwise =
                DefinitionOutdatedType
                { _defTypeWhenUsed = usedType
                , _defTypeCurrent = defType
                , _defTypeUseCurrent = error "TODO"
                }
                & Map.singleton globalVar
