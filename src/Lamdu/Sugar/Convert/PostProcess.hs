{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.PostProcess
    ( PostProcessResult(..), postProcessDef, postProcessExpr
    ) where

import           Control.Monad.Trans.State (StateT(..))
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.IRef (DefI, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Error as InferErr
import qualified Lamdu.Sugar.Convert.Load as Load

import           Lamdu.Prelude

type T = Transaction

data PostProcessResult = GoodExpr | BadExpr InferErr.Error

postProcessDef :: Monad m => DefI m -> T m PostProcessResult
postProcessDef defI =
    do
        def <- Transaction.readIRef defI
        case def ^. Definition.defBody of
            Definition.BodyBuiltin {} -> return GoodExpr
            Definition.BodyExpr defExpr ->
                do
                    loaded <- Definition.expr ExprIRef.readVal defExpr
                    checked <- Load.inferCheckDef loaded (ExprIRef.globalId defI)
                    case checked of
                        Left err -> BadExpr err & return
                        Right (inferredVal, inferContext) ->
                            do
                                def
                                    & Definition.defType .~
                                        Infer.makeScheme inferContext inferredType
                                    & Definition.defBody . Definition._BodyExpr .
                                        Definition.exprFrozenDeps .~
                                        Definition.pruneDefExprDeps loaded
                                    & Transaction.writeIRef defI
                                return GoodExpr
                            where
                                inferredType = inferredVal ^. Val.payload . _1 . Infer.plType

postProcessExpr ::
    Monad m =>
    Transaction.MkProperty m (Definition.Expr (ValI m)) ->
    T m PostProcessResult
postProcessExpr mkProp =
    do
        prop <- mkProp ^. Transaction.mkProperty
        -- TODO: This is code duplication with the above Load.inferDef
        -- & functions inside Load itself
        defExpr <- Definition.expr ExprIRef.readVal (prop ^. Property.pVal)
        let inferred =
                Load.inferDefExpr Infer.emptyScope defExpr
                & Infer.run
                & (`runStateT` Infer.initialContext)
        case inferred of
            Left err -> BadExpr err & return
            Right _ ->
                do
                    Definition.exprFrozenDeps .~
                        Definition.pruneDefExprDeps defExpr
                        & Property.pureModify prop
                    return GoodExpr

