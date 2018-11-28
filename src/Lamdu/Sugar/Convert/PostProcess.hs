module Lamdu.Sugar.Convert.PostProcess
    ( Result(..), def, expr
    ) where

import           AST.Ann (ann)
import qualified Control.Lens as Lens
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Debug as Debug
import qualified Lamdu.Eval.Results as EvalResults
import           Lamdu.Expr.IRef (DefI, ValI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Error as InferErr
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Load as Load
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

data Result = GoodExpr | BadExpr InferErr.Error

def :: Monad m => Load.InferFunc (ValP m) -> Debug.Monitors -> DefI m -> T m Result
def infer monitors defI =
    do
        loadedDef <- ExprLoad.def defI <&> void
        case loadedDef ^. Definition.defBody of
            Definition.BodyBuiltin {} -> pure GoodExpr
            Definition.BodyExpr defExpr ->
                do
                    checked <-
                        ExprIRef.globalId defI
                        & Load.inferDef infer monitors (pure EvalResults.empty) defExpr
                    case checked of
                        Left err -> BadExpr err & pure
                        Right (Load.InferResult inferredVal inferContext) ->
                            GoodExpr <$
                            ( loadedDef
                            & Definition.defType .~
                                Infer.makeScheme inferContext inferredType
                            & Definition.defBody . Definition._BodyExpr .
                                Definition.exprFrozenDeps .~
                                Definition.pruneDefExprDeps defExpr
                            & Definition.defBody . Lens.mapped %~
                                (^. ann . Property.pVal)
                            & Transaction.writeIRef defI
                            )
                            where
                                inferredType = inferredVal ^. ann . Input.inferred . Infer.plType

expr ::
    (HasCallStack, Monad m) =>
    Load.InferFunc (ValP m) -> Debug.Monitors ->
    MkProperty' (T m) (Definition.Expr (ValI m)) ->
    T m Result
expr infer monitors prop =
    do
        defExprLoaded <- ExprLoad.defExpr prop
        -- TODO: This is code duplication with the above Load.inferCheckDef
        -- & functions inside Load itself
        inferred <-
            Load.inferDefExpr infer monitors (pure EvalResults.empty) defExprLoaded
        case inferred of
            Left err -> BadExpr err & pure
            Right _ ->
                GoodExpr <$
                Property.modP prop
                (Definition.exprFrozenDeps .~ Definition.pruneDefExprDeps defExprLoaded)

