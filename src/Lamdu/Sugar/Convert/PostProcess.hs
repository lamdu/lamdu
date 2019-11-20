{-# LANGUAGE TypeApplications #-}

module Lamdu.Sugar.Convert.PostProcess
    ( Result(..), def, expr
    , makeScheme
    ) where

import qualified Control.Lens as Lens
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import           Hyper
import           Hyper.Type.AST.Scheme (saveScheme)
import           Hyper.Unify.Binding (UVar)
import           Hyper.Unify.Generalize (generalize)
import           Lamdu.Calc.Infer (runPureInfer)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Debug as Debug
import qualified Lamdu.Eval.Results as EvalResults
import           Lamdu.Expr.IRef (DefI, ValI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Load as Load
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

data Result = GoodExpr | BadExpr (Tree Pure T.TypeError)

makeScheme ::
    Load.InferOut m ->
    Either (Tree Pure T.TypeError) (Tree Pure T.Scheme)
makeScheme (Load.InferOut inferredVal inferContext) =
    generalize (inferredVal ^. annotation . Input.inferResult)
    >>= saveScheme
    & runPureInfer @(Tree V.Scope UVar) V.emptyScope inferContext
    <&> (^. Lens._1)

def :: Monad m => Load.InferFunc (ValP m) -> Debug.Monitors -> DefI m -> T m Result
def infer monitors defI =
    do
        loadedDef <- ExprLoad.def defI <&> void
        case loadedDef ^. Definition.defBody of
            Definition.BodyBuiltin {} -> pure GoodExpr
            Definition.BodyExpr defExpr ->
                ExprIRef.globalId defI
                & Load.inferDef infer monitors (pure EvalResults.empty) defExpr
                <&> (>>= makeScheme)
                >>=
                \case
                Left err -> BadExpr err & pure
                Right scheme ->
                    GoodExpr <$
                    ( loadedDef
                    & Definition.defType .~ scheme
                    & Definition.defBody . Definition._BodyExpr .
                        Definition.exprFrozenDeps .~
                        Definition.pruneDefExprDeps defExpr
                    & Definition.defBody . Lens.mapped %~
                        (^. annotation . Property.pVal)
                    & Transaction.writeIRef defI
                    )

expr ::
    Monad m =>
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

