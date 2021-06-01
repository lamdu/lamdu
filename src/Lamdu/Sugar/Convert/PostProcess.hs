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
import           Hyper.Unify (UVar)
import           Hyper.Unify.Generalize (generalize)
import           Lamdu.Calc.Infer (runPureInfer)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Debug as Debug
import           Lamdu.Expr.IRef (DefI, ValI, HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Load as Load
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

data Result = GoodExpr | BadExpr (Pure # T.TypeError)

makeScheme ::
    Load.InferOut m ->
    Either (Pure # T.TypeError) (Pure # T.Scheme)
makeScheme (Load.InferOut inferredVal inferContext) =
    generalize (inferredVal ^. hAnn . Input.inferredTypeUVar)
    >>= saveScheme
    & runPureInfer @(V.Scope # UVar) V.emptyScope inferContext
    <&> (^. _1)

def :: Monad m => Load.InferFunc (HRef m) -> Debug.Monitors -> DefI m -> T m Result
def infer monitors defI =
    do
        loadedDef <- ExprLoad.def defI <&> void
        case loadedDef ^. Definition.defBody of
            Definition.BodyBuiltin {} -> pure GoodExpr
            Definition.BodyExpr defExpr ->
                ExprIRef.globalId defI
                & Load.inferDef infer monitors defExpr
                >>= makeScheme
                &
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
                        (^. hAnn . ExprIRef.iref)
                    & Transaction.writeIRef defI
                    )

expr ::
    Monad m =>
    Load.InferFunc (HRef m) -> Debug.Monitors ->
    MkProperty' (T m) (Definition.Expr (ValI m)) ->
    T m Result
expr infer monitors prop =
    do
        defExprLoaded <- ExprLoad.defExpr prop
        -- TODO: This is code duplication with the above Load.inferCheckDef
        -- & functions inside Load itself
        let inferred = Load.inferDefExpr infer monitors defExprLoaded
        case inferred of
            Left err -> BadExpr err & pure
            Right _ ->
                GoodExpr <$
                Property.modP prop
                (Definition.exprFrozenDeps .~ Definition.pruneDefExprDeps defExprLoaded)

