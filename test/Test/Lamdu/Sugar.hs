module Test.Lamdu.Sugar where

import           Control.DeepSeq (NFData, deepseq)
import qualified Control.Lens as Lens
import           Control.Monad.Transaction (getP)
import qualified Data.Property as Property
import qualified Data.Set as Set
import qualified Lamdu.Cache as Cache
import           Lamdu.Calc.Val.Annotated (Val)
import           Lamdu.Data.Anchors (Code(..), paneDef)
import           Lamdu.Data.Db.Layout (ViewM, codeAnchors, runDbTransaction)
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Debug (noopMonitors)
import qualified Lamdu.Eval.Results as EvalResults
import           Lamdu.Expr.IRef (DefI, ValP)
import qualified Lamdu.Expr.Load as ExprLoad
import           Lamdu.GUI.CodeEdit.Load (loadWorkArea)
import           Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Lens (workAreaExpressions, subExprPayloads)
import           Lamdu.Sugar.Types as Sugar
import           Lamdu.VersionControl (runAction)
import           Revision.Deltum.Transaction (Transaction)
import           Test.Lamdu.Db (withDB)

import           Test.Lamdu.Prelude

type T = Transaction

adhocBodyPayloads :: Lens.Traversal' (Body name i o expr) (Sugar.Payload name i o ())
adhocBodyPayloads f (BodyLabeledApply x) =
    x & aFunc . afPayload %%~ f <&> BodyLabeledApply
adhocBodyPayloads f (BodyInject x) =
    x & iMVal . _InjectNullary . nullaryPayload %%~ f <&> BodyInject
adhocBodyPayloads _ x = pure x

allEntityIds :: WorkArea name i o ExprGui.Payload -> Set EntityId
allEntityIds workArea =
    pls ^.. Lens.folded . plData . plHiddenEntityIds . Lens.folded
    <> pls ^.. Lens.folded . plEntityId
    <> workArea ^..
        workAreaExpressions . subExprPayloads . Lens.asIndex . body .
        adhocBodyPayloads . plEntityId
    & Set.fromList
    where
        pls = workArea ^.. workAreaExpressions . subExprPayloads

validateHiddenEntityIds :: WorkArea name i o ExprGui.Payload -> Either String ()
validateHiddenEntityIds workArea
    | Set.null hiddenAndExplicit = Right ()
    | otherwise =
        show hiddenAndExplicit ++ " are both hidden and explicit entityIds"
        & Left
    where
        pls = workArea ^.. workAreaExpressions . subExprPayloads
        explicitEntityIds = pls ^.. Lens.folded . plEntityId & Set.fromList
        hiddenEntityIds =
            pls ^.. Lens.folded . plData . plHiddenEntityIds . Lens.folded
            & Set.fromList
        hiddenAndExplicit = Set.intersection explicitEntityIds hiddenEntityIds

data WorkAreaLowLevel = WorkAreaLowLevel
    { wallRepl :: Def.Expr (Val (ValP ViewM))
    , wallPanes :: [Def.Definition (Val (ValP ViewM)) (DefI ViewM)]
    }

workAreaLowLevelValProps :: WorkAreaLowLevel -> [ValP ViewM]
workAreaLowLevelValProps (WorkAreaLowLevel r p) =
    defExprs ^.. Lens.folded . Def.expr . Lens.folded
    where
        defExprs = r : p ^.. Lens.folded . Def.defBody . Def._BodyExpr

workAreaLowLevelEntityIds :: WorkAreaLowLevel -> Set EntityId
workAreaLowLevelEntityIds wall =
    workAreaLowLevelValProps wall
    <&> EntityId.ofValI . Property.value
    & Set.fromList

workAreaLowLevelLoad :: T ViewM WorkAreaLowLevel
workAreaLowLevelLoad =
    WorkAreaLowLevel
    <$> ExprLoad.defExpr (repl codeAnchors)
    <*> (getP (panes codeAnchors) >>= traverse (ExprLoad.def . paneDef))

validate ::
    NFData name =>
    WorkArea name (T fa) (T fb) ExprGui.Payload ->
    T ViewM (WorkArea name (T fa) (T fb) ExprGui.Payload)
validate workArea =
    do
        wallEntityIds <- workAreaLowLevelLoad <&> workAreaLowLevelEntityIds
        let sugarEntityIds = allEntityIds workArea
        let missing = wallEntityIds `Set.difference` sugarEntityIds
        unless (Set.null missing) $ fail $
            show missing ++ " do not appear in any sugar entity ids"
        deepseq workArea -- make sure no "error" clauses are hiding within
            (validateHiddenEntityIds workArea)
            & either fail (\() -> pure workArea)

convertWorkArea ::
    Cache.Functions ->
    T ViewM (WorkArea (Name (T ViewM)) (T ViewM) (T ViewM) ExprGui.Payload)
convertWorkArea cache =
    loadWorkArea cache noopMonitors (pure EvalResults.empty) codeAnchors
    >>= validate

testProgram :: FilePath -> (Cache.Functions -> T ViewM a) -> IO a
testProgram program action =
    do
        cache <- Cache.make <&> snd
        withDB ("test/programs/" <> program)
            (runDbTransaction ?? runAction (action cache))
