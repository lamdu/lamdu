module Test.Lamdu.Sugar where

import           Control.DeepSeq (NFData, deepseq)
import qualified Control.Lens as Lens
import qualified Data.Set as Set
import qualified Lamdu.Cache as Cache
import           Lamdu.Data.Db.Layout (ViewM, codeAnchors, runDbTransaction)
import           Lamdu.Debug (noopMonitors)
import qualified Lamdu.Eval.Results as EvalResults
import           Lamdu.GUI.CodeEdit.Load (loadWorkArea)
import           Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.Name (Name)
import           Lamdu.Sugar.Lens (workAreaExpressions, subExprPayloads)
import           Lamdu.Sugar.Types
import           Lamdu.VersionControl (runAction)
import           Revision.Deltum.Transaction (Transaction)
import           Test.Lamdu.Db (withDB)

import           Test.Lamdu.Prelude

type T = Transaction

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

validate ::
    (Monad m, NFData name) =>
    WorkArea name (T fa) (T fb) ExprGui.Payload ->
    m (WorkArea name (T fa) (T fb) ExprGui.Payload)
validate workArea =
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
