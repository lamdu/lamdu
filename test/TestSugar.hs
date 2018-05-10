-- | Test sugar convert results (including its actions)

module TestSugar where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Val as V
import           Lamdu.Data.Db.Layout (runDbTransaction, codeAnchors, ViewM)
import           Lamdu.Debug (noopMonitors)
import qualified Lamdu.Eval.Results as EvalResults
import qualified Lamdu.Sugar.Convert as Convert
import           Lamdu.Sugar.Types
import           Lamdu.VersionControl (runAction)
import           Revision.Deltum.Transaction (Transaction)
import           Test.Lamdu.Db (withDB)

import           Test.Lamdu.Prelude

type T = Transaction

test :: Test
test = testGroup "sugar-tests" [testChangeParam]

convertWorkArea ::
    T ViewM (WorkArea Convert.InternalName (T ViewM) (T ViewM) [EntityId])
convertWorkArea =
    Convert.loadWorkArea noopMonitors (pure EvalResults.empty) codeAnchors

-- | Verify that a sugar action does not result in a crash
testSugarAction ::
    FilePath ->
    (WorkArea Convert.InternalName (T ViewM) (T ViewM) [EntityId] -> T ViewM a) ->
    IO ()
testSugarAction program action =
    withDB ("test/programs/" <> program)
    (runDbTransaction ?? runAction (void (convertWorkArea >>= action >> convertWorkArea)))

-- | Test for issue #374
-- https://trello.com/c/CDLdSlj7/374-changing-tag-results-in-inference-error
testChangeParam :: Test
testChangeParam =
    testSugarAction "apply-id-of-lambda.json" action
    & testCase "change-param"
    where
        action workArea =
            "new" &
            workArea ^?! waRepl . replExpr .
            rBody . _BodySimpleApply . V.applyFunc .
            rBody . _BodySimpleApply . V.applyArg .
            rBody . _BodyLam . lamBinder . bParams . _Params . Lens.ix 0 .
            fpInfo . piTag . tagSelection . tsNewTag
