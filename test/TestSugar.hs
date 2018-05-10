-- | Test sugar convert results (including its actions)

module TestSugar where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Val as V
import           Lamdu.GUI.CodeEdit.Load (loadWorkArea)
import           Lamdu.Data.Db.Layout (runDbTransaction, codeAnchors, ViewM)
import           Lamdu.Debug (noopMonitors)
import qualified Lamdu.Eval.Results as EvalResults
import           Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.Name (Name)
import           Lamdu.Sugar.Types
import           Lamdu.VersionControl (runAction)
import           Revision.Deltum.Transaction (Transaction)
import           Test.Lamdu.Db (withDB)

import           Test.Lamdu.Prelude

type T = Transaction

test :: Test
test = testGroup "sugar-tests" [testChangeParam, testExtract]

convertWorkArea ::
    T ViewM (WorkArea (Name (T ViewM)) (T ViewM) (T ViewM) ExprGui.Payload)
convertWorkArea =
    loadWorkArea noopMonitors (pure EvalResults.empty) codeAnchors

-- | Verify that a sugar action does not result in a crash
testSugarAction ::
    FilePath ->
    (WorkArea (Name (T ViewM)) (T ViewM) (T ViewM) ExprGui.Payload -> T ViewM a) ->
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

-- | Test for issue #373
-- https://trello.com/c/1kP4By8j/373-re-ordering-let-items-results-in-inference-error
testExtract :: Test
testExtract =
    testSugarAction "let-items-extract.json" action
    & testCase "reorder-lets"
    where
        action =
            (^?! waRepl . replExpr
            . rBody . _BodyLam . lamBinder . bBody
            . bbContent . _BinderLet . lBody
            . bbContent . _BinderLet . lValue
            . bActions . baMNodeActions . Lens._Just . extract
            )
