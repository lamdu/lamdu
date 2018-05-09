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
import           Test.Framework
import           Test.Framework.Providers.HUnit (testCase)
import           Test.Lamdu.Db (withDB)

import           Lamdu.Prelude

type T = Transaction

test :: Test
test = testGroup "sugar-tests" [testChangeParam]

convertWorkArea ::
    T ViewM (WorkArea Convert.InternalName (T ViewM) (T ViewM) [EntityId])
convertWorkArea =
    Convert.loadWorkArea noopMonitors (pure EvalResults.empty) codeAnchors

-- | Test for issue #374
-- https://trello.com/c/CDLdSlj7/374-changing-tag-results-in-inference-error
testChangeParam :: Test
testChangeParam =
    testCase "change-param" $
    withDB "test/programs/apply-id-of-lambda.json" $
    \db ->
    runDbTransaction db $ runAction $
    do
        sugarExpr <- convertWorkArea
        let replaceParam =
                sugarExpr ^?! waRepl . replExpr .
                rBody . _BodySimpleApply . V.applyFunc .
                rBody . _BodySimpleApply . V.applyArg .
                rBody . _BodyLam . lamBinder . bParams . _Params . Lens.ix 0 .
                fpInfo . piTag . tagSelection . tsNewTag
        _ <- replaceParam "new"
        _ <- convertWorkArea
        pure ()
