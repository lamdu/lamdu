-- | Test sugar convert results (including its actions)

module TestSugar where

import           Control.DeepSeq (deepseq)
import qualified Control.Lens as Lens
import qualified Data.List.Class as List
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
test =
    testGroup "sugar-tests"
    [testChangeParam, testReorderLets, testExtract, testInline, delParam]

convertWorkArea ::
    T ViewM (WorkArea (Name (T ViewM)) (T ViewM) (T ViewM) ExprGui.Payload)
convertWorkArea =
    loadWorkArea noopMonitors (pure EvalResults.empty) codeAnchors
    >>= \x -> deepseq x (pure x)

-- | Verify that a sugar action does not result in a crash
testSugarActions ::
    FilePath ->
    [WorkArea (Name (T ViewM)) (T ViewM) (T ViewM) ExprGui.Payload -> T ViewM a] ->
    IO ()
testSugarActions program actions =
    withDB ("test/programs/" <> program)
    (runDbTransaction ?? runAction (void (traverse_ (convertWorkArea >>=) actions >> convertWorkArea)))

-- | Test for issue #374
-- https://trello.com/c/CDLdSlj7/374-changing-tag-results-in-inference-error
testChangeParam :: Test
testChangeParam =
    testSugarActions "apply-id-of-lambda.json" [action]
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
testReorderLets :: Test
testReorderLets =
    testGroup "reorder-lets"
    [ f "let-items-extract.json"
    , f "let-items-extract-with-tag-clash.json"
    , f "let-items-extract-with-anon-tag.json"
    ]
    where
        f program =
            testSugarActions program [(^?! extractSecondLetItemInLambda)]
            & testCase (takeWhile (/= '.') program)
        extractSecondLetItemInLambda =
            waRepl . replExpr .
            rBody . _BodyLam . lamBinder . bBody .
            bbContent . _BinderLet . lBody .
            bbContent . _BinderLet . lValue .
            bActions . baMNodeActions . Lens._Just . extract

-- Test for issue #395
-- https://trello.com/c/UvBdhzzl/395-extract-of-binder-body-with-let-items-may-cause-inference-failure
testExtract :: Test
testExtract =
    testSugarActions "extract-lambda-with-let.json" [(^?! action)]
    & testCase "extract"
    where
        action =
            waRepl . replExpr .
            rBody . _BodyLam . lamBinder . bBody .
            bbContent . _BinderLet . lActions . laNodeActions . extract

-- Test for issue #402
-- https://trello.com/c/ClDnsGQi/402-wrong-result-when-inlining-from-hole-results
testInline :: Test
testInline =
    testSugarActions "let-item-inline.json" [inline, verify]
    & testCase "inline"
    where
        inline workArea =
            do
                Just yOption <-
                    letItem ^. lBody . bbContent . _BinderExpr . rBody . _BodyHole . holeOptions
                    >>= findM isY
                List.Cons (_, mkResult) _ <- yOption ^. hoResults & List.runList
                result <- mkResult
                result ^. holeResultPick
                _ <- result ^?! holeResultConverted . rBody . _BodyGetVar . _GetBinder . bvInline . _InlineVar
                pure ()
            where
                letItem =
                    workArea ^?! waRepl . replExpr .
                    rBody . _BodyLam . lamBinder . bBody .
                    bbContent . _BinderLet
                isY option =
                    option ^. hoSugaredBaseExpr
                    <&> Lens.has (rBody . _BodyGetVar . _GetBinder . bvForm . _GetLet)
        verify workArea
            | Lens.has afterInline workArea = pure ()
            | otherwise = fail "Expected inline result"
        afterInline =
            waRepl . replExpr .
            rBody . _BodyLam . lamBinder . bBody .
            bbContent . _BinderExpr .
            rBody . _BodyLiteral . _LiteralNum

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM f (x:xs) =
    do
        found <- f x
        if found
            then Just x & pure
            else findM f xs

delParam :: Test
delParam =
    testSugarActions "const-five.json" [(^?! action), verify]
    & testCase "del-param"
    where
        action =
            waRepl . replExpr .
            rBody . _BodyLam . lamBinder . bParams . _Params . Lens.ix 0 .
            fpInfo . piActions . fpDelete
        verify workArea
            | Lens.has afterDel workArea = pure ()
            | otherwise = fail "Expected 5"
        afterDel = waRepl . replExpr . rBody . _BodyLiteral . _LiteralNum