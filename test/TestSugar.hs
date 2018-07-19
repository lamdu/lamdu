-- | Test sugar convert results (including its actions)

module TestSugar where

import qualified Control.Lens as Lens
import qualified Data.List.Class as List
import qualified Lamdu.Calc.Val as V
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.Name (Name)
import           Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)
import           Test.Lamdu.Sugar (convertWorkArea, testProgram)

import           Test.Lamdu.Prelude

type T = Transaction

test :: Test
test =
    testGroup "sugar-tests"
    [ delDefParam
    , delParam
    , paramAnnotations
    , testChangeParam
    , testExtract
    , testLightLambda
    , testInline
    , testReorderLets
    ]

-- | Verify that a sugar action does not result in a crash
testSugarActions ::
    HasCallStack =>
    FilePath ->
    [WorkArea (Name (T ViewM)) (T ViewM) (T ViewM)
        (Sugar.Payload (Name (T ViewM)) (T ViewM) (T ViewM) ExprGui.Payload) ->
        T ViewM a] ->
    IO ()
testSugarActions program actions =
    testProgram program $ \cache ->
    traverse_ (convertWorkArea cache >>=) actions <* convertWorkArea cache

replBody :: Lens.Traversal' (WorkArea name i o a) (Body name i o (Ann a))
replBody = waRepl . replExpr . _Node . val . _BinderExpr

lamFirstParam :: Lens.Traversal' (Body name i o a) (FuncParam name i (ParamInfo name i o))
lamFirstParam = _BodyLam . lamFunc . fParams . _Params . Lens.ix 0

-- | Test for issue #374
-- https://trello.com/c/CDLdSlj7/374-changing-tag-results-in-inference-error
testChangeParam :: Test
testChangeParam =
    testSugarActions "apply-id-of-lambda.json" [action]
    & testCase "change-param"
    where
        action workArea =
            "new" &
            workArea ^?!
            replBody . _BodySimpleApply . V.applyFunc .
            _Node . val . _BodySimpleApply . V.applyArg .
            _Node . val . lamFirstParam . fpInfo . piTag . tagSelection . tsNewTag

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
            replBody . _BodyLam . lamFunc . fBody .
            _Node . val . _BinderLet . lBody .
            _Node . val . _BinderLet . lValue .
            _Node . ann . plActions . extract

-- Test for issue #395
-- https://trello.com/c/UvBdhzzl/395-extract-of-binder-body-with-let-items-may-cause-inference-failure
testExtract :: Test
testExtract =
    testSugarActions "extract-lambda-with-let.json" [(^?! action)]
    & testCase "extract"
    where
        action =
            replBody . _BodyLam . lamFunc . fBody . _Node . ann . plActions .
            extract

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
                    letItem ^. lBody . _Node . val . _BinderExpr . _BodyHole
                    . holeOptions
                    >>= findM isY
                List.Cons (_, mkResult) _ <- yOption ^. hoResults & List.runList
                result <- mkResult
                result ^. holeResultPick
                _ <-
                    result ^?! holeResultConverted . _Node . val . _BinderExpr
                    . _BodyGetVar . _GetBinder . bvInline . _InlineVar
                pure ()
            where
                letItem =
                    workArea ^?!
                    replBody . _BodyLam . lamFunc . fBody .
                    _Node . val . _BinderLet
                isY option =
                    option ^. hoSugaredBaseExpr
                    <&> Lens.has
                    (_Node . val . _BinderExpr . _BodyGetVar . _GetBinder .
                        bvForm . _GetLet)
        verify workArea
            | Lens.has afterInline workArea = pure ()
            | otherwise = fail "Expected inline result"
        afterInline =
            replBody . _BodyLam . lamFunc . fBody .
            _Node . val . _BinderExpr . _BodyLiteral . _LiteralNum

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = pure Nothing
findM f (x:xs) =
    do
        found <- f x
        if found
            then Just x & pure
            else findM f xs

paramAnnotations :: Test
paramAnnotations =
    testSugarActions "const-five.json" [verify]
    & testCase "param-annotations"
    where
        verify workArea =
            unless
            (Lens.allOf (replBody . lamFirstParam . fpAnnotation) (Lens.has _AnnotationNone) workArea)
            (fail "parameter should not have type annotation")

delParam :: Test
delParam =
    testSugarActions "const-five.json" [(^?! action), verify]
    & testCase "del-param"
    where
        action = replBody . lamFirstParam . fpInfo . piActions . fpDelete
        verify workArea
            | Lens.has afterDel workArea = pure ()
            | otherwise = fail "Expected 5"
        afterDel = replBody . _BodyLiteral . _LiteralNum

testLightLambda :: Test
testLightLambda =
    testSugarActions "fold.json" [verify]
    & testCase "light-lambda"
    where
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = fail "Expected light lambda sugar!"
        expected =
            replBody . _BodyLabeledApply . aAnnotatedArgs . traverse . aaExpr .
            _Node . val . _BodyLam . lamMode . _LightLambda

delDefParam :: Test
delDefParam =
    testSugarActions "def-with-params.json" [void . (^?! openDef), (^?! action)]
    & testCase "del-def-param"
    where
        openDef = replBody . _BodyGetVar . _GetBinder . bvNameRef . nrGotoDefinition
        action =
            waPanes . traverse . paneDefinition .
            drBody . _DefinitionBodyExpression . deContent .
            _Node . val . _BodyFunction .
            fParams . _Params . traverse .
            fpInfo . piActions . fpDelete
