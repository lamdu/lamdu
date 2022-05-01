-- | Test sugar convert results (including its actions)

module Tests.Sugar (test) where

import qualified Control.Lens as Lens
import           Control.Monad ((>=>))
import           Control.Monad.Once (OnceT)
import qualified Data.Property as Property
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Calc.Term as V
import           Lamdu.Data.Db.Layout (ViewM)
import           Lamdu.Name
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Lens.Annotations as SugarLens
import           Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)
import           Test.Lamdu.Env (Env)
import qualified Test.Lamdu.Env as Env
import           Test.Lamdu.Sugar (convertWorkArea, testProgram)

import           Test.Lamdu.Prelude

type T = Transaction

test :: Test
test =
    testGroup "sugar-tests"
    [ delDefParam
    , updateDef
    , delParam
    , delInfixArg
    , delLet
    , paramAnnotations
    , testChangeParam
    , testExtract
    , testExtractForRecursion
    , testLightLambda
    , testNotALightLambda
    , testInline
    , testCannotInline
    , testReorderLets
    , testReplaceParent
    , testReplaceParentFragment
    , setHoleToHole
    , testCreateLetInLetVal
    , testFloatToRepl
    , floatLetWithGlobalRef
    , lightConst
    , addParamToLet
    , testHoleTypeShown
    , testUnnamed
    , testPunnedIso
    , testNullParamUnused
    , testPunnedLightParam
    , testParamsOrder
    , testAddToInferredParamList
    , testInfixWithArgParens
    , testNoParenForPrefixInInfix
    , testDisambig
    , testSuspendedHoleResultSimple
    , testSuspendedHoleResult
    , testNoInjectValAnn
    , testCaseNav
    , testSkolemScope
    , testAddRecursiveFuncParam
    , testGroup "insist-tests"
        [ testInsistFactorial
        , testInsistEq
        , testInsistIf
        , testInsistSubsets
        ]
    ]

type TestAnnotation = Annotation (EvaluationScopes Name (OnceT (T ViewM))) Name
type TestWorkArea =
    WorkArea TestAnnotation Name (OnceT (T ViewM)) (T ViewM)
    (Sugar.Payload TestAnnotation (T ViewM))

testSugarActionsWith :: HasCallStack => FilePath -> [TestWorkArea -> OnceT (T ViewM) a] -> Env -> IO ()
testSugarActionsWith program actions env =
    traverse_ (convertWorkArea "" env >>=) actions <* convertWorkArea "" env
    & testProgram program

-- | Verify that a sugar action does not result in a crash
testSugarActions :: HasCallStack => FilePath -> [TestWorkArea -> OnceT (T ViewM) a] -> IO ()
testSugarActions program actions =
    Env.make >>= testSugarActionsWith program actions

replBinder ::
    Lens.Traversal' (WorkArea v name i o a)
    ( BinderBody v name i o #
        Annotated a
    )
replBinder = waRepl . replExpr . hVal . bBody

replBody ::
    Lens.Traversal' (WorkArea v name i o a)
    (Term v name i o # Annotated a)
replBody = replBinder . _BinderTerm

replLet :: Lens.Traversal' (WorkArea v name i o a) (Let v name i o # Annotated a)
replLet = replBinder . _BinderLet

testNoInjectValAnn :: Test
testNoInjectValAnn =
    testSugarActions "inject.json" [verify]
    & testCase "no-inject-val-ann"
    where
        verify workArea
            | Lens.has (waRepl . replExpr . annotation . plAnnotation . _AnnotationVal) workArea =
                error "redundant value annotation"
            | otherwise = pure ()

testCaseNav :: Test
testCaseNav =
    testSugarActions "lambda-case.json" [verify]
    & testCase "case-scope-nav"
    where
        verify workArea
            | Lens.has (lamApplyLimit . _UnlimitedFuncApply) lam = pure ()
            | otherwise = error "expected case lambda to be UnlimitedFuncApply"
            where
                lam =
                    workArea ^? replBody . _BodyPostfixFunc . _PfCase . cList . SugarLens.taggedListItems . tiValue .
                    hVal . _BodyLam & fromMaybe (error "Expected a case lambda")

testUnnamed :: Test
testUnnamed =
    testSugarActions "unnamed.json" [verify]
    & testCase "name-of-unnamed"
    where
        verify workArea =
            case workArea ^?! replBody . _BodyLeaf . _LeafGetVar . vNameRef . nrName of
            Unnamed{} -> pure ()
            _ -> error "Unexpected name"

-- | Test for issue #374
-- https://trello.com/c/CDLdSlj7/374-changing-tag-results-in-inference-error
testChangeParam :: Test
testChangeParam =
    testSugarActions "apply-id-of-lambda.json" [action]
    & testCase "change-param"
    where
        action workArea =
            workArea ^?!
            replBody . _BodySimpleApply . V.appFunc .
            hVal . _BodySimpleApply . V.appArg .
            hVal . _BodyLam . lamFunc . fParams . _LhsRecord .
            tlItems . Lens._Just . tlHead . tiTag . tagRefReplace
            >>= lift . (^. tcNewTag . toPick)

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
            testSugarActions program [lift . (^?! extractSecondLetItemInLambda)]
            & testCase (takeWhile (/= '.') program)
        extractSecondLetItemInLambda =
            replBody . _BodyLam . lamFunc . fBody .
            hVal . bBody . _BinderLet . lBody .
            hVal . bBody . _BinderLet . lValue .
            annotation . plActions . extract

-- Test for issue #395
-- https://trello.com/c/UvBdhzzl/395-extract-of-binder-body-with-let-items-may-cause-inference-failure
testExtract :: Test
testExtract =
    testSugarActions "extract-lambda-with-let.json" [lift . (^?! action)]
    & testCase "extract"
    where
        action =
            replBody . _BodyLam . lamFunc . fBody . annotation . plActions .
            extract

addParamToLet :: Test
addParamToLet =
    testSugarActions "let-used-twice.json" [lift . (^?! action)]
    & testCase "add-param-to-let"
    where
        action =
            replLet . lValue . hVal . _BodyPlain . apAddFirstParam

testCannotInline :: Test
testCannotInline =
    testSugarActions "let-used-twice.json" [verify]
    & testCase "cannot-inline"
    where
        verify workArea =
            case workArea ^? pos of
            Nothing -> error "expected a getvar!"
            Just x ->
                case x ^. vInline of
                InlineVar{} -> error "shouldn't be able to inline!"
                _ -> pure ()
            where
                pos =
                    replLet . lBody
                    . hVal . bBody . _BinderTerm . _BodyLabeledApply . aMOpArgs . Lens._Just . oaLhs
                    . hVal . _BodyLeaf . _LeafGetVar

-- Test for issue #402
-- https://trello.com/c/ClDnsGQi/402-wrong-result-when-inlining-from-hole-results
testInline :: Test
testInline =
    do
        queryLangInfo <- Env.make <&> hasQueryLangInfo
        let inline workArea =
                do
                    result <-
                        workArea ^?!
                        replBody . _BodyLam . lamFunc . fBody .
                        hVal . bBody . _BinderLet . lBody . hVal . bBody . _BinderTerm . _BodyLeaf . _LeafHole
                        . holeOptions
                        >>= (Query queryLangInfo mempty "num" &)
                        <&> fromMaybe (error "expected option") . (^? traverse)
                    result ^. optionPick & lift
                    result ^?! optionExpr . hVal . _HoleBinder . bBody . _BinderTerm
                        . _BodyLeaf . _LeafGetVar . vInline . _InlineVar
                        & lift & void
        testSugarActions "let-item-inline.json" [inline, verify]
    & testCase "inline"
    where
        verify workArea
            | Lens.has afterInline workArea = pure ()
            | otherwise = error "Expected inline result"
        afterInline =
            replBody . _BodyLam . lamFunc . fBody .
            hVal . bBody . _BinderTerm . _BodyLeaf . _LeafLiteral . _LiteralNum

testSkolemScope :: Test
testSkolemScope =
    do
        queryLangInfo <- Env.make <&> hasQueryLangInfo
        let res workArea =
                    workArea ^?!
                    replBody . _BodyToNom . nVal .
                    hVal . bBody . _BinderTerm . _BodyLabeledApply . aMOpArgs . Lens._Just . oaRhs .
                    hVal . _BodyLam . lamFunc . fBody .
                    hVal . bBody . _BinderTerm . _BodyLeaf . _LeafHole . holeOptions
                    >>= (Query queryLangInfo mempty "cont" &)
                    >>= lift . (^. optionPick) . fromMaybe (error "expected option") . (^? traverse)
        testSugarActions "skolem.json" [res]
    & testCase "skolem-scope"

lightConst :: Test
lightConst =
    testSugarActions "const-five.json" [verify]
    & testCase "param-annotations"
    where
        verify workArea
            | Lens.has (replBody . _BodyLam . lamLightweight . Lens.only True) workArea = pure ()
            | otherwise = error "Expected light lambda"

paramAnnotations :: Test
paramAnnotations =
    Env.make <&> has .~ Annotations.None
    >>= testSugarActionsWith "const-five.json" [verify]
    & testCase "param-annotations"
    where
        verify workArea =
            unless
            (Lens.allOf
                (replBody . _BodyLam . lamFunc . fParams . _LhsVar . vParam . fpAnnotation)
                (Lens.has _AnnotationNone) workArea)
            (error "parameter should not have type annotation")

delParam :: Test
delParam =
    testSugarActions "const-five.json" [lift . (^?! action), verify]
    & testCase "del-param"
    where
        action = replBody . _BodyLam . lamFunc . fParams . _LhsVar . vDelete
        verify workArea
            | Lens.has afterDel workArea = pure ()
            | otherwise = error "Expected 5"
        afterDel = replBody . _BodyLeaf . _LeafLiteral . _LiteralNum

delInfixArg :: Test
delInfixArg =
    testSugarActions "one-plus-one.json" [argDel, holeDel, verify]
    & testCase "del-infix-arg"
    where
        argDel workArea =
            workArea ^?! arg . annotation . plActions . delete . _SetToHole
            & void & lift
        holeDel workArea =
            workArea ^?! arg . annotation . plActions . delete . _Delete
            & void & lift
        arg = replBody . _BodyLabeledApply . aMOpArgs . Lens._Just . oaRhs
        verify workArea
            | Lens.has afterDel workArea = pure ()
            | otherwise = error "Expected 1"
        afterDel = replBody . _BodyLeaf . _LeafLiteral . _LiteralNum

delLet :: Test
delLet =
    testSugarActions "simple-let.json" [lift . void . (^?! letDel), verify]
    & testCase "del-let"
    where
        letDel = replBinder . _BinderLet . lValue . annotation . plActions . delete . _Delete
        verify workArea
            | Lens.has (replBody . _BodyLeaf . _LeafLiteral . _LiteralNum) workArea = pure ()
            | otherwise = error "Expected 1"

testExtractForRecursion :: Test
testExtractForRecursion =
    testSugarActions "fold.json"
    [ lift . void . (^?! openDef)
    , lift . void . (^?! extractDef)
    ]
    & testCase "no-extract-recursive"
    where
        openDef =
            replBody . _BodyLabeledApply . aFunc .
            hVal . Lens._Wrapped . vNameRef . nrGotoDefinition
        extractDef =
            waPanes . traverse . SugarLens.paneBinder .
            annotation . plActions . extract

testInsistFactorial :: Test
testInsistFactorial =
    testSugarActions "factorial-mismatch.json"
    [ lift . void . (^?! openDef)
    , lift . void . (^?! insist)
    , verify
    ]
    & testCase "insist-factorial"
    where
        openDef =
            replBody . _BodySimpleApply . appFunc .
            hVal . _BodyLeaf . _LeafGetVar . vNameRef . nrGotoDefinition
        ifElse =
            waPanes . traverse . SugarLens.paneBinder .
            hVal . _BodyFunction . fBody .
            hVal . bBody . _BinderTerm . _BodyIfElse
        insist =
            Lens.cloneTraversal ifElse . iThen .
            hVal . _BodyLam . lamFunc . fBody .
            hVal . bBody . _BinderTerm . _BodyFragment . fHeal
        verify workArea
            | Lens.has unexpected workArea = error "fragment created at unexpected position"
            | otherwise = pure ()
        unexpected =
            Lens.cloneTraversal ifElse . iElse .
            hVal . _SimpleElse . _BodyLam . lamFunc . fBody .
            hVal . bBody . _BinderTerm . _BodySimpleApply . appFunc .
            hVal . _BodyFragment

testInsistEq :: Test
testInsistEq =
    testSugarActions "compare-int-and-text.json"
    [ lift . void . (^?! insist)
    , verify
    ]
    & testCase "insist-eq"
    where
        insist =
            replBody . _BodyLabeledApply . aMOpArgs . Lens._Just . oaRhs .
            hVal . _BodyFragment . fHeal
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = error "fragment not created at expected position"
        expected =
            replBody . _BodyLabeledApply . aMOpArgs . Lens._Just . oaLhs .
            hVal . _BodyFragment

testInsistIf :: Test
testInsistIf =
    testSugarActions "if-with-mismatch.json"
    [ lift . void . (^?! insist)
    , verify
    ]
    & testCase "insist-if"
    where
        insist =
            replBody . _BodyIfElse . iThen .
            hVal . _BodyLam . lamFunc . fBody .
            hVal . bBody . _BinderTerm . _BodyFragment . fHeal
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = error "fragment not created at expected position"
        expected =
            replBody . _BodyIfElse . iElse .
            hVal . _SimpleElse . _BodyLam . lamFunc . fBody .
            hVal . bBody . _BinderTerm . _BodyFragment

testInsistSubsets :: Test
testInsistSubsets =
    testSugarActions "subsets.json"
    [ openTopLevelDef
    , lift . void . (^?! insist)
    , verify
    ]
    & testCase "insist-subsets"
    where
        consArgs =
            waPanes . traverse . SugarLens.paneBinder .
            hVal . _BodyFunction . fBody .
            hVal . bBody . _BinderTerm . _BodyPostfixApply . pFunc .
            hVal . _PfCase . cList . tlItems . Lens._Just . tlTail . Lens.ix 0 . tsiItem . tiValue .
            hVal . _BodyLam . lamFunc . fBody .
            hVal . bBody . _BinderTerm . _BodyLabeledApply . aMOpArgs . Lens._Just
        insist =
            Lens.cloneTraversal consArgs . oaRhs .
            hVal . _BodyLam . lamFunc . fBody .
            hVal . bBody . _BinderLet . lBody .
            hVal . bBody . _BinderTerm . _BodyLabeledApply . aMOpArgs . Lens._Just . oaRhs .
            hVal . _BodyLam . lamFunc . fBody .
            hVal . bBody . _BinderTerm . _BodyLabeledApply . aMOpArgs . Lens._Just . oaLhs .
            hVal . _BodyFragment . fHeal
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = error "fragment not created at expected position"
        expected = Lens.cloneTraversal consArgs . oaLhs . hVal . _BodyFragment

testLightLambda :: Test
testLightLambda =
    testSugarActions "fold.json" [verify]
    & testCase "light-lambda"
    where
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = error "Expected light lambda sugar!"
        expected =
            replBody . _BodyLabeledApply . aAnnotatedArgs . traverse . aaExpr .
            hVal . _BodyLam . lamLightweight . Lens.only True

testNotALightLambda :: Test
testNotALightLambda =
    testSugarActions "lam-in-lam.json" [verify]
    & testCase "not-a-light-lambda"
    where
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = error "Expected light lambda sugar!"
        expected = replBody . _BodyLam . lamLightweight . Lens.only False

openTopLevelDef :: WorkArea v name i (T ViewM) a -> OnceT (T ViewM) ()
openTopLevelDef =
    lift . void . (^?! replBody . _BodyLeaf . _LeafGetVar . vNameRef . nrGotoDefinition)

delDefParam :: Test
delDefParam =
    testSugarActions "def-with-params.json"
    [openTopLevelDef, lift . void . (^?! action)]
    & testCase "del-def-param"
    where
        action =
            waPanes . traverse . SugarLens.paneBinder .
            hVal . _BodyFunction .
            fParams . _LhsRecord . tlItems . Lens._Just . tlHead . tiDelete

updateDef :: Test
updateDef =
    testSugarActions "update-def-type.json"
    [openTopLevelDef, lift . void . (^?! action)]
    & testCase "update-def-type"
    where
        action =
            waPanes . traverse . SugarLens.paneBinder .
            hVal . _BodyFunction . fBody .
            hVal . bBody . _BinderTerm . _BodyLabeledApply . aFunc .
            hVal . Lens._Wrapped . vForm . _GetDefinition . _DefTypeChanged . defTypeUseCurrent

testReplaceParent :: Test
testReplaceParent =
    testSugarActions "let-item-inline.json" [lift . (^?! action)]
    & testCase "replace-parent"
    where
        action =
            replBody . _BodyLam . lamFunc . fBody .
            annotation . plActions . mReplaceParent . Lens._Just

testReplaceParentFragment :: Test
testReplaceParentFragment =
    testSugarActions "multiply-list.json" [void . lift . (^?! action), verify]
    & testCase "replace-parent-fragment"
    where
        action =
            Lens.cloneTraversal fragExpr .
            hVal . _BodySimpleApply . appArg .
            annotation . plActions . mReplaceParent . Lens._Just
        fragExpr = replBody . _BodyLabeledApply . aMOpArgs . Lens._Just . oaLhs . hVal . _BodyFragment . fExpr
        verify workArea
            | Lens.has (Lens.cloneTraversal fragExpr) workArea =
                error "replace-parent did not remove fragment"
            | otherwise = pure ()

floatLetWithGlobalRef :: Test
floatLetWithGlobalRef =
    testSugarActions "let-with-global-reference.json"
    [ lift . (^?! replLet . lBody . hVal . bBody . _BinderLet . lValue . annotation . plActions . extract)
    ]
    & testCase "float-let-with-global-ref"

setHoleToHole :: Test
setHoleToHole =
    testSugarActions "let-item-inline.json" [action, verify]
    & testCase "set-hole-to-hole"
    where
        action workArea = workArea ^?! setToHole & void & lift
        verify workArea
            | Lens.has setToHole workArea =
                error "hole has set to hole?"
            | otherwise = pure ()
        setToHole :: Lens.Traversal' (WorkArea v name i o (Payload v o)) (o EntityId)
        setToHole =
            replBody . _BodyLam . lamFunc . fBody .
            hVal . bBody . _BinderLet . lValue .
            annotation . plActions . delete . _SetToHole

assertEq :: (Monad m, Show a, Eq a) => String -> a -> a -> m ()
assertEq msg expected got
    | expected == got = pure ()
    | otherwise =
          "Assertion failed: " ++ msg ++
          "\n  expected to be: " ++ show expected ++
          "\n  but was:        " ++ show got
          & error

testFloatToRepl :: Test
testFloatToRepl =
    testCase "float-to-repl" $
    do
        env <- Env.make
        testProgram "repl-2-lets.json" $
            do
                workArea <- convertWorkArea "" env
                assertLetVals workArea 1 2
                _ <- workArea ^?! innerLet . annotation . plActions . extract & lift
                newWorkArea <- convertWorkArea "" env
                assertLetVals newWorkArea 2 1
    where
        assertLetVals workArea outer inner =
            do
                assertEq "Outer let hVal" outer
                    (workArea ^?! replLet . lValue . literalVal)
                assertEq "Inner let hVal" inner
                    (workArea ^?! innerLet . literalVal)

        innerLet ::
            Lens.Traversal' (WorkArea v name i o a) (Annotated a # Assignment v name i o)
        innerLet = replLet . lBody . hVal . bBody . _BinderLet . lValue
        literalVal =
            hVal . _BodyPlain . apBody . bBody . _BinderTerm .
            _BodyLeaf . _LeafLiteral . _LiteralNum . Property.pVal

testCreateLetInLetVal :: Test
testCreateLetInLetVal =
    testCase "create-let-in-let-val" $
    do
        env <- Env.make
        result <-
            testProgram "let-item-inline.json" $
                do
                    _ <- convertWorkArea "" env >>= lift . (^?! theLetVal . bAddOuterLet)
                    convertWorkArea "" env
            <&> Lens.has (theLetVal . bBody . _BinderLet)
        assertBool "Let was not created inside the let-value" result
    where
        -- | Extract from:
        -- >>> \x -> let y = 0 in <hole>
        --           ^^^^^^^^^^^^^^^^^^^
        theLetVal ::
            Lens.Traversal'
            (WorkArea v name i o a)
            (Binder v name i o # Annotated a)
        theLetVal =
            replBody . _BodyLam . lamFunc . fBody .
            hVal . bBody . _BinderLet . lValue . hVal . _BodyPlain . apBody

testHoleTypeShown :: Test
testHoleTypeShown =
    testCase "hole-type-shown" $
    do
        env <- Env.make <&> has .~ Annotations.None
        workArea <- testProgram "to-nom.json" (convertWorkArea "" env)
        let x = workArea ^?! replBody . _BodyToNom . nVal
        putStrLn $ case x ^. annotation . plAnnotation of
            AnnotationType {} -> "Type"
            AnnotationVal {} -> "Val"
            AnnotationNone {} -> "None"
        Lens.has (annotation . plAnnotation . _AnnotationType) x
            & assertBool "Expected to have type"

-- Test for https://trello.com/c/Dzp5vgos/510-not-punning-auto-named-variables
testPunnedIso :: Test
testPunnedIso =
    testCase "punned-iso" $
    Env.make >>= testProgram "punned-fields.json" . convertWorkArea ""
    <&> (^.. replBinder . _BinderLet . lBody . hVal . bBody . _BinderLet . lBody .
            hVal . bBody . _BinderTerm . _BodyRecord . cList . SugarLens.taggedListItems)
    <&> Lens.mapped %~
        (\x -> (x ^. tiTag . tagRefTag . tagName, x ^? tiValue . hVal . _BodyLeaf . _LeafGetVar . vNameRef . nrName))
    >>= assertEqual "Record items expected to be punned" []

testNullParamUnused :: Test
testNullParamUnused =
    testCase "null-param-unused" $
    Env.make >>= testProgram "null-param-cond.json" . convertWorkArea ""
    <&> Lens.has
        ( replBinder . _BinderLet . lValue . hVal . _BodyFunction
        . fParams . _LhsVar . vIsNullParam . Lens.only False)
    >>= assertBool "Null param only if unused"

-- Test for https://github.com/lamdu/lamdu/issues/123
testPunnedLightParam :: Test
testPunnedLightParam =
    testCase "punned-light-param" $
    Env.make >>= testProgram "punned-light-param.json" . convertWorkArea ""
    <&> Lens.has
        ( replBinder . _BinderTerm . _BodyLam . lamFunc . fBody . hVal .
            bBody . _BinderTerm . _BodyRecord . cPunnedItems . traverse . pvVar . hVal .
            Lens._Wrapped . vForm . _GetLightParam
        )
    >>= assertBool "Null param only if unused"

-- Test for https://github.com/lamdu/lamdu/issues/124
testParamsOrder :: Test
testParamsOrder =
    testCase "params-order" $
    do
        env <- Env.make
        let reorder msg =
                do
                    mOrderBefore <-
                        convertWorkArea msg env
                        <&> (^? funcParams . tlTail . traverse . tsiSwapWithPrevious)
                    case mOrderBefore of
                        Just a -> lift a
                        Nothing -> error ("cant reorder before " <> msg)
        let readTags =
                convertWorkArea "" env <&> (^.. funcParams . SugarLens.taggedListBodyItems . tiTag . tagRefTag . tagVal)
        testProgram "func-params.json" $
            do
                params0 <- readTags
                reorder "at beginning"
                params1 <- readTags
                when (params0 == params1) (error ("params didn't change: " <> show params0))
                reorder "after reorder"
                params2 <- readTags
                assertEq "params should be same" params0 params2
    where
        funcParams :: Lens.Traversal' (WorkArea v n i o a) (TaggedListBody n i o (LhsField n v))
        funcParams =
            replBinder . _BinderLet . lValue .
            hVal . _BodyFunction . fParams . _LhsRecord . tlItems . Lens._Just

testAddToInferredParamList :: Test
testAddToInferredParamList =
    testCase "add-to-inferred-param-list" $
    do
        env <- Env.make
        workArea <-
            testProgram "func-params.json" $
            do
                convertWorkArea "" env
                    >>= (^?! elseClause . lamBodyParams . tiAddAfter)
                    >>= lift . (^. tcNewTag . toPick)
                convertWorkArea "" env
        let paramList = workArea ^.. elseClause . _BodyFragment . fExpr . hVal . lamBodyParams
        assertEqual "Parameter list length" (length paramList) 3
    where
        elseClause :: Lens.Traversal' (WorkArea v n i o a) (Term v n i o # Annotated a)
        elseClause =
            replBinder . _BinderLet . lBody .
            hVal . bBody . _BinderTerm . _BodyIfElse . iElse .
            hVal . _SimpleElse . _BodyLam . lamFunc . fBody .
            hVal . bBody . _BinderTerm
        lamBodyParams :: Lens.Traversal' (Term v n i o # k) (TaggedItem n i o (LhsField n v))
        lamBodyParams = _BodyLam . lamFunc . fParams . _LhsRecord . SugarLens.taggedListItems

testNoParenForPrefixInInfix :: Test
testNoParenForPrefixInInfix =
    testSugarActions "from-array-map.json" [verify]
    & testCase "perfix-in-infix-no-parens"
    where
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = error "Expected no parens"
        expected =
            replBody . _BodyLabeledApply . aMOpArgs . Lens._Just . oaLhs .
            annotation . plParenInfo . piNeedParens . Lens.only False

testInfixWithArgParens :: Test
testInfixWithArgParens =
    testCase "infix-with-arg-parens" $
    Env.make >>= testProgram "infix-with-args-needs-paren.json" . convertWorkArea ""
    <&> (^?! replBinder . _BinderTerm . _BodySimpleApply . appArg . annotation . plParenInfo . piNeedParens)
    >>= assertBool "Expected paren"

testDisambig :: Test
testDisambig =
    testCase "disambig-operator" $
    Env.make >>= testProgram "disambig.json" . convertWorkArea ""
    <&> Lens.has itemOp
    >>= assertBool "Expect collsion"
    where
        itemOp =
            replBinder . _BinderTerm . _BodyLabeledApply . aAnnotatedArgs . traverse . aaExpr .
            hVal . _BodyLabeledApply . aFunc .
            hVal . Lens._Wrapped . vNameRef . nrName . _NameTag . tnTagCollision . _Collision

testAddRecursiveFuncParam :: Test
testAddRecursiveFuncParam =
    testSugarActions "recursive-func.json"
    [ lift . void . (^?! openDef)
    , (^?! addParam) >=> lift . (^. tcNewTag . toPick)
    , verify
    ]
    & testCase "add-recursive-func-param"
    where
        openDef =
            replBody . _BodySimpleApply . appFunc .
            hVal . _BodyLeaf . _LeafGetVar . vNameRef . nrGotoDefinition
        func = waPanes . traverse . SugarLens.paneBinder . hVal . _BodyFunction
        addParam = Lens.cloneTraversal func . fParams . _LhsVar . vAddNext . _AddNext
        verify workArea
            | Lens.has (Lens.cloneTraversal func) workArea = pure ()
            | otherwise = error "Adding parameter created fragment"

getHoleResults ::
    FilePath ->
    Lens.ATraversal'
    TestWorkArea
    (Term v0 name (OnceT (T ViewM)) o k0) ->
    IO [Option HoleOpt name (OnceT (T ViewM)) o]
getHoleResults progName traversal =
    do
        env <- Env.make
        testProgram progName $
            convertWorkArea "" env
            <&> (^? Lens.cloneTraversal traversal)
            <&> fromMaybe (error "getHoleResults: Inexistent path")
            <&> (^? _BodyLeaf) <&> fromMaybe (error "getHoleResults: Not a leaf")
            <&> (^? _LeafHole . holeOptions)
            >>= fromMaybe (error "getHoleResults: Not a hole")
            >>= ($ Query (hasQueryLangInfo env) mempty "")

testSuspendedHoleResultSimple :: Test
testSuspendedHoleResultSimple =
    testCase "suspended-hole-result-simple" $
    do
        opts <- getHoleResults "simplest-if-with-holes.json" replHole
        let opt = head opts
        opt ^. optionTypeMatch & assertBool "First opt is not a type match"
        let optLam =
                opt ^? optionExpr . hVal . _HoleBinder . bBody . _BinderTerm . _BodyLam
                & fromMaybe (error "First opt is not a lam")
        Lens.has (lamFunc . fParams . _LhsVar . vIsNullParam . Lens.only True) optLam
            & assertBool "First opt is not being sugared as a null param lambda"
    where
        replHole = replBinder . _BinderTerm . _BodyIfElse . iThen . hVal

testSuspendedHoleResult :: Test
testSuspendedHoleResult =
    testCase "suspended-hole-result" $
    do
        opts <- getHoleResults "suspended-hole-result.json" replHole
        let opt = head opts
        opt ^. optionTypeMatch & assertBool "First opt is not a type match"
        Lens.has
            ( optionExpr . hVal . _HoleBinder . bBody . _BinderTerm . _BodyLam
            . lamFunc . fParams . _LhsVar . vIsNullParam . Lens.only True
            ) opt
            & assertBool "First opt is not a null param lambda"
    where
        replHole =
            replBinder . _BinderTerm . _BodyLam . lamFunc . fBody .
            hVal . bBody . _BinderTerm . _BodyIfElse . iThen . hVal
