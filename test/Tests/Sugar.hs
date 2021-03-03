-- | Test sugar convert results (including its actions)

module Tests.Sugar where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT)
import qualified Data.List.Class as List
import qualified Data.Property as Property
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Calc.Term as V
import           Lamdu.Data.Db.Layout (ViewM)
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Lens as SugarLens
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
    , paramAnnotations
    , testChangeParam
    , testExtract
    , testExtractForRecursion
    , testLightLambda
    , testNotALightLambda
    , testInline
    , testReorderLets
    , testReplaceParent
    , testReplaceParentFragment
    , setHoleToHole
    , testCreateLetInLetVal
    , testFloatToRepl
    , floatLetWithGlobalRef
    , testHoleTypeShown
    , testUnnamed
    , testValidHoleResult
    , testPunnedIso
    , testGroup "insist-tests"
        [ testInsistFactorial
        , testInsistEq
        , testInsistIf
        , testInsistSubsets
        ]
    ]

testSugarActionsWith ::
    FilePath ->
    [WorkArea (Annotation (EvaluationScopes Name (OnceT (T ViewM))) Name) Name (OnceT (T ViewM)) (T ViewM)
        ( Sugar.Payload (Annotation (EvaluationScopes Name (OnceT (T ViewM))) Name) Name (OnceT (T ViewM)) (T ViewM)
        , (ParenInfo, [EntityId])
        ) ->
        OnceT (T ViewM) a] ->
    Env ->
    IO ()
testSugarActionsWith program actions env =
    traverse_ (convertWorkArea env >>=) actions <* convertWorkArea env
    & testProgram program

-- | Verify that a sugar action does not result in a crash
testSugarActions ::
    FilePath ->
    [WorkArea (Annotation (EvaluationScopes Name (OnceT (T ViewM))) Name) Name (OnceT (T ViewM)) (T ViewM)
        ( Sugar.Payload (Annotation (EvaluationScopes Name (OnceT (T ViewM))) Name) Name (OnceT (T ViewM)) (T ViewM)
        , (ParenInfo, [EntityId])
        ) ->
        OnceT (T ViewM) a] ->
    IO ()
testSugarActions program actions =
    Env.make >>= testSugarActionsWith program actions

replBinder ::
    Lens.Traversal' (WorkArea v name i o a)
    ( Binder v name i o #
        Annotated a
    )
replBinder = waRepl . replExpr . hVal

replBody ::
    Lens.Traversal' (WorkArea v name i o a)
    (Term v name i o # Annotated a)
replBody = replBinder . _BinderTerm

replLet :: Lens.Traversal' (WorkArea v name i o a) (Let v name i o # Annotated a)
replLet = replBinder . _BinderLet

lamFirstParam ::
    Lens.Traversal' (Term v name i o a)
    (FuncParam v name, ParamInfo name i o)
lamFirstParam = _BodyLam . lamFunc . fParams . _Params . Lens.ix 0

testUnnamed :: Test
testUnnamed =
    testSugarActions "unnamed.json" [verify]
    & testCase "name-of-unnamed"
    where
        verify workArea =
            case workArea ^?! replBody . _BodyLeaf . _LeafGetVar . _GetBinder . bvNameRef . nrName of
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
            hVal . lamFirstParam . _2 . piTag . tagRefReplace . tcNewTag
            >>= lift . (^. toPick)

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
            hVal . _BinderLet . lBody .
            hVal . _BinderLet . lValue .
            annotation . _1 . plActions . extract

-- Test for issue #395
-- https://trello.com/c/UvBdhzzl/395-extract-of-binder-body-with-let-items-may-cause-inference-failure
testExtract :: Test
testExtract =
    testSugarActions "extract-lambda-with-let.json" [lift . (^?! action)]
    & testCase "extract"
    where
        action =
            replBody . _BodyLam . lamFunc . fBody . annotation . _1 . plActions .
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
                yOption <-
                    workArea ^?!
                    replBody . _BodyLam . lamFunc . fBody .
                    hVal . _BinderLet . lBody . hVal . _BinderTerm . _BodyLeaf . _LeafHole
                    . holeOptions
                    >>= findM isY . (OptsNormal &)
                    <&> fromMaybe (error "expected option")
                mkResult <-
                    yOption ^. hoResults & List.runList
                    <&>
                    \case
                    List.Cons (_, x) _ -> x
                    List.Nil -> error "expected Cons"
                result <- mkResult
                result ^. holeResultPick & lift
                _ <-
                    result ^?! holeResultConverted . hVal . _BinderTerm
                    . _BodyLeaf . _LeafGetVar . _GetBinder . bvInline . _InlineVar
                    & lift
                pure ()
            where
                isY option =
                    option ^. hoSearchTerms
                    <&> Lens.has (traverse . _HoleName) -- TODO: Checked if was get of a let variable
        verify workArea
            | Lens.has afterInline workArea = pure ()
            | otherwise = error "Expected inline result"
        afterInline =
            replBody . _BodyLam . lamFunc . fBody .
            hVal . _BinderTerm . _BodyLeaf . _LeafLiteral . _LiteralNum

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
    Env.make <&> has .~ Annotations.None
    >>= testSugarActionsWith "const-five.json" [verify]
    & testCase "param-annotations"
    where
        verify workArea =
            unless
            (Lens.allOf (replBody . lamFirstParam . _1 . fpAnnotation) (Lens.has _AnnotationNone) workArea)
            (error "parameter should not have type annotation")

delParam :: Test
delParam =
    testSugarActions "const-five.json" [lift . (^?! action), verify]
    & testCase "del-param"
    where
        action = replBody . lamFirstParam . _2 . piActions . fpDelete
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
            workArea ^?! arg . annotation . _1 . plActions . delete . _SetToHole
            & void & lift
        holeDel workArea =
            workArea ^?! arg . annotation . _1 . plActions . delete . _Delete
            & void & lift
        arg = replBody . _BodyLabeledApply . aSpecialArgs . _Operator . _2
        verify workArea
            | Lens.has afterDel workArea = pure ()
            | otherwise = error "Expected 1"
        afterDel = replBody . _BodyLeaf . _LeafLiteral . _LiteralNum

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
            hVal . Lens._Wrapped . bvNameRef . nrGotoDefinition
        extractDef =
            waPanes . traverse . SugarLens.paneBinder .
            annotation . _1 . plActions . extract

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
            hVal . _BodyLeaf . _LeafGetVar . _GetBinder . bvNameRef . nrGotoDefinition
        ifElse =
            waPanes . traverse . SugarLens.paneBinder .
            hVal . _BodyFunction . fBody .
            hVal . _BinderTerm . _BodyIfElse
        insist =
            Lens.cloneTraversal ifElse . iThen .
            hVal . _BodyLam . lamFunc . fBody .
            hVal . _BinderTerm . _BodyFragment . fHeal
        verify workArea
            | Lens.has unexpected workArea = error "fragment created at unexpected position"
            | otherwise = pure ()
        unexpected =
            Lens.cloneTraversal ifElse . iElse .
            hVal . _SimpleElse . _BodyLam . lamFunc . fBody .
            hVal . _BinderTerm . _BodySimpleApply . appFunc .
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
            replBody . _BodyLabeledApply . aSpecialArgs . _Operator . _2 .
            hVal . _BodyFragment . fHeal
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = error "fragment not created at expected position"
        expected =
            replBody . _BodyLabeledApply . aSpecialArgs . _Operator . _1 .
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
            hVal . _BinderTerm . _BodyFragment . fHeal
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = error "fragment not created at expected position"
        expected =
            replBody . _BodyIfElse . iElse .
            hVal . _SimpleElse . _BodyLam . lamFunc . fBody .
            hVal . _BinderTerm . _BodyFragment

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
            hVal . _BinderTerm . _BodyPostfixApply . pFunc .
            hVal . _PfCase . cItems . Lens.ix 1 . ciExpr .
            hVal . _BodyLam . lamFunc . fBody .
            hVal . _BinderTerm . _BodyLabeledApply . aSpecialArgs . _Operator
        insist =
            Lens.cloneTraversal consArgs . _2 .
            hVal . _BodyLam . lamFunc . fBody .
            hVal . _BinderLet . lBody .
            hVal . _BinderTerm . _BodyLabeledApply . aSpecialArgs . _Operator . _2 .
            hVal . _BodyLam . lamFunc . fBody .
            hVal . _BinderTerm . _BodyLabeledApply . aSpecialArgs . _Operator . _1 .
            hVal . _BodyFragment . fHeal
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = error "fragment not created at expected position"
        expected = Lens.cloneTraversal consArgs . _1 . hVal . _BodyFragment

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
            hVal . _BodyLam . lamMode . _LightLambda

testNotALightLambda :: Test
testNotALightLambda =
    testSugarActions "lam-in-lam.json" [verify]
    & testCase "not-a-light-lambda"
    where
        verify workArea
            | Lens.has expected workArea = pure ()
            | otherwise = error "Expected light lambda sugar!"
        expected = replBody . _BodyLam . lamMode . _NormalBinder

openTopLevelDef :: WorkArea v name i (T ViewM) a -> OnceT (T ViewM) ()
openTopLevelDef =
    lift . void . (^?! replBody . _BodyLeaf . _LeafGetVar . _GetBinder . bvNameRef . nrGotoDefinition)

delDefParam :: Test
delDefParam =
    testSugarActions "def-with-params.json"
    [openTopLevelDef, lift . (^?! action)]
    & testCase "del-def-param"
    where
        action =
            waPanes . traverse . SugarLens.paneBinder .
            hVal . _BodyFunction .
            fParams . _Params . traverse .
            _2 . piActions . fpDelete

updateDef :: Test
updateDef =
    testSugarActions "update-def-type.json"
    [openTopLevelDef, lift . void . (^?! action)]
    & testCase "update-def-type"
    where
        action =
            waPanes . traverse . SugarLens.paneBinder .
            hVal . _BodyFunction . fBody .
            hVal . _BinderTerm . _BodyLabeledApply . aFunc .
            hVal . Lens._Wrapped . bvForm . _GetDefinition . _DefTypeChanged . defTypeUseCurrent

testReplaceParent :: Test
testReplaceParent =
    testSugarActions "let-item-inline.json" [lift . (^?! action)]
    & testCase "replace-parent"
    where
        action =
            replBody . _BodyLam . lamFunc . fBody .
            annotation . _1 . plActions . mReplaceParent . Lens._Just

testReplaceParentFragment :: Test
testReplaceParentFragment =
    testSugarActions "multiply-list.json" [void . lift . (^?! action), verify]
    & testCase "replace-parent-fragment"
    where
        action =
            Lens.cloneTraversal fragExpr .
            hVal . _BodySimpleApply . appArg .
            annotation . _1 . plActions . mReplaceParent . Lens._Just
        fragExpr = replBody . _BodyLabeledApply . aSpecialArgs . traverse . hVal . _BodyFragment . fExpr
        verify workArea
            | Lens.has (Lens.cloneTraversal fragExpr) workArea =
                error "replace-parent did not remove fragment"
            | otherwise = pure ()

floatLetWithGlobalRef :: Test
floatLetWithGlobalRef =
    testSugarActions "let-with-global-reference.json"
    [ lift . (^?! replLet . lBody . hVal . _BinderLet . lValue . annotation . _1 . plActions . extract)
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
        setToHole :: Lens.Traversal' (WorkArea v name i o (Payload v name i o, a)) (o EntityId)
        setToHole =
            replBody . _BodyLam . lamFunc . fBody .
            hVal . _BinderLet . lValue .
            annotation . _1 . plActions . delete . _SetToHole

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
                workArea <- convertWorkArea env
                assertLetVals workArea 1 2
                _ <- workArea ^?! innerLet . annotation . _1 . plActions . extract & lift
                newWorkArea <- convertWorkArea env
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
        innerLet = replLet . lBody . hVal . _BinderLet . lValue
        literalVal = hVal . _BodyPlain . apBody . _BinderTerm . _BodyLeaf . _LeafLiteral . _LiteralNum . Property.pVal

testCreateLetInLetVal :: Test
testCreateLetInLetVal =
    testCase "create-let-in-let-hVal" $
    do
        env <- Env.make
        result <-
            testProgram "let-item-inline.json" $
                do
                    workArea <- convertWorkArea env
                    _ <-
                        workArea ^?!
                        theLet . lValue . annotation . _1 . plActions . mNewLet . Lens._Just
                        & lift
                    newWorkArea <- convertWorkArea env
                    Lens.has
                        ( theLet . lValue . hVal . _BodyPlain
                        . apBody . _BinderLet
                        ) newWorkArea & pure
        assertBool "Let was not created inside the let-value" result
    where
        -- | Extract from:
        -- >>> \x -> let y = 0 in <hole>
        --           ^^^^^^^^^^^^^^^^^^^
        theLet ::
            Lens.Traversal'
            (WorkArea v name i o a)
            (Let v name i o # Annotated a)
        theLet = replBody . _BodyLam . lamFunc . fBody . hVal . _BinderLet

testHoleTypeShown :: Test
testHoleTypeShown =
    testCase "hole-type-shown" $
    do
        env <- Env.make <&> has .~ Annotations.None
        workArea <- testProgram "to-nom.json" (convertWorkArea env)
        let x = workArea ^?! replBody . _BodyToNom . nVal
        putStrLn $ case x ^. annotation . _1 . plAnnotation of
            AnnotationType {} -> "Type"
            AnnotationVal {} -> "Val"
            AnnotationNone {} -> "None"
        Lens.has (annotation . _1 . plAnnotation . _AnnotationType) x
            & assertBool "Expected to have type"

-- Test for issue #497
-- https://trello.com/c/OAeXOCMi/497-invalid-nullary-inject-suggested
testValidHoleResult :: Test
testValidHoleResult =
    testCase "valid-hole-result" $
    do
        env <- Env.make
        testProgram "nom-list.json" $
            do
                workArea <- convertWorkArea env
                opts <-
                    workArea ^?!
                    replBody . _BodyToNom . nVal . hVal . _BinderTerm . _BodyLeaf . _LeafHole . holeOptions
                -- The bug occured in the first suggested result
                (_, mkHoleResult) <- opts OptsNormal ^?! Lens.ix 0 . hoResults & List.runList <&> List.headL
                holeResult <- mkHoleResult
                holeResult ^. holeResultPick & lift

-- Test for https://trello.com/c/Dzp5vgos/510-not-punning-auto-named-variables
testPunnedIso :: Test
testPunnedIso =
    testCase "punned-iso" $
    Env.make >>= testProgram "punned-fields.json" . convertWorkArea
    <&> (^? replBinder . _BinderLet . lBody . hVal . _BinderLet . lBody . hVal . _BinderTerm . _BodyRecord . cItems)
    <&> Lens.mapped . Lens.mapped %~
        (\x -> (x ^. ciTag . tagRefTag . tagName, x ^? ciExpr . hVal . _BodyLeaf . _LeafGetVar . _GetBinder . bvNameRef . nrName))
    >>= assertEqual "Record items expected to be punned" (Just [])
