{-# LANGUAGE TypeFamilies #-}
-- | Test precedences
module Tests.Precedence (test) where

import           Control.Monad.Unit (Unit(..))
import qualified Control.Lens as Lens
import           Control.Lens.Tuple
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Sugar.Parens as Parens
import qualified Lamdu.Sugar.Types as Sugar
import           Test.Lamdu.SugarStubs (($$), ($.))
import qualified Test.Lamdu.SugarStubs as Stub

import           Test.Lamdu.Prelude

test :: TestTree
test =
    testGroup "precedence"
    [ testMinOpPrecOperator
    , testPunnedArgOp
    , testGetFieldOfApply
    , testInjectInRec
    , test445
    , test513
    , test514
    ]

-- | Test for issue #471
-- https://trello.com/c/fQmgXuRE/471-operators-on-punned-args-dont-work-require-fragmenting-first
testPunnedArgOp :: TestTree
testPunnedArgOp =
    expr ^?
    hVal . Sugar._BodyLabeledApply . Sugar.aPunnedArgs . traverse .
    Sugar.pvVar . annotation . _1 . Sugar.piMinOpPrec
    & assertEqual "punned arg precedence" (Just 0)
    & testCase "punned-arg-op"
    where
        expr =
            Sugar.BodyLabeledApply Sugar.LabeledApply
            { Sugar._aFunc = Stub.defRef "a" "a" & Const & Stub.node
            , Sugar._aMOpArgs = Nothing
            , Sugar._aAnnotatedArgs = []
            , Sugar._aPunnedArgs = [Sugar.PunnedVar (Stub.defRef "b" "b" & Const & Stub.node) "b"]
            } & Stub.node
            & Parens.addToTopLevel 0

testGetFieldOfApply :: TestTree
testGetFieldOfApply =
    expr ^?!
    hVal . Sugar._BodySimpleApply . V.appArg . annotation . _1 . Sugar.piNeedParens
    & assertBool "get field should disambiguate compound expression"
    & testCase "get-field-of-apply"
    where
        expr = Stub.identity $$ (Stub.hole $. "a") & Parens.addToTopLevel 0

testInjectInRec :: TestTree
testInjectInRec =
    expr ^?!
    hVal . Sugar._BodyRecord . Sugar.cList . Sugar.tlItems . Lens._Just . Sugar.tlHead . Sugar.tiValue .
    annotation . _1 . Sugar.piNeedParens
    & assertBool "get field should disambiguate compound expression"
    & testCase "inject-in-record"
    where
        expr =
            Sugar.BodyRecord Sugar.Composite
            { Sugar._cList =
                Sugar.TaggedList
                { Sugar._tlAddFirst = Identity Stub.tagRefReplace
                , Sugar._tlItems =
                    Just Sugar.TaggedListBody
                    { Sugar._tlHead =
                        Stub.mkTag Nothing "x"
                        & Sugar.LeafInject & Sugar.BodyLeaf & Stub.node
                        & Sugar.TaggedItem (Stub.mkTag Nothing "x") Unit (Identity Stub.tagRefReplace)
                    , Sugar._tlTail = []
                    }
                }
            , Sugar._cPunnedItems = []
            , Sugar._cTail = Sugar.ClosedCompositeTail (Sugar.ClosedCompositeActions Unit)
            } & Stub.node
            & Parens.addToTopLevel 0

testMinOpPrecOperator :: TestTree
testMinOpPrecOperator =
    do
        assertBool "Plus in mul need no paren?!" needsParens
        assertEqual "minOpPrec is not 10?!" 10 minOpPrec
        & testCase "min-op-prec-infix"
    where
        (Sugar.ParenInfo minOpPrec needsParens, _) =
            expr ^?! hVal . Sugar._BodyLabeledApply . Sugar.aMOpArgs . Lens._Just . Sugar.oaRhs . annotation
        expr = i 1 `Stub.mul` (i 2 `Stub.plus` i 3) & Parens.addToTopLevel 0
        i = Stub.litNum

-- Test for https://trello.com/c/OuaLvwiJ/445-wrong-parenthesis
test445 :: TestTree
test445 =
    assertBool "Expected paren" (problemPos ^. annotation . _1 . Sugar.piNeedParens)
    & testCase "445-wrong-parenthesis"
    where
        expr =
            Stub.identity $$ ((i 1 `Stub.plus` i 2) `Stub.mul` i 3)
            & Parens.addToTopLevel 0
        problemPos =
            expr ^?!
            hVal . Sugar._BodySimpleApply . Sugar.appArg .
            hVal . Sugar._BodyLabeledApply . Sugar.aMOpArgs . Lens._Just . Sugar.oaLhs
        i = Stub.litNum

-- Test for https://trello.com/c/xLzHmxpZ/513-disambiguate-applied-get-field-from-operator-function
test513 :: TestTree
test513 =
    assertBool "Expected paren" (problemPos ^. annotation . _1 . Sugar.piNeedParens)
    & testCase "513-applied-getfield"
    where
        expr = h Stub.$. "minimum" $$ h & Parens.addToTopLevel 0
        problemPos = expr ^?! hVal . Sugar._BodySimpleApply . Sugar.appFunc
        h = Stub.hole

-- Test for https://trello.com/c/BGSSndJi/514-disambiguate-fragments-on-lambdas-vs-in-lambdas
test514 :: TestTree
test514 =
    assertBool "Expected paren" (problemPos ^. annotation . _1 . Sugar.piNeedParens)
    & testCase "514-lambda-in-fragment"
    where
        expr =
            Sugar.BodyFragment Sugar.Fragment
            { Sugar._fExpr =
                Sugar.BodyLam Sugar.Lambda
                { Sugar._lamLightweight = False
                , Sugar._lamApplyLimit = Sugar.UnlimitedFuncApply
                , Sugar._lamFunc =
                    (Stub.node . Sugar.BodyLeaf . Sugar.LeafGetVar)
                    (Sugar.GetVar (Stub.taggedEntityName "x" "x")
                        Sugar.GetNormalVar Nothing "x" Sugar.CannotInline)
                    $$ Stub.hole
                    & Stub.funcExpr "x" "x"
                } & Stub.node
            , Sugar._fHeal = Unit
            , Sugar._fTypeMismatch = Nothing
            , Sugar._fOptions = mempty
            , Sugar._fOptApply = pure (error "shouldn't affect test")
            , Sugar._fTagSuffixes = mempty
            } & Stub.node
            & Parens.addToTopLevel 0
        problemPos = expr ^?! hVal . Sugar._BodyFragment . Sugar.fExpr
