{-# LANGUAGE TypeFamilies #-}
-- | Test precedences
module Tests.Precedence where

import           Control.Monad.Unit (Unit(..))
import qualified Control.Lens as Lens
import           Control.Lens.Tuple
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Sugar.Parens as Parens
import qualified Lamdu.Sugar.Types as Sugar
import           Test.Lamdu.SugarStubs (($$), ($.))
import qualified Test.Lamdu.SugarStubs as Stub

import           Test.Lamdu.Prelude

test :: Test
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
testPunnedArgOp :: Test
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
            , Sugar._aPunnedArgs =
                [ Sugar.PunnedVar (Stub.defRef "b" "b" & Sugar.GetBinder & Const & Stub.node) "b"
                ]
            } & Stub.node
            & Parens.addToTopLevel 0

testGetFieldOfApply :: Test
testGetFieldOfApply =
    expr ^?!
    hVal . Sugar._BodySimpleApply . V.appArg . annotation . _1 . Sugar.piNeedParens
    & assertBool "get field should disambiguate compound expression"
    & testCase "get-field-of-apply"
    where
        expr = Stub.identity $$ (Stub.hole $. "a") & Parens.addToTopLevel 0

testInjectInRec :: Test
testInjectInRec =
    expr ^?!
    hVal . Sugar._BodyRecord . Sugar.cItems . traverse . Sugar.tiValue . annotation . _1 . Sugar.piNeedParens
    & assertBool "get field should disambiguate compound expression"
    & testCase "inject-in-record"
    where
        expr =
            Sugar.BodyRecord Sugar.Composite
            { Sugar._cItems =
                [ Stub.mkTag Nothing "x"
                    & Sugar.LeafInject & Sugar.BodyLeaf & Stub.node
                    & Sugar.TaggedItem (Stub.mkTag Nothing "x") Unit
                ]
            , Sugar._cPunnedItems = []
            , Sugar._cTail = Sugar.ClosedComposite (Sugar.ClosedCompositeActions Unit)
            , Sugar._cAddItem = "stub" <$ Stub.tagRefReplace
            } & Stub.node
            & Parens.addToTopLevel 0

testMinOpPrecOperator :: Test
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
test445 :: Test
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
test513 :: Test
test513 =
    assertBool "Expected paren" (problemPos ^. annotation . _1 . Sugar.piNeedParens)
    & testCase "513-applied-getfield"
    where
        expr = h Stub.$. "minimum" $$ h & Parens.addToTopLevel 0
        problemPos = expr ^?! hVal . Sugar._BodySimpleApply . Sugar.appFunc
        h = Stub.hole

-- Test for https://trello.com/c/BGSSndJi/514-disambiguate-fragments-on-lambdas-vs-in-lambdas
test514 :: Test
test514 =
    assertBool "Expected paren" (problemPos ^. annotation . _1 . Sugar.piNeedParens)
    & testCase "514-lambda-in-fragment"
    where
        expr =
            Sugar.BodyFragment Sugar.Fragment
            { Sugar._fExpr =
                Sugar.BodyLam Sugar.Lambda
                { Sugar._lamMode = Sugar.NormalBinder
                , Sugar._lamApplyLimit = Sugar.UnlimitedFuncApply
                , Sugar._lamFunc =
                    (Stub.node . Sugar.BodyLeaf . Sugar.LeafGetVar . Sugar.GetParam)
                    (Sugar.ParamRef (Stub.nameRef (Stub.taggedEntityName "x" "x")) Sugar.NormalBinder)
                    $$ Stub.hole
                    & Stub.funcExpr [("x", "x")]
                } & Stub.node
            , Sugar._fHeal = Unit
            , Sugar._fTypeMismatch = Nothing
            , Sugar._fOptions = pure (const (pure []))
            } & Stub.node
            & Parens.addToTopLevel 0
        problemPos = expr ^?! hVal . Sugar._BodyFragment . Sugar.fExpr
