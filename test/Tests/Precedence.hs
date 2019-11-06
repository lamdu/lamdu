-- | Test precedences
module Tests.Precedence where

import qualified Control.Lens as Lens
import           Control.Lens.Tuple
import           Hyper (annotation, hVal)
import qualified Lamdu.Sugar.Parens as Parens
import qualified Lamdu.Sugar.Types as Sugar
import           Test.Lamdu.SugarStubs (($$), ($.))
import qualified Test.Lamdu.SugarStubs as Stub

import           Test.Lamdu.Prelude

infixArgs ::
    Lens.Traversal'
    (Sugar.Expression name i o a)
    ( Sugar.Expression name i o a
    , Sugar.Expression name i o a
    )
infixArgs = hVal . Sugar._BodyLabeledApply . Sugar.aSpecialArgs . Sugar._Infix

test :: Test
test =
    testGroup "precedence"
    [ testMinOpPrecInfix
    , testPunnedArgOp
    , testGetFieldOfApply
    , test445
    ]

-- | Test for issue #471
-- https://trello.com/c/fQmgXuRE/471-operators-on-punned-args-dont-work-require-fragmenting-first
testPunnedArgOp :: Test
testPunnedArgOp =
    expr ^?!
    hVal . Sugar._BodyLabeledApply . Sugar.aPunnedArgs . traverse . annotation . _1
    & assertEqual "punned arg precedence" 0
    & testCase "punned-arg-op"
    where
        expr =
            Sugar.BodyLabeledApply Sugar.LabeledApply
            { Sugar._aFunc = Stub.defRef "a" "a" & Const & Stub.node
            , Sugar._aSpecialArgs = Sugar.Verbose
            , Sugar._aAnnotatedArgs = []
            , Sugar._aPunnedArgs =
                [ Stub.defRef "b" "b" & Sugar.GetBinder & Const & Stub.node
                ]
            } & Stub.node
            & Parens.addToExprWith 0

testGetFieldOfApply :: Test
testGetFieldOfApply =
    expr ^?!
    hVal . Sugar._BodyGetField . Sugar.gfRecord . annotation . _2
    & assertEqual "get field should disambiguate compound expression"
        Parens.NeedsParens
    & testCase "get-field-of-apply"
    where
        expr = (Stub.identity $$ Stub.hole) $. "a" & Parens.addToExprWith 0

testMinOpPrecInfix :: Test
testMinOpPrecInfix =
    do
        assertEqual "Plus in mul need no paren?!" Parens.NeedsParens needsParens
        assertEqual "minOpPrec is not 10?!" 10 minOpPrec
        & testCase "min-op-prec-infix"
    where
        (minOpPrec, needsParens, _) = expr ^?! infixArgs . _2 . annotation
        expr = i 1 `Stub.mul` (i 2 `Stub.plus` i 3) & Parens.addToExprWith 0
        i = Stub.litNum

-- Test for https://trello.com/c/OuaLvwiJ/445-wrong-parenthesis
test445 :: Test
test445 =
    assertEqual "Expected paren" Parens.NeedsParens (problemPos ^. annotation . _2)
    & testCase "445-wrong-parenthesis"
    where
        expr =
            Stub.identity $$ ((i 1 `Stub.plus` i 2) `Stub.mul` i 3)
            & Parens.addToExprWith 0
        problemPos =
            expr ^?!
            hVal . Sugar._BodySimpleApply . Sugar.appArg .
            hVal . Sugar._BodyLabeledApply . Sugar.aSpecialArgs . Sugar._Infix . _1
        i = Stub.litNum
