-- | Test precedences
module Tests.Precedence where

import qualified Control.Lens as Lens
import           Control.Lens.Tuple
import           Data.Tree.Diverse (ann, val)
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
infixArgs = val . Sugar._BodyLabeledApply . Sugar.aSpecialArgs . Sugar._Infix

test :: Test
test =
    testGroup "precedence"
    [ testMinOpPrecInfix
    , testGetFieldOfApply
    , test445
    ]

testGetFieldOfApply :: Test
testGetFieldOfApply =
    expr ^?!
    val . Sugar._BodyGetField . Sugar.gfRecord . ann . _2
    & assertEqual "get field should disambiguate compound expression"
        Parens.NeedsParens
    & testCase "get-field-of-apply"
    where
        expr = (Stub.identity $$ Stub.hole) $. "a" & Parens.addToExpr

testMinOpPrecInfix :: Test
testMinOpPrecInfix =
    do
        assertEqual "Plus in mul need no paren?!" Parens.NeedsParens needsParens
        assertEqual "minOpPrec is not 10?!" 10 minOpPrec
        & testCase "min-op-prec-infix"
    where
        (minOpPrec, needsParens, _) = expr ^?! infixArgs . _2 . ann
        expr = i 1 `Stub.mul` (i 2 `Stub.plus` i 3) & Parens.addToExpr
        i = Stub.litNum

-- Test for https://trello.com/c/OuaLvwiJ/445-wrong-parenthesis
test445 :: Test
test445 =
    assertEqual "Expected paren" Parens.NeedsParens (problemPos ^. ann . _2)
    & testCase "445-wrong-parenthesis"
    where
        expr =
            Stub.identity $$ ((i 1 `Stub.plus` i 2) `Stub.mul` i 3)
            & Parens.addToExpr
        problemPos =
            expr ^?!
            val . Sugar._BodySimpleApply . Sugar.applyArg .
            val . Sugar._BodyLabeledApply . Sugar.aSpecialArgs . Sugar._Infix . _1
        i = Stub.litNum
