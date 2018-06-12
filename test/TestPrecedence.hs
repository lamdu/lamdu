-- | Test precedences
module TestPrecedence (test) where

import qualified Control.Lens as Lens
import qualified Lamdu.Sugar.Parens as Parens
import qualified Lamdu.Sugar.Types as Sugar
import qualified Test.Lamdu.SugarStubs as Stub

import           Test.Lamdu.Prelude

infixArgs ::
    Lens.Traversal'
    (Sugar.Expression name i o a)
    ( Sugar.Expression name i o a
    , Sugar.Expression name i o a
    )
infixArgs = Sugar.body . Sugar._BodyLabeledApply . Sugar.aSpecialArgs . Sugar._Infix

test :: Test
test =
    testGroup "precedence"
    [ testMinOpPrecInfix
    ]

testMinOpPrecInfix :: Test
testMinOpPrecInfix =
    do
        assertEqual "Plus in mul need no paren?!" Parens.NeedsParens needsParens
        assertEqual "Parens minOpPrec is not 0?!" 0 minOpPrec
        & testCase "min-op-prec-infix"
    where
        (minOpPrec, needsParens, ()) =
            expr ^?! infixArgs . _2 . Sugar.annotation . Sugar.plData
        expr = i 1 `Stub.mul` (i 2 `Stub.plus` i 3) & Parens.add
        i = Stub.litNum
