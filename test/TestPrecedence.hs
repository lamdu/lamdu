-- | Test precedences
module TestPrecedence (test) where

import qualified Control.Lens as Lens
import qualified Lamdu.Sugar.Parens as Parens
import qualified Lamdu.Sugar.Types as Sugar
import           Test.Framework (Test)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (assertEqual)
import qualified Test.Lamdu.SugarStubs as Stub

import           Lamdu.Prelude

infixArgs ::
    Lens.Traversal'
    (Sugar.Expression name i o a)
    ( Sugar.Expression name i o a
    , Sugar.Expression name i o a
    )
infixArgs = Sugar.rBody . Sugar._BodyLabeledApply . Sugar.aSpecialArgs . Sugar._Infix

test :: Test
test =
    do
        (minOpPrec, needsParens, ()) <-
            expr ^?! infixArgs . _2 . Sugar.rPayload . Sugar.plData
            & pure
        assertEqual "Plus in mul need no paren?!" Parens.NeedsParens needsParens
        assertEqual "Parens minOpPrec is not 0?!" 0 minOpPrec
        pure ()
        & testCase "Test minOpPrec inside parenthesis"
    where
        expr = i 1 `Stub.mul` (i 2 `Stub.plus` i 3) & Parens.add
        i = Stub.litNum
