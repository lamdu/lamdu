-- | Test Lamdu.Calc.Val.Utils
{-# LANGUAGE TypeApplications #-}
module TestValUtils where

import qualified Control.Lens as Lens
import qualified Data.Set as Set
import           Lamdu.Calc.Val.Annotated (Val)
import           Lamdu.Calc.Val.Annotated.Arbitrary ()
import           Lamdu.Calc.Val.Utils (culledSubexprPayloads)
import           Test.Framework (Test)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

import           Test.Lamdu.Prelude

prop_allPayloads :: Ord a => Val a -> Bool
prop_allPayloads val =
    Set.fromList (val ^.. Lens.folded) ==
    Set.fromList (culledSubexprPayloads (const False) val)

test :: Test
test =
    testProperty "Test culledSubexprPayloads returns all payloads with const False"
    (prop_allPayloads @Int)
