-- | Test Lamdu.Calc.Val.Utils
{-# LANGUAGE TypeApplications #-}
module Tests.ValUtils where

import qualified Data.Set as Set
import           Data.Tree.Diverse (annotations)
import           Lamdu.Calc.Term (Val)
import           Lamdu.Calc.Term.Arbitrary ()
import           Lamdu.Calc.Term.Utils (culledSubexprPayloads)

import           Test.Lamdu.Prelude

prop_allPayloads :: Ord a => Val a -> Bool
prop_allPayloads val =
    Set.fromList (val ^.. annotations) ==
    Set.fromList (culledSubexprPayloads (const False) val)

test :: Test
test =
    testProperty "Test culledSubexprPayloads returns all payloads with const False"
    (prop_allPayloads @Int)
