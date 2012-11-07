module Data.Store.Guid.Arbitrary () where

import Data.Store.Guid (Guid)
import Test.QuickCheck (Arbitrary(..))

instance Arbitrary Guid where
  arbitrary = fmap Guid $ arbitrary
