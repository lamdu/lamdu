{-# OPTIONS -fno-warn-orphans #-} -- Arbitrary ByteString
module Data.ByteString.Arbitrary () where

import Test.QuickCheck (Arbitrary(..))

instance Arbitrary SBS.ByteString where
  arbitrary = fmap SBS.pack arbitrary
