{-# OPTIONS -fno-warn-orphans #-}
module Data.Store.Guid.Arbitrary () where

import Control.Monad (replicateM)
import Data.Store.Guid (Guid)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.ByteString as BS
import qualified Data.Store.Guid as Guid

instance Arbitrary Guid where
  arbitrary = fmap (Guid.make . BS.pack) $ replicateM Guid.length arbitrary
