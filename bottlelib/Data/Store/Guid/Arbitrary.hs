{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-orphans #-}
module Data.Store.Guid.Arbitrary () where

import           Control.Monad (replicateM)
import qualified Data.ByteString as BS
import           Data.Store.Guid (Guid)
import qualified Data.Store.Guid as Guid
import           Test.QuickCheck (Arbitrary(..))

import           Prelude.Compat

instance Arbitrary Guid where
    arbitrary = Guid.make . BS.pack <$> replicateM Guid.length arbitrary
