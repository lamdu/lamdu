{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -O2 -Wall #-}
module Data.Store.Rev.Change (Key, Value, Change(..))
where

import Data.ByteString (ByteString)
import Data.Binary (Binary(..))
import Data.Store.Guid (Guid)
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)

type Key = Guid
type Value = ByteString

-- TODO: Store the smaller of (Maybe Value) and (IRef Value)
data Change = Change {
  objectKey :: Key,
  oldValue :: Maybe Value,
  newValue :: Maybe Value
  }
  deriving (Eq, Ord, Show, Read)
derive makeBinary ''Change
