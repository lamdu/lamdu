{-# LANGUAGE TemplateHaskell #-}
module Data.Store.Rev.Change (Key, Value, Change(..))
where

import Data.Binary (Binary(..))
import Data.ByteString (ByteString)
import Data.Derive.Binary(makeBinary)
import Data.DeriveTH(derive)
import Data.Store.Guid (Guid)

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
