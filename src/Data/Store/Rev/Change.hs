{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# OPTIONS -O2 -Wall #-}
module Data.Store.Rev.Change
    (Key, Value,
     Change(Change), make,
     Dir, objectKey, oldValue, newValue)
where

import Data.ByteString   (ByteString)
import Data.Binary       (Binary(..))
import Data.Binary.Utils (get3, put3)
import Data.Record.Label ((:->), mkLabels, lens)
import Data.Store.Guid   (Guid)

type Key = Guid
type Value = ByteString

-- TODO: Store the smaller of (Maybe Value) and (IRef Value)
data Change = Change {
  _objectKey :: Key,
  _oldValue :: Maybe Value,
  _newValue :: Maybe Value
  }
  deriving (Eq, Ord, Show, Read)
$(mkLabels [''Change])
-- objectKey :: Change :-> Key
type Dir = Change :-> Maybe Value
-- oldValue :: Dir
-- newValue :: Dir
instance Binary Change where
  get = get3 Change
  put (Change key old new) = put3 key old new

make :: Key -> Maybe Value -> Maybe Value -> Change
make = Change