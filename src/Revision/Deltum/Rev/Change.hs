module Revision.Deltum.Rev.Change (Key, Value, Change(..))
where

import Data.UUID.Types (UUID)

import Lamdu.Prelude

type Key = UUID
type Value = ByteString

-- TODO: Store the smaller of (Maybe Value) and (IRef Value)
data Change = Change
    { objectKey :: !Key
    , oldValue :: !(Maybe Value)
    , newValue :: !(Maybe Value)
    }
    deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass Binary
