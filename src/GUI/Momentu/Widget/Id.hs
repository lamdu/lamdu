{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GUI.Momentu.Widget.Id
    ( Id(..)
    , joinId, subId
    ) where

import Data.Binary (Binary)
import Data.List (intercalate)
import Data.List.Lens (prefixed)
import GUI.Momentu.Animation.Id (AnimId)
import Numeric.Extended (encodeHex)

import Lamdu.Prelude

newtype Id = Id
    { toAnimId :: AnimId
    } deriving (Eq, Ord, Read, Binary, Semigroup, Monoid, Generic)

instance Show Id where
    show (Id animId) =
        "W:" ++ intercalate ":" (map each animId)
        where
            each bs = encodeHex bs ++ "(" ++ show bs ++ ")"

joinId :: Id -> AnimId -> Id
joinId (Id x) y = x ++ y & Id

subId :: Id -> Id -> Maybe AnimId
subId (Id folder) (Id path) = path ^? prefixed folder
