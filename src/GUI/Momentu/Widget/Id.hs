{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GUI.Momentu.Widget.Id
    ( Id(..)
    , joinId, subId, isSubId
    ) where

import qualified Control.Lens as Lens
import           Data.List (intercalate)
import           Data.List.Lens (prefixed)
import           GUI.Momentu.Animation.Id (AnimId)
import           Numeric.Extended (encodeHex)

import           GUI.Momentu.Prelude

newtype Id = Id
    { toAnimId :: AnimId
    }
    deriving stock (Read, Generic)
    deriving newtype (Eq, Ord, Binary, Semigroup, Monoid)

instance Show Id where
    show (Id animId) =
        "W:" ++ intercalate ":" (map each animId)
        where
            each bs = encodeHex bs ++ "(" ++ show bs ++ ")"

joinId :: Id -> AnimId -> Id
joinId (Id x) y = x ++ y & Id

subId :: Id -> Id -> Maybe AnimId
Id short `subId` Id long = long ^? prefixed short

isSubId :: Id -> Id -> Bool
short `isSubId` long = short `subId` long & Lens.has Lens._Just
