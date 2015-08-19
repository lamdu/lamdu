{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.WidgetId
    ( Id(..)
    , joinId, subId
    ) where

import Prelude.Compat

import Control.Lens.Operators
import Data.Binary (Binary)
import Data.List (intercalate)
import Data.List.Lens (prefixed)
import Graphics.UI.Bottle.Animation.Id (AnimId)
import Numeric.Utils (encodeHex)

newtype Id = Id
    { toAnimId :: AnimId
    } deriving (Eq, Ord, Read, Binary, Monoid)

instance Show Id where
    show (Id animId) =
        "W:" ++ intercalate ":" (map each animId)
        where
            each bs = encodeHex bs ++ "(" ++ show bs ++ ")"

joinId :: Id -> AnimId -> Id
joinId (Id x) y = Id $ x ++ y

subId :: Id -> Id -> Maybe AnimId
subId (Id folder) (Id path) = path ^? prefixed folder
