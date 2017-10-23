{-# LANGUAGE NoImplicitPrelude #-}
module GUI.Momentu.Widget.Id
    ( Id(..)
    , joinId, subId
    ) where

import Data.List.Lens (prefixed)
import GUI.Momentu.Animation.Id (AnimId)
import GUI.Momentu.Widget.Instances ()
import GUI.Momentu.Widget.Types (Id(..))

import Lamdu.Prelude

joinId :: Id -> AnimId -> Id
joinId (Id x) y = x ++ y & Id

subId :: Id -> Id -> Maybe AnimId
subId (Id folder) (Id path) = path ^? prefixed folder
