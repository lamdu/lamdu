module Graphics.UI.Bottle.Animation.Id
    ( AnimId
    , joinId, subId
    , mappingFromPrefixMap
    ) where

import           Control.Lens.Operators
import qualified Data.ByteString as SBS
import           Data.List.Lens (prefixed)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))

type AnimId = [SBS.ByteString]

joinId :: AnimId -> AnimId -> AnimId
joinId = (++)

subId :: AnimId -> AnimId -> Maybe AnimId
subId folder path = path ^? prefixed folder

mappingFromPrefixMap :: Map AnimId AnimId -> AnimId -> AnimId
mappingFromPrefixMap m animId =
    do
        (animIdPrefixCandidate, newAnimId) <- Map.lookupLE animId m
        suffix <- animId ^? prefixed animIdPrefixCandidate
        newAnimId <> suffix & Just
    & fromMaybe animId
