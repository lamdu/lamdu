module GUI.Momentu.Animation.Id
    ( AnimId
    , augmentId
    , mappingFromPrefixMap
    ) where

import qualified Data.ByteString.Char8 as SBS8
import           Data.List.Lens (prefixed)
import qualified Data.Map as Map

import           Lamdu.Prelude

type AnimId = [ByteString]

augmentId :: Show a => AnimId -> a -> AnimId
augmentId animId = (animId ++) . (:[]) . SBS8.pack . show

mappingFromPrefixMap :: Map AnimId AnimId -> AnimId -> AnimId
mappingFromPrefixMap m animId =
    do
        (animIdPrefixCandidate, newAnimId) <- Map.lookupLE animId m
        suffix <- animId ^? prefixed animIdPrefixCandidate
        newAnimId <> suffix & Just
    & fromMaybe animId
