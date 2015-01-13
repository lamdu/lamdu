module Graphics.UI.Bottle.Animation.Id
    ( AnimId
    , joinId, subId
    ) where

import qualified Data.ByteString as SBS
import           Data.List (isPrefixOf)

type AnimId = [SBS.ByteString]

joinId :: AnimId -> AnimId -> AnimId
joinId = (++)

subId :: AnimId -> AnimId -> Maybe AnimId
subId folder path
  | folder `isPrefixOf` path = Just $ drop (length folder) path
  | otherwise = Nothing
