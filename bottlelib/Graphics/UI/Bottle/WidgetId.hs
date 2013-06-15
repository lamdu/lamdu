{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.WidgetId (Id(..), joinId, subId, augmentId) where

import Data.Binary (Binary)
import Data.ByteString.Char8(pack) -- IsString instance
import Data.List (isPrefixOf, intercalate)
import Data.Monoid (Monoid(..))
import Graphics.UI.Bottle.Animation.Id (AnimId)
import Numeric.Utils (encodeHex)

newtype Id = Id {
  toAnimId :: AnimId
  } deriving (Eq, Ord, Read, Binary, Monoid)

instance Show Id where
  show = ('W' :) . intercalate ":" . map encodeHex . toAnimId

joinId :: Id -> AnimId -> Id
joinId (Id x) y = Id $ x ++ y

subId :: Id -> Id -> Maybe AnimId
subId (Id folder) (Id path)
  | folder `isPrefixOf` path = Just $ drop (length folder) path
  | otherwise = Nothing

augmentId :: Show a => a -> Id -> Id
augmentId x = (`joinId` [pack (show x)])
