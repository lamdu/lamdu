{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.WidgetId (Id(..), joinId, subId) where

import Data.Binary (Binary)
import Data.List (isPrefixOf, intercalate)
import Data.Monoid (Monoid(..))
import Graphics.UI.Bottle.Animation (AnimId)
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
