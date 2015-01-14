{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.WidgetId
  ( Id(..)
  , joinId, subId, augmentId
  ) where

import           Control.Lens.Operators
import           Data.Binary (Binary)
import           Data.ByteString.Char8 (pack)
import           Data.List (intercalate)
import           Data.List.Lens (prefixed)
import           Data.Monoid (Monoid(..))
import           Graphics.UI.Bottle.Animation.Id (AnimId)

newtype Id = Id
  { toAnimId :: AnimId
  } deriving (Eq, Ord, Read, Binary, Monoid)

instance Show Id where
  show = ('W' :) . intercalate ":" . map show . toAnimId

joinId :: Id -> AnimId -> Id
joinId (Id x) y = Id $ x ++ y

subId :: Id -> Id -> Maybe AnimId
subId (Id folder) (Id path) = path ^? prefixed folder

augmentId :: Show a => a -> Id -> Id
augmentId x = (`joinId` [pack (show x)])
