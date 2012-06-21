{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Graphics.UI.Bottle.WidgetId (Id(..), joinId, subId, atId) where

import Data.Binary (Binary)
import Data.List(isPrefixOf)
import Data.Monoid (Monoid(..))
import Graphics.UI.Bottle.Animation (AnimId)
import qualified Data.AtFieldTH as AtFieldTH

newtype Id = Id {
  toAnimId :: AnimId
  }
  deriving (Eq, Ord, Show, Read, Binary, Monoid)

AtFieldTH.make ''Id

joinId :: Id -> AnimId -> Id
joinId (Id x) y = Id $ x ++ y

subId :: Id -> Id -> Maybe AnimId
subId (Id folder) (Id path)
  | folder `isPrefixOf` path = Just $ drop (length folder) path
  | otherwise = Nothing
