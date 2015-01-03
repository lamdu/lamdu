{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.WidgetIds
  ( module Lamdu.GUI.WidgetIds
  , module Lamdu.GUI.WidgetIdIRef
  ) where

import Data.ByteString.Char8 (ByteString) -- IsString instance
import Data.Monoid (mappend)
import Data.Store.Guid (Guid)
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.WidgetId (Id(..))
import Lamdu.GUI.WidgetIdIRef
import Lamdu.Sugar.EntityId (EntityId)
import System.Random.Utils (randFunc)
import qualified Data.Store.Guid as Guid
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Sugar.EntityId as EntityId

fromBS :: ByteString -> Id
fromBS = Id . (: [])

fromEntityId :: EntityId -> Id
fromEntityId = fromBS . EntityId.bs

fromGuid :: Guid -> Id
fromGuid = fromBS . Guid.bs

hash :: Show a => a -> Widget.Id
hash = fromGuid . randFunc . show

backgroundCursorId :: AnimId
backgroundCursorId = ["background cursor"]

textCursorId :: AnimId
textCursorId = ["text cursor"]

branchSelection :: Widget.Id
branchSelection = Widget.Id ["selected branch"]

goUpId :: Widget.Id
goUpId = Widget.Id ["go up"]

builtinFFIPath :: Widget.Id -> Widget.Id
builtinFFIPath = flip Widget.joinId ["FFIPath"]

builtinFFIName :: Widget.Id -> Widget.Id
builtinFFIName = flip Widget.joinId ["FFIName"]

searchTermId :: Widget.Id -> Widget.Id
searchTermId = flip Widget.joinId ["search term"]

parenHighlightId :: AnimId
parenHighlightId = ["paren highlight"]

parensPrefix :: AnimId -> AnimId
parensPrefix = flip mappend ["parens"]

underlineId :: AnimId -> AnimId
underlineId = flip mappend ["underline"]

activePaneBackground :: AnimId
activePaneBackground = ["active def bg"]

flyNav :: AnimId
flyNav = ["flyNav"]
