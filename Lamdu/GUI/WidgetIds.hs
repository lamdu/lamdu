{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.WidgetIds
    ( module Lamdu.GUI.WidgetIds
    , module Lamdu.GUI.WidgetIdIRef
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
import           Data.ByteString.Char8 (ByteString)
import           Data.Store.Guid (Guid)
import qualified Data.Store.Guid as Guid
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.WidgetId (Id(..))
import           Lamdu.GUI.WidgetIdIRef
import qualified Lamdu.Sugar.EntityId as EntityId
import qualified Lamdu.Sugar.Types as Sugar
import           System.Random.Utils (randFunc)

fromBS :: ByteString -> Id
fromBS = Id . (: [])

fromEntityId :: Sugar.EntityId -> Id
fromEntityId = fromBS . EntityId.bs

fromExprPayload :: Sugar.Payload m a -> Id
fromExprPayload pl = fromEntityId (pl ^. Sugar.plEntityId)

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

delegatingId :: Widget.Id -> Widget.Id
delegatingId = flip Widget.joinId ["delegating"]

notDelegatingId :: Widget.Id -> Widget.Id
notDelegatingId = flip Widget.joinId ["non-delegating"]
