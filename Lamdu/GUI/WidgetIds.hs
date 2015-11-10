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

nameEditOf :: Id -> Id
nameEditOf = delegatingId

fromGuid :: Guid -> Id
fromGuid = fromBS . Guid.bs

hash :: Show a => a -> Id
hash = fromGuid . randFunc . show

backgroundCursorId :: AnimId
backgroundCursorId = ["background cursor"]

textCursorId :: AnimId
textCursorId = ["text cursor"]

branchSelection :: Id
branchSelection = Id ["selected branch"]

goUpId :: Id
goUpId = Id ["go up"]

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

delegatingId :: Id -> Id
delegatingId = flip Widget.joinId ["delegating"]

notDelegatingId :: Id -> Id
notDelegatingId = flip Widget.joinId ["non-delegating"]
