{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.WidgetIds
    ( module Lamdu.GUI.WidgetIds
    , module Lamdu.GUI.WidgetIdIRef
    ) where

import           Control.Lens.Operators
import           Data.ByteString.Char8 (ByteString)
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Utils as UUIDUtils
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.WidgetId (Id(..))
import           Lamdu.GUI.WidgetIdIRef
import qualified Lamdu.Sugar.EntityId as EntityId
import qualified Lamdu.Sugar.Types as Sugar
import           System.Random.Utils (randFunc)

import           Prelude.Compat

fromBS :: ByteString -> Id
fromBS = Id . (: [])

fromEntityId :: Sugar.EntityId -> Id
fromEntityId = fromBS . EntityId.bs

fromExprPayload :: Sugar.Payload m a -> Id
fromExprPayload pl = fromEntityId (pl ^. Sugar.plEntityId)

nameEditOf :: Id -> Id
nameEditOf = delegatingId

fromUUID :: UUID -> Id
fromUUID = fromBS . UUIDUtils.toSBS16

hash :: Show a => a -> Id
hash = fromUUID . randFunc . show

branchSelection :: Id
branchSelection = Id ["selected branch"]

activePaneBackground :: AnimId
activePaneBackground = ["active def bg"]

flyNav :: AnimId
flyNav = ["flyNav"]

delegatingId :: Id -> Id
delegatingId = flip Widget.joinId ["delegating"]

notDelegatingId :: Id -> Id
notDelegatingId = flip Widget.joinId ["non-delegating"]
