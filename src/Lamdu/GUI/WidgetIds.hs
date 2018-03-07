{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.WidgetIds
    ( module Lamdu.GUI.WidgetIds
    , module Lamdu.GUI.WidgetIdIRef
    ) where

import           Data.UUID.Types (UUID)
import qualified Data.UUID.Utils as UUIDUtils
import           GUI.Momentu.Animation.Id (AnimId)
import           GUI.Momentu.Widget.Id (Id(..))
import qualified GUI.Momentu.Widget.Id as WidgetId
import           Lamdu.GUI.WidgetIdIRef
import qualified Lamdu.Sugar.EntityId as EntityId
import qualified Lamdu.Sugar.Types as Sugar
import           System.Random.Utils (randFunc)

import           Lamdu.Prelude

defaultCursor :: Id
defaultCursor = replId

fromBS :: ByteString -> Id
fromBS = Id . (: [])

fromEntityId :: Sugar.EntityId -> Id
fromEntityId = fromBS . EntityId.bs

fromExprPayload :: Sugar.Payload name f a -> Id
fromExprPayload pl = fromEntityId (pl ^. Sugar.plEntityId)

literalEditOf :: Id -> Id
literalEditOf = flip WidgetId.joinId ["literal edit"]

fromUUID :: UUID -> Id
fromUUID = fromBS . UUIDUtils.toSBS16

hash :: Show a => a -> Id
hash = fromUUID . randFunc . show

branchSelection :: Id
branchSelection = Id ["selected branch"]

activePaneBackground :: AnimId
activePaneBackground = ["active def bg"]

letBinderId :: Id -> Id
letBinderId = (`WidgetId.joinId` ["letId"])

replId :: Id
replId = Id ["repl"]

tagHoleId :: Id -> Id
tagHoleId = (`WidgetId.joinId` ["hole"])

dotterId :: Id -> Id
dotterId widgetId = widgetId `WidgetId.joinId` ["dotter"]

-- Cursor dest for items first created
newDest :: Id -> Id
newDest = (`WidgetId.joinId` ["new"])
