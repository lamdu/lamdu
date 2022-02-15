module Lamdu.GUI.WidgetIds
    ( module Lamdu.GUI.WidgetIds
    , module Lamdu.GUI.WidgetIdIRef
    ) where

import           Data.UUID.Types (UUID)
import qualified Data.UUID.Utils as UUIDUtils
import           GUI.Momentu.Widget.Id (Id(..))
import qualified GUI.Momentu.Widget.Id as WidgetId
import           Lamdu.GUI.WidgetIdIRef
import qualified Lamdu.Sugar.EntityId as EntityId
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

defaultCursor :: Id
defaultCursor = gotoDefId

fromBS :: ByteString -> Id
fromBS = Id . (: [])

fromEntityId :: Sugar.EntityId -> Id
fromEntityId = fromBS . EntityId.bs

ofTagValue :: Sugar.EntityId -> Id
ofTagValue = (`WidgetId.joinId` ["val"]) . fromEntityId

fromExprPayload :: Sugar.Payload v o -> Id
fromExprPayload pl = fromEntityId (pl ^. Sugar.plEntityId)

literalEditOf :: Id -> Id
literalEditOf = flip WidgetId.joinId ["literal edit"]

fromUUID :: UUID -> Id
fromUUID = fromBS . UUIDUtils.toSBS16

branchSelection :: Id
branchSelection = Id ["selected branch"]

statusBarHamburger :: Id
statusBarHamburger = Id ["hamburger"]

tagHoleId :: Id -> Id
tagHoleId = (`WidgetId.joinId` ["hole"])

fragmentHoleId :: Id -> Id
fragmentHoleId = (`WidgetId.joinId` ["hole"])

gotoDefId :: Id
gotoDefId = Id ["goto-def"]
