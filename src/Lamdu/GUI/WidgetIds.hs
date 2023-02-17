module Lamdu.GUI.WidgetIds
    ( module Lamdu.GUI.WidgetIds
    , module Lamdu.GUI.WidgetIdIRef
    ) where

import           Data.UUID.Types (UUID)
import qualified Data.UUID.Utils as UUIDUtils
import           GUI.Momentu.Element.Id (ElemId(..))
import           Lamdu.GUI.WidgetIdIRef
import qualified Lamdu.Sugar.EntityId as EntityId
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

defaultCursor :: ElemId
defaultCursor = gotoDefId

fromBS :: ByteString -> ElemId
fromBS = ElemId . (: [])

fromEntityId :: Sugar.EntityId -> ElemId
fromEntityId = fromBS . EntityId.bs

ofTagValue :: Sugar.EntityId -> ElemId
ofTagValue = (<> "val") . fromEntityId

fromExprPayload :: Sugar.Payload v o -> ElemId
fromExprPayload pl = fromEntityId (pl ^. Sugar.plEntityId)

literalEditOf :: ElemId -> ElemId
literalEditOf = (<> "literal edit")

fromUUID :: UUID -> ElemId
fromUUID = fromBS . UUIDUtils.toSBS16

branchSelection :: ElemId
branchSelection = "selected branch"

statusBarHamburger :: ElemId
statusBarHamburger = "hamburger"

tagHoleId :: ElemId -> ElemId
tagHoleId = (<> "hole")

fragmentHoleId :: ElemId -> ElemId
fragmentHoleId = (<> "hole")

gotoDefId :: ElemId
gotoDefId = "goto-def"
