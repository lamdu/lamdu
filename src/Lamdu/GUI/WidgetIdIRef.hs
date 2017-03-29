-- TODO: Kill this module? Sugar should be providing all the ids the
-- GUI sees. GUI shouldn't see IRefs directly at all

-- This module is used to avoid a dependency on WidgetIds by tests
module Lamdu.GUI.WidgetIdIRef
    ( fromIRef
    ) where

import           Data.ByteString.Utils (strictifyBS)
import           Data.Store.IRef (IRef)
import qualified Data.Store.IRef as IRef
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import           Graphics.UI.Bottle.WidgetId (Id(..))

fromUUID :: UUID -> Id
fromUUID = Id . (: []) . strictifyBS . UUID.toByteString

fromIRef :: IRef m a -> Id
fromIRef = fromUUID . IRef.uuid
