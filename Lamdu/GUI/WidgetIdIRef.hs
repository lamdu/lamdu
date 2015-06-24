-- TODO: Kill this module? Sugar should be providing all the ids the
-- GUI sees. GUI shouldn't see IRefs directly at all

-- This module is used to avoid a dependency on WidgetIds by
-- ExampleDB/tests/etc
module Lamdu.GUI.WidgetIdIRef
    ( fromIRef
    ) where

import qualified Data.Store.Guid as Guid
import           Data.Store.Guid (Guid)
import qualified Data.Store.IRef as IRef
import           Data.Store.IRef (IRef)
import           Graphics.UI.Bottle.WidgetId (Id(..))

fromGuid :: Guid -> Id
fromGuid = Id . (: []) . Guid.bs

fromIRef :: IRef m a -> Id
fromIRef = fromGuid . IRef.guid
