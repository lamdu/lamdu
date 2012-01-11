{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.UI.Bottle.AnimIds(
  backgroundCursorId, textCursorId, fromIRef, valueEditId,
  deeperId, collapserId, branchSelection, goUpId)
where

import Graphics.UI.Bottle.Animation(AnimId)
import Data.Store.IRef(IRef, guid)
import Data.Store.Guid(bs)

backgroundCursorId :: AnimId
backgroundCursorId = ["background cursor"]

textCursorId :: AnimId
textCursorId = ["text cursor"]

fromIRef :: IRef a -> AnimId
fromIRef = (: []) . bs . guid

valueEditId :: AnimId -> AnimId
valueEditId = ("value edit" :)

deeperId :: AnimId -> AnimId
deeperId = ("deeper" :)

collapserId :: AnimId -> AnimId
collapserId = ("collapser" :)

branchSelection :: AnimId
branchSelection = ["selected branch"]

goUpId :: AnimId
goUpId = ["go up"]
