{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module AnimIds(
  backgroundCursorId, textCursorId, fromIRef,
  deeperId, collapserId, branchSelection, goUpId,
  delegating, notDelegating)
where

import Graphics.UI.Bottle.Animation(AnimId, joinId)
import Data.Store.IRef(IRef, guid)
import Data.Store.Guid(bs)

backgroundCursorId :: AnimId
backgroundCursorId = ["background cursor"]

textCursorId :: AnimId
textCursorId = ["text cursor"]

fromIRef :: IRef a -> AnimId
fromIRef = (: []) . bs . guid

deeperId :: AnimId -> AnimId
deeperId = flip joinId ["deeper"]

collapserId :: AnimId -> AnimId
collapserId = flip joinId ["collapser"]

branchSelection :: AnimId
branchSelection = ["selected branch"]

delegating :: AnimId -> AnimId
delegating = flip joinId ["delegating"]

notDelegating :: AnimId -> AnimId
notDelegating = flip joinId ["not delegating"]

goUpId :: AnimId
goUpId = ["go up"]
