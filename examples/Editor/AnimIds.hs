{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Editor.AnimIds(
  backgroundCursorId, textCursorId, fromIRef,
  collapserId, branchSelection, goUpId,
  delegating, notDelegating)
where

import Graphics.UI.Bottle.Animation(AnimId, joinId)
import Data.Store.IRef(IRef, guid)
import Data.Store.Guid(bs)
import Data.ByteString.Char8() -- IsString instance

backgroundCursorId :: AnimId
backgroundCursorId = ["background cursor"]

textCursorId :: AnimId
textCursorId = ["text cursor"]

fromIRef :: IRef a -> AnimId
fromIRef = (: []) . bs . guid

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
