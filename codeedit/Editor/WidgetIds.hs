{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Editor.WidgetIds(
  backgroundCursorId, textCursorId, fromIRef,
  collapserId, branchSelection, goUpId,
  delegating, notDelegating, searchTermId,
  parenHighlightId)
where

import Data.ByteString.Char8() -- IsString instance
import Data.Store.Guid(bs)
import Data.Store.IRef(IRef, guid)
import qualified Graphics.UI.Bottle.Widget as Widget

backgroundCursorId :: Widget.Id
backgroundCursorId = Widget.Id ["background cursor"]

textCursorId :: Widget.Id
textCursorId = Widget.Id ["text cursor"]

fromIRef :: IRef a -> Widget.Id
fromIRef = Widget.Id . (: []) . bs . guid

collapserId :: Widget.Id -> Widget.Id
collapserId = flip Widget.joinId ["collapser"]

branchSelection :: Widget.Id
branchSelection = Widget.Id ["selected branch"]

delegating :: Widget.Id -> Widget.Id
delegating = flip Widget.joinId ["delegating"]

notDelegating :: Widget.Id -> Widget.Id
notDelegating = flip Widget.joinId ["not delegating"]

goUpId :: Widget.Id
goUpId = Widget.Id ["go up"]

searchTermId :: Widget.Id -> Widget.Id
searchTermId = flip Widget.joinId ["search term"]

parenHighlightId :: Widget.Id
parenHighlightId = Widget.Id ["paren highlight"]