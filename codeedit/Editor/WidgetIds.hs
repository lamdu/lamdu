{-# LANGUAGE OverloadedStrings #-}
module Editor.WidgetIds(
  backgroundCursorId, textCursorId, fromIRef, fromGuid,
  collapserId, branchSelection, goUpId,
  delegating, notDelegating, searchTermId,
  parenHighlightId,
  parensPrefix,
  paramId, varId, diveIn)
where

import Data.ByteString.Char8() -- IsString instance
import Data.Store.Guid(Guid, bs)
import Data.Store.IRef(IRef, guid)
import qualified Editor.Data as Data
import qualified Graphics.UI.Bottle.Widget as Widget

backgroundCursorId :: Widget.Id
backgroundCursorId = Widget.Id ["background cursor"]

textCursorId :: Widget.Id
textCursorId = Widget.Id ["text cursor"]

fromIRef :: IRef a -> Widget.Id
fromIRef = fromGuid . guid

fromGuid :: Guid -> Widget.Id
fromGuid = Widget.Id . (: []) . bs

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

parensPrefix :: Widget.Id -> Widget.Id
parensPrefix = flip Widget.joinId ["parens"]

varId :: Data.VariableRef -> Widget.Id
varId = Data.onVariableIRef fromIRef

diveIn :: Functor f => f (IRef a) -> f Widget.Id
diveIn = fmap $ delegating . fromIRef

paramId :: Guid -> Widget.Id
paramId x = Widget.joinId (fromGuid x) ["param"]
