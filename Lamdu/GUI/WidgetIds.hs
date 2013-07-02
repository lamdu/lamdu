{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.WidgetIds ( module Lamdu.GUI.WidgetIds, module Lamdu.GUI.WidgetIdIRef ) where

import Data.ByteString.Char8() -- IsString instance
import Data.Monoid(mappend)
import Data.Store.IRef(IRef)
import Graphics.UI.Bottle.Animation (AnimId)
import Lamdu.GUI.WidgetIdIRef
import System.Random.Utils (randFunc)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

hash :: Show a => a -> Widget.Id
hash = fromGuid . randFunc . show

backgroundCursorId :: AnimId
backgroundCursorId = ["background cursor"]

textCursorId :: AnimId
textCursorId = ["text cursor"]

collapserId :: Widget.Id -> Widget.Id
collapserId = flip Widget.joinId ["collapser"]

branchSelection :: Widget.Id
branchSelection = Widget.Id ["selected branch"]

goUpId :: Widget.Id
goUpId = Widget.Id ["go up"]

builtinFFIPath :: Widget.Id -> Widget.Id
builtinFFIPath = flip Widget.joinId ["FFIPath"]

builtinFFIName :: Widget.Id -> Widget.Id
builtinFFIName = flip Widget.joinId ["FFIName"]

searchTermId :: Widget.Id -> Widget.Id
searchTermId = flip Widget.joinId ["search term"]

parenHighlightId :: AnimId
parenHighlightId = ["paren highlight"]

parensPrefix :: AnimId -> AnimId
parensPrefix = flip mappend ["parens"]

diveIn :: Functor f => f (IRef t a) -> f Widget.Id
diveIn = fmap $ FocusDelegator.delegatingId . fromIRef

underlineId :: AnimId -> AnimId
underlineId = flip mappend ["underline"]

activeDefBackground :: AnimId
activeDefBackground = ["active def bg"]

flyNav :: AnimId
flyNav = ["flyNav"]
