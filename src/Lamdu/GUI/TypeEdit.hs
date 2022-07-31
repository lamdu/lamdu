module Lamdu.GUI.TypeEdit
    ( makeScheme
    , removeResponsiveEvents
    ) where

import           GUI.Momentu (Responsive)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Responsive as Responsive
import qualified Lamdu.GUI.TypeParams as TypeParams
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude
import qualified GUI.Momentu.Responsive.Options as ResponsiveOptions

newtype Prec = Prec Int deriving stock (Eq, Ord, Show)

-- TODO: In Momentu if at all?
removeResponsiveEvents :: Responsive f -> Responsive g
removeResponsiveEvents = Responsive.alignedWidget . M.tValue . Widget.wState %~ removeStateEvents

removeStateEvents :: Widget.State (f M.Update) -> Widget.State (g M.Update)
removeStateEvents (Widget.StateUnfocused u) = u & Widget.uMEnter .~ Nothing & Widget.StateUnfocused
removeStateEvents (Widget.StateFocused f) = f <&> removeFocusedEvents & Widget.StateFocused

removeFocusedEvents :: Widget.Focused (f M.Update) -> Widget.Focused (g M.Update)
removeFocusedEvents f =
    f
    { Widget._fMEnterPoint = Nothing
    , Widget._fPreEvents = []
    , Widget._fEventMap = mempty
    }

makeScheme :: _ =>
    o M.WidgetId ->
    M.WidgetId -> -- TODO: should this come form a payload of the scheme expression?
    Sugar.Scheme Name i o -> m (Responsive o)
makeScheme prevDst myId scheme =
    do
        (_addFirstEventMap, paramEdits) <- TypeParams.make (scheme ^. Sugar.schemeForAll) prevDst (Widget.joinId myId ["foralls"])
        ResponsiveOptions.boxSpaced ?? ResponsiveOptions.disambiguationNone ?? paramEdits
