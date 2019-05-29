-- | Tag Views

module Lamdu.GUI.TagView
    ( make
    , id
    ) where

import qualified Control.Monad.Reader as Reader
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.GUI.NameView as NameView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude hiding (id)

id :: Widget.Id -> Widget.Id
id = (`Widget.joinId` ["view"])

make ::
    ( MonadReader env m, Has TextView.Style env, Element.HasAnimIdPrefix env
    , Has Theme env, Has Dir.Layout env, Has (Texts.Name Text) env
    ) =>
    Sugar.Tag (Name f) -> m (WithTextPos View)
make tag =
    NameView.make (tag ^. Sugar.tagName)
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId =
            tag ^. Sugar.tagInstance
            & WidgetIds.fromEntityId
            & Widget.toAnimId
