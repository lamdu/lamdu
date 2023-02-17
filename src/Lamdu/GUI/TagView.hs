-- | Tag Views

module Lamdu.GUI.TagView
    ( make
    ) where

import qualified GUI.Momentu as M
import qualified Lamdu.GUI.NameView as NameView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

make :: _ => Sugar.Tag Name -> m (M.WithTextPos M.View)
make tag =
    NameView.make (tag ^. Sugar.tagName)
    & local (M.elemIdPrefix .~ elemId)
    where
        elemId =
            tag ^. Sugar.tagInstance
            & WidgetIds.fromEntityId
            & M.asElemId
