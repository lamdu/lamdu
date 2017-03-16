-- | A type-class for operations that work on layouts

module Graphics.UI.Bottle.Widget.Layout
    ( Layout(..)
    ) where

import qualified Control.Lens as Lens
import           Data.Vector.Vector2 (Vector2)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widget.Aligned (AlignedWidget)
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget
import           Graphics.UI.Bottle.Widget.TreeLayout (TreeLayout)
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout

class Layout w where
    widget :: Lens.Setter (w a) (w b) (Widget a) (Widget b)
    pad :: Vector2 Widget.R -> w a -> w a
    empty :: w a

instance Layout TreeLayout where
    widget = TreeLayout.widget
    pad = TreeLayout.pad
    empty = TreeLayout.empty

instance Layout Widget where
    widget = id
    pad = Widget.pad
    empty = Widget.empty

instance Layout AlignedWidget where
    widget = AlignedWidget.widget
    pad = AlignedWidget.pad
    empty = AlignedWidget.empty
