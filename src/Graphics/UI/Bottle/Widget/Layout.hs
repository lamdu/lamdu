-- | A type-class for operations that work on layouts

-- TODO: Remove and consider moving `pad` to `HasView` class

module Graphics.UI.Bottle.Widget.Layout
    ( Layout(..)
    ) where

import           Data.Vector.Vector2 (Vector2)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widget.Aligned (AlignedWidget)
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget
import           Graphics.UI.Bottle.Widget.TreeLayout (TreeLayout)
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout

class Layout w where
    pad :: Vector2 Widget.R -> w a -> w a

instance Layout TreeLayout where
    pad = TreeLayout.pad

instance Layout Widget where
    pad = Widget.pad

instance Layout AlignedWidget where
    pad = AlignedWidget.pad
