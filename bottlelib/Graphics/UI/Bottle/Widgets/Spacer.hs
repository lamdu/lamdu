module Graphics.UI.Bottle.Widgets.Spacer
  ( make
  , makeWidget
  , makeHorizontal, makeVertical
  , makeHorizontalWidget
  , makeHorizLine
  ) where

import           Data.Monoid (mempty)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Graphics.UI.Bottle.Animation as Anim
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget

make :: View.Size -> View
make size = View size mempty

makeWidget :: Widget.Size -> Widget f
makeWidget = Widget.fromView . make

makeHorizontal :: Anim.R -> View
makeHorizontal width = make $ Vector2 width 0

makeVertical :: Anim.R -> View
makeVertical height = make $ Vector2 0 height

makeHorizontalWidget :: Widget.R -> Widget f
makeHorizontalWidget = Widget.fromView . makeHorizontal

horizLineFrame :: Anim.AnimId -> Widget.Size -> Anim.Frame
horizLineFrame animId size@(Vector2 w h) =
    Anim.sizedFrame animId size $ DrawUtils.line (0, h/2) (w, h/2)

makeHorizLine :: Anim.AnimId -> Widget.Size -> Widget f
makeHorizLine animId size = Widget.fromView $ View size $ horizLineFrame animId size
