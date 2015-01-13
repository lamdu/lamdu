module Graphics.UI.Bottle.Widgets.Edges(
  makeVertical
  ) where

import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (mplus)
import           Data.List.Utils (minimumOn)
import           Data.Monoid (Monoid(..), (<>))
import           Data.Vector.Vector2 (Vector2(..))
import           Graphics.UI.Bottle.Direction (Direction)
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as EventMap
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.View (View(..))
import           Graphics.UI.Bottle.Widget (Widget(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widgets.StdKeys (DirKeys(..), stdDirKeys)

choose ::
  Widget.EnterResult f -> Widget.EnterResult f ->
  Direction -> Widget.EnterResult f
choose x _ Direction.Outside = x
choose x y (Direction.PrevFocalArea rect) = chooseRect x y rect
choose x y (Direction.Point pt) = chooseRect x y $ Rect pt 0

chooseRect :: Widget.EnterResult f -> Widget.EnterResult f -> Rect -> Widget.EnterResult f
chooseRect x y rect =
  minimumOn (Rect.distance rect . (^. Widget.enterResultRect)) [x, y]

makeVertical :: Widget.Size -> Widget f -> Widget f -> Widget f
makeVertical size top unTranslatedBottom = Widget
  { _wIsFocused = _wIsFocused top || _wIsFocused bottom
  , _wView = View size $ top ^. Widget.wAnimFrame <> bottom ^. Widget.wAnimFrame
  , _wMaybeEnter = mEnter (_wMaybeEnter top) (_wMaybeEnter bottom)
  , _wEventMap = eventMap
  , _wFocalArea = maybe (Rect 0 0) _wFocalArea selectedWidget
  }
  where
    mEnter (Just enterTop) (Just enterBottom) =
      Just $ \dir -> choose (enterTop dir) (enterBottom dir) dir
    mEnter x y = x `mplus` y
    selectedWidget
      | _wIsFocused top = Just $ addTo (EventMap.Doc ["Navigation", "Move", "down"]) (keysDown stdDirKeys) bottom top
      | _wIsFocused bottom = Just $ addTo (EventMap.Doc ["Navigation", "Move", "up"]) (keysUp stdDirKeys) top bottom
      | otherwise = Nothing
    mkKeys = map (ModKey mempty)
    eventMap = maybe mempty _wEventMap selectedWidget
    addTo doc ks other me =
      maybe id
      (Widget.weakerEvents . mkEventMap me doc (mkKeys ks))
      (_wMaybeEnter other) me
    mkEventMap me doc keys enterOther =
      EventMap.keyPresses keys doc . (^. Widget.enterResultEvent) . enterOther .
      Direction.PrevFocalArea $ _wFocalArea me
    bottom = Widget.translate (Vector2 0 (max topHeight bottomsTop)) unTranslatedBottom
    topHeight = top ^. Widget.wHeight
    bottomHeight = unTranslatedBottom ^. Widget.wHeight
    bottomsTop = size ^. _2 - bottomHeight
