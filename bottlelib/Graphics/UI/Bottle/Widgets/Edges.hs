module Graphics.UI.Bottle.Widgets.Edges(
  makeVertical
  ) where

import Control.Lens ((^.))
import Control.Monad (mplus)
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.Widget (Widget(..))
import Graphics.UI.Bottle.Widgets.StdKeys (DirKeys(..), stdDirKeys)
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget

makeVertical :: Widget.Size -> Widget f -> Widget f -> Widget f
makeVertical size top unTranslatedBottom = Widget
  { wIsFocused = wIsFocused top || wIsFocused bottom
  , wSize = size
  , wFrame = wFrame top `mappend` wFrame bottom
  , wMaybeEnter = wMaybeEnter top `mplus` wMaybeEnter bottom
  , wEventMap = eventMap
  , wFocalArea = maybe (Rect 0 0) wFocalArea selectedWidget
  }
  where
    selectedWidget
      | wIsFocused top = Just $ addTo "down" (keysDown stdDirKeys) bottom top
      | wIsFocused bottom = Just $ addTo "up" (keysUp stdDirKeys) top bottom
      | otherwise = Nothing
    mkKeys = map $ EventMap.ModKey EventMap.noMods
    eventMap = maybe mempty wEventMap selectedWidget
    addTo doc ks other me =
      maybe id
      (Widget.weakerEvents . mkEventMap me doc (mkKeys ks))
      (wMaybeEnter other) me
    mkEventMap me doc keys enterOther =
      EventMap.keyPresses keys doc . Widget.enterResultEvent . enterOther .
      Direction.PrevFocalArea $ wFocalArea me
    bottom = Widget.translate (Vector2 0 (max topHeight bottomsTop)) unTranslatedBottom
    topHeight = wSize top ^. Vector2.second
    bottomHeight = wSize unTranslatedBottom ^. Vector2.second
    bottomsTop = size ^. Vector2.second - bottomHeight
