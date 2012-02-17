{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.Bottle.Widgets.Grid(
  Grid, KGrid(..), make, makeKeyed, unkey, getElement,
  atGridMCursor,
  atGridContent,
  GridElement(..),
  atGridElementRect,
  atGridElementUio,
  Cursor, toWidget, toWidgetBiased)
where

import Control.Applicative (liftA2)
import Control.Arrow (second)
import Control.Monad (msum, (>=>))
import Data.List (foldl', transpose, find, minimumBy)
import Data.List.Utils (index, enumerate2d)
import Data.Maybe (isJust, fromMaybe, mapMaybe, catMaybes)
import Data.Monoid (mempty, mconcat)
import Data.Ord (comparing)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Rect (Rect(..))
import Graphics.UI.Bottle.SizeRange (Size)
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget(..), UserIO(..))
import qualified Data.AtFieldTH as AtFieldTH
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.Sized as Sized
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.GLFW as GLFW

type Cursor = Vector2 Int

length2d :: [[a]] -> Vector2 Int
length2d xs = Vector2 (foldl' max 0 . map length $ xs) (length xs)

capCursor :: Vector2 Int -> Vector2 Int -> Vector2 Int
capCursor size = fmap (max 0) . liftA2 min (fmap (subtract 1) size)

mkNavEventmap :: [[Widget.MEnter f]] -> Size -> Rect -> Cursor -> (Widget.EventHandlers f, Widget.EventHandlers f)
mkNavEventmap mEnterChildren widgetSize curRect cursor@(Vector2 cursorX cursorY) = (weakMap, strongMap)
  where
    weakMap = mconcat . catMaybes $ [
      movement "left"       (k GLFW.KeyLeft)  leftOfCursor,
      movement "right"      (k GLFW.KeyRight) rightOfCursor,
      movement "up"         (k GLFW.KeyUp)    aboveCursor,
      movement "down"       (k GLFW.KeyDown)  belowCursor,
      edgeMovement (Vector2 (Just 0) Nothing) "more left" (k GLFW.KeyHome) leftMostCursor,
      edgeMovement (Vector2 (Just 1) Nothing) "more right" (k GLFW.KeyEnd) rightMostCursor
      ]
    strongMap = mconcat . catMaybes $ [
      edgeMovement (Vector2 Nothing (Just 0)) "top"       (k GLFW.KeyPageup)   topCursor,
      edgeMovement (Vector2 Nothing (Just 1)) "bottom"    (k GLFW.KeyPagedown) bottomCursor,
      edgeMovement (Vector2 (Just 0) Nothing) "leftmost"  (ctrlK GLFW.KeyHome) leftMostCursor,
      edgeMovement (Vector2 (Just 1) Nothing) "rightmost" (ctrlK GLFW.KeyEnd)  rightMostCursor
      ]
    k = EventMap.KeyEventType EventMap.noMods
    ctrlK = EventMap.KeyEventType EventMap.ctrl
    size = length2d mEnterChildren
    Vector2 cappedX cappedY = capCursor size cursor
    movementHelper rect dirName event =
      fmap
        (EventMap.fromEventType
         event
         ("Move " ++ dirName) .
         Widget.enterResultEvent .
         ($ Direction.RelativePos rect)) .
      msum
    movement = movementHelper curRect
    edgeMovement edge =
      movementHelper
      Rect {
        Rect.rectTopLeft =
          liftA2 fromMaybe (Rect.rectTopLeft curRect) $
            liftA2 (fmap . (*)) widgetSize edge,
        Rect.rectSize =
          liftA2 fromMaybe (Rect.rectSize curRect) $
            (fmap . fmap) (const 0) edge
        }
    leftOfCursor    = reverse $ take cursorX curRow
    aboveCursor     = reverse $ take cursorY curColumn
    rightOfCursor   = drop (cursorX+1) curRow
    belowCursor     = drop (cursorY+1) curColumn
    topCursor       = take (min 1 cursorY) curColumn
    leftMostCursor  = take (min 1 cursorX) curRow
    bottomCursor    = take 1 . reverse $ drop (cursorY+1) curColumn
    rightMostCursor = take 1 . reverse $ drop (cursorX+1) curRow
    curRow          = fromMaybe [] $ index cappedY mEnterChildren
    curColumn       = fromMaybe [] $ index cappedX (transpose mEnterChildren)

getCursor :: [[Widget k]] -> Maybe Cursor
getCursor =
  fmap cursorOf . find (isFocused . snd) . concat . enumerate2d
  where
    cursorOf ((row, column), _) = Vector2 column row

data GridElement f = GridElement {
  gridElementRect :: Rect,
  gridElementUio :: UserIO f
  }

data KGrid key f = KGrid {
  gridMCursor :: Maybe Cursor,
  gridContent :: Sized [[(key, GridElement f)]]
  }

AtFieldTH.make ''GridElement
AtFieldTH.make ''KGrid

type Grid = KGrid ()

makeKeyed :: [[(key, Widget f)]] -> KGrid key f
makeKeyed children = KGrid {
  gridMCursor = getCursor $ (map . map) snd children,
  gridContent =
   GridView.makeGeneric (second . translate) $
   (map . map) (keyIntoSized . second Widget.content) children
  }
  where
    keyIntoSized (key, sized) = fmap ((,) key) sized
    translate rect =
      GridElement rect .
      Widget.translateUserIO (Rect.rectTopLeft rect)

unkey :: [[Widget f]] -> [[((), Widget f)]]
unkey = (map . map) ((,) ())

getElement :: (Show key, Eq key) => key -> [[(key, GridElement f)]] -> GridElement f
getElement key =
  fromMaybe (error errorMsg) . lookup key . concat
  where
    errorMsg = "getElement: " ++ show key ++ " not found in Grid!"

make :: [[Widget f]] -> Grid f
make = makeKeyed . unkey

helper ::
  (Size -> [[Widget.MEnter f]] -> Widget.MEnter f) ->
  KGrid key f -> Widget f
helper combineEnters (KGrid mCursor sChildren) =
  Widget {
    isFocused = isJust mCursor,
    content =
      Sized.atFromSize combineUserIOs $
      (fmap . map . map) snd sChildren
    }
  where
    combineUserIOs mkUserIOss size =
      maybe unselectedUserIO makeUserIO mCursor
      where
        userIOss = (map . map) gridElementUio $ mkUserIOss size
        frame = mconcat . map uioFrame $ concat userIOss
        mEnterss = (map . map) uioMaybeEnter userIOss
        mEnter = combineEnters size mEnterss

        unselectedUserIO = UserIO {
          uioFrame = frame,
          uioMaybeEnter = mEnter,
          uioEventMap = mempty,
          uioFocalArea = Rect 0 size
          }

        makeUserIO cursor@(Vector2 x y) = UserIO {
          uioFrame = frame,
          uioMaybeEnter = Nothing, -- We're already entered
          uioEventMap = makeEventMap cursor userIO,
          uioFocalArea = uioFocalArea userIO
          }
          where
            userIO = userIOss !! y !! x

        makeEventMap cursor userIO =
          mconcat [strongMap, uioEventMap userIO, weakMap]
          where
            (weakMap, strongMap) = mkNavEventmap mEnterss size (uioFocalArea userIO) cursor

toWidget :: KGrid key f -> Widget f
toWidget =
  helper makeMEnter
  where
    makeMEnter size children =
      search . mapMaybe indexIntoMaybe . concat $ enumerate2d children
      where
        indexIntoMaybe (i, m) = fmap ((,) i) m
        search [] = Nothing
        search childEnters = Just $ byDirection childEnters
        byDirection childEnters dir =
          (snd . minimumOn fst .
           (map . scoredEnter dir . Direction.fold (Rect 0 0) id) dir) childEnters dir

        -- TODO: Take this out to a generic location
        rectScore entryRect (row, col) enterResultRect =
          (borderScore, Rect.distance entryRect enterResultRect)
          where
            borderScore =
              concat [[col | fromLeft], [-col | fromRight],
                      [row | fromTop], [-row | fromBottom]]
            Vector2 fromLeft fromTop = fmap (<= 0) (Rect.bottomRight entryRect)
            Vector2 fromRight fromBottom = liftA2 (>=) (Rect.rectTopLeft entryRect) size

        scoredEnter dir entryRect (i, childEnter) =
          ((rectScore entryRect i . Widget.enterResultRect . childEnter) dir, childEnter)

        minimumOn = minimumBy . comparing

-- ^ If unfocused, will enters the given child when entered
toWidgetBiased :: Cursor -> KGrid key f -> Widget f
toWidgetBiased (Vector2 x y) =
  helper . const $ index y >=> index x >=> id
