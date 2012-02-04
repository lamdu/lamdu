{-# OPTIONS -Wall #-}
module Graphics.UI.Bottle.Widgets.Grid(Cursor, make, makeBiased) where

import Control.Applicative (liftA2)
import Control.Monad (msum, (>=>))
import Data.List (foldl', transpose, find, minimumBy)
import Data.List.Utils (index, enumerate2d)
import Data.Maybe (isJust, fromMaybe, catMaybes)
import Data.Monoid (mempty, mconcat)
import Data.Ord (comparing)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.SizeRange (Coordinate)
import Graphics.UI.Bottle.Widget (Widget(..), UserIO(..))
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Sized as Sized
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.GLFW as GLFW

type Cursor = Vector2 Int

length2d :: [[a]] -> Vector2 Int
length2d xs = Vector2 (foldl' max 0 . map length $ xs) (length xs)

capCursor :: Vector2 Int -> Vector2 Int -> Vector2 Int
capCursor size = fmap (max 0) . liftA2 min (fmap (subtract 1) size)

mkNavEventmap :: [[Widget.MEnter f]] -> Coordinate -> Cursor -> (Widget.EventHandlers f, Widget.EventHandlers f)
mkNavEventmap mEnterChildren curPos cursor@(Vector2 cursorX cursorY) = (weakMap, strongMap)
  where
    weakMap = mconcat . catMaybes $ [
      movement "left"       (k GLFW.KeyLeft)  leftOfCursor,
      movement "right"      (k GLFW.KeyRight) rightOfCursor,
      movement "up"         (k GLFW.KeyUp)    aboveCursor,
      movement "down"       (k GLFW.KeyDown)  belowCursor,
      movement "more left"  (k GLFW.KeyHome)  leftMostCursor,
      movement "more right" (k GLFW.KeyEnd)   rightMostCursor
      ]
    strongMap = mconcat . catMaybes $ [
      movement "top"       (k GLFW.KeyPageup)   topCursor,
      movement "bottom"    (k GLFW.KeyPagedown) bottomCursor,
      movement "leftmost"  (ctrlK GLFW.KeyHome) leftMostCursor,
      movement "rightmost" (ctrlK GLFW.KeyEnd)  rightMostCursor
      ]
    k = EventMap.KeyEventType EventMap.noMods
    ctrlK = EventMap.KeyEventType EventMap.ctrl
    size = length2d mEnterChildren
    Vector2 cappedX cappedY = capCursor size cursor
    movement dirName event =
      fmap
        (EventMap.fromEventType
         event
         ("Move " ++ dirName) .
         Widget.enterResultEvent .
         ($ Widget.RelativePos curPos)) .
      msum
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

-- makeHelper :: Bool -> Cursor -> Cursor -> [[Widget k]] -> Widget k
-- makeHelper isFocused =

getCursor :: [[Widget k]] -> Maybe Cursor
getCursor =
  fmap cursorOf . find (isFocused . snd) . concat . enumerate2d
  where
    cursorOf ((row, column), _) = Vector2 column row

makeHelper ::
  ([[Widget.MEnter f]] -> Widget.MEnter f) ->
  [[Widget f]] -> Widget f
makeHelper combineEnters children =
  Widget {
    isFocused = isJust mCursor,
    content =
      Sized.atFromSize combineUserIOs .
      GridView.makeGeneric Widget.translateUserIO .
      (map . map) content $ children
    }
  where
    mCursor = getCursor children
    combineUserIOs mkUserIOss size =
      maybe unselectedUserIO makeUserIO mCursor
      where
        userIOss = mkUserIOss size
        frame = mconcat . map uioFrame $ concat userIOss
        mEnterss = (map . map) uioMaybeEnter userIOss
        mEnter = combineEnters mEnterss

        unselectedUserIO = UserIO {
          uioFrame = frame,
          uioMaybeEnter = mEnter,
          uioEventMap = mempty,
          uioFocalArea = Anim.Rect 0 size
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
            pos = Anim.center $ uioFocalArea userIO
            (weakMap, strongMap) = mkNavEventmap mEnterss pos cursor

-- ^ If unfocused, will enters the given child when entered
makeBiased :: Cursor -> [[Widget k]] -> Widget k
makeBiased (Vector2 x y) = makeHelper $ index y >=> index x >=> id

sqrDistance :: Num a => Vector2 a -> Vector2 a -> a
sqrDistance v1 v2 = Vector2.uncurry (+) $ (v1 - v2) ^ (2 :: Int)

make :: [[Widget k]] -> Widget k
make = makeHelper makeEnter
  where
    makeEnter = search . catMaybes . concat
      where
        search [] = Nothing
        search childEnters = Just $ byDirection childEnters
        byDirection childEnters dir =
          (snd . minimumOn fst . (map . distanceEnter dir . Widget.direction 0 id) dir) childEnters $ dir

        distanceEnter dir entryPos childEnter =
          ((sqrDistance entryPos . Anim.center . Widget.enterResultRect . childEnter) dir, childEnter)

        minimumOn = minimumBy . comparing
