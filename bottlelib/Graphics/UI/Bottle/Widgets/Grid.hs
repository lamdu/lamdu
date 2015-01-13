{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Graphics.UI.Bottle.Widgets.Grid
  ( Grid, KGrid(..)
  , make, makeKeyed, makeAlign, makeCentered
  , unkey
  , Alignment
  , gridMCursor, gridSize, gridContent
  , Element(..)
  , elementAlign, elementRect, elementW
  , Cursor
  , Keys(..), stdKeys
  , toWidget, toWidgetWithKeys
  , toWidgetBiased, toWidgetBiasedWithKeys
  ) where

import           Control.Applicative (liftA2, (<$>))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (join, msum)
import           Data.Foldable (Foldable)
import           Data.Function (on)
import           Data.List (foldl', transpose, find, minimumBy, sortBy, groupBy)
import           Data.List.Utils (index, enumerate2d)
import           Data.MRUMemo (memo)
import           Data.Maybe (isJust, fromMaybe, catMaybes, mapMaybe)
import           Data.Monoid (Monoid(..))
import           Data.Ord (comparing)
import           Data.Traversable (Traversable)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as EventMap
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.ModKey as ModKey
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.Widget (Widget(..), R)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widgets.GridView (Alignment)
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import           Graphics.UI.Bottle.Widgets.StdKeys (DirKeys(..), stdDirKeys)
import qualified Graphics.UI.GLFW as GLFW

type Cursor = Vector2 Int

length2d :: [[a]] -> Vector2 Int
length2d xs = Vector2 (foldl' max 0 . map length $ xs) (length xs)

capCursor :: Vector2 Int -> Vector2 Int -> Vector2 Int
capCursor size = fmap (max 0) . liftA2 min (subtract 1 <$> size)

data NavDests f = NavDests
  { leftOfCursor
  , aboveCursor
  , rightOfCursor
  , belowCursor
  , topCursor
  , leftMostCursor
  , bottomCursor
  , rightMostCursor :: Maybe (Widget.EnterResult f)
  }

mkNavDests :: Widget.Size -> Rect -> [[Widget.MEnter f]] -> Cursor -> NavDests f
mkNavDests widgetSize prevFocalArea mEnterss cursor@(Vector2 cursorX cursorY) = NavDests
  { leftOfCursor    = givePrevFocalArea . reverse $ take cursorX curRow
  , aboveCursor     = givePrevFocalArea . reverse $ take cursorY curColumn
  , rightOfCursor   = givePrevFocalArea $ drop (cursorX+1) curRow
  , belowCursor     = givePrevFocalArea $ drop (cursorY+1) curColumn

  , topCursor       = giveEdge (Vector2 Nothing (Just 0)) $ take (min 1 cursorY) curColumn
  , leftMostCursor  = giveEdge (Vector2 (Just 0) Nothing) $ take (min 1 cursorX) curRow
  , bottomCursor    = giveEdge (Vector2 Nothing (Just 1)) . take 1 . reverse $ drop (cursorY+1) curColumn
  , rightMostCursor = giveEdge (Vector2 (Just 1) Nothing) . take 1 . reverse $ drop (cursorX+1) curRow
  }
  where
    curRow = fromMaybe [] $ index cappedY mEnterss
    curColumn = fromMaybe [] $ index cappedX (transpose mEnterss)
    Vector2 cappedX cappedY = capCursor size cursor
    size = length2d mEnterss

    give rect = fmap ($ Direction.PrevFocalArea rect) . msum
    givePrevFocalArea = give prevFocalArea
    giveEdge edge = give Rect
      { Rect._topLeft =
          liftA2 fromMaybe (Rect._topLeft prevFocalArea) $
          liftA2 (fmap . (*)) widgetSize edge
      , Rect._size =
          liftA2 fromMaybe (Rect._size prevFocalArea) $
          (fmap . fmap) (const 0) edge
      }

data Keys key = Keys
  { keysDir :: DirKeys key
  , keysMoreLeft :: [key]
  , keysMoreRight :: [key]
  , keysLeftMost :: [key]
  , keysRightMost :: [key]
  , keysTop :: [key]
  , keysBottom :: [key]
  } deriving (Functor, Foldable, Traversable)

stdKeys :: Keys ModKey
stdKeys = Keys
  { keysDir = k <$> stdDirKeys
  , keysMoreLeft = [k GLFW.Key'Home]
  , keysMoreRight = [k GLFW.Key'End]
  , keysLeftMost = [ctrlK GLFW.Key'Home]
  , keysRightMost = [ctrlK GLFW.Key'End]
  , keysTop = [k GLFW.Key'PageUp]
  , keysBottom = [k GLFW.Key'PageDown]
  }
  where
    k = ModKey mempty
    ctrlK = ModKey.ctrl

mkNavEventmap ::
  Keys ModKey -> NavDests f -> (Widget.EventHandlers f, Widget.EventHandlers f)
mkNavEventmap Keys{..} navDests = (weakMap, strongMap)
  where
    weakMap = mconcat $ catMaybes
      [ movement "left"       (keysLeft  keysDir) leftOfCursor
      , movement "right"      (keysRight keysDir) rightOfCursor
      , movement "up"         (keysUp    keysDir) aboveCursor
      , movement "down"       (keysDown  keysDir) belowCursor
      , movement "more left"  keysMoreLeft        leftMostCursor
      , movement "more right" keysMoreRight       rightMostCursor
      ]
    strongMap = mconcat $ catMaybes
      [ movement "top"       keysTop       topCursor
      , movement "bottom"    keysBottom    bottomCursor
      , movement "leftmost"  keysLeftMost  leftMostCursor
      , movement "rightmost" keysRightMost rightMostCursor
      ]
    movement dirName events f =
      (EventMap.keyPresses
       events
       (EventMap.Doc ["Navigation", "Move", dirName]) .
       (^. Widget.enterResultEvent)) <$>
      f navDests

getCursor :: [[Widget k]] -> Maybe Cursor
getCursor =
  fmap cursorOf . find (_wIsFocused . snd) . concat . enumerate2d
  where
    cursorOf ((row, column), _) = Vector2 column row

data Element f = Element
  { _elementAlign :: Alignment
  , _elementRect :: Rect
  , _elementW :: Widget f
  }

data KGrid key f = KGrid
  { _gridMCursor :: Maybe Cursor
  , _gridSize :: Widget.Size
  , _gridContent :: [[(key, Element f)]]
  }

Lens.makeLenses ''Element
Lens.makeLenses ''KGrid

type Grid = KGrid ()

makeKeyed :: [[(key, (Alignment, Widget f))]] -> KGrid key f
makeKeyed children = KGrid
  { _gridMCursor = getCursor $ (map . map) (snd . snd) children
  , _gridSize = size
  , _gridContent = content
  }
  where
    (size, content) =
      GridView.makeGeneric translate $
      (map . map) mkSizedKeyedContent children
    mkSizedKeyedContent (key, (alignment, widget)) =
      (alignment, (widget ^. Widget.wSize, (key, widget)))
    translate align rect =
      _2 %~
      Element align rect . Widget.translate (rect ^. Rect.topLeft)

unkey :: [[(Alignment, Widget f)]] -> [[((), (Alignment, Widget f))]]
unkey = (map . map) ((,) ())

make :: [[(Alignment, Widget f)]] -> Grid f
make = makeKeyed . unkey

makeAlign :: Alignment -> [[Widget f]] -> Grid f
makeAlign alignment = make . (map . map) ((,) alignment)

makeCentered :: [[Widget f]] -> Grid f
makeCentered = makeAlign 0.5

helper ::
  Keys ModKey -> (Widget.Size -> [[Widget.MEnter f]] -> Widget.MEnter f) ->
  KGrid key f -> Widget f
helper keys combineEnters (KGrid mCursor size sChildren) =
  combineWs $ (map . map) (^. _2 . elementW) sChildren
  where
    combineWs wss =
      maybe unselectedW makeW mCursor
      where
        framess = (map . map) _wFrame wss
        mEnterss = (map . map) _wMaybeEnter wss
        frame = mconcat $ concat framess
        mEnter = combineEnters size mEnterss

        unselectedW = Widget
          { _wIsFocused = isJust mCursor
          , _wSize = size
          , _wFrame = frame
          , _wMaybeEnter = mEnter
          , _wEventMap = mempty
          , _wFocalArea = Rect 0 size
          }

        makeW cursor@(Vector2 x y) = Widget
          { _wIsFocused = isJust mCursor
          , _wSize = size
          , _wFrame = frame
          , _wMaybeEnter = mEnter
          , _wEventMap = makeEventMap w navDests
          , _wFocalArea = _wFocalArea w
          }
          where
            navDests = mkNavDests size (_wFocalArea w) mEnterss cursor
            w = wss !! y !! x

        makeEventMap w navDests =
          mconcat [strongMap, _wEventMap w, weakMap]
          where
            (weakMap, strongMap) = mkNavEventmap keys navDests

tupleToVector2 :: (a, a) -> Vector2 a
tupleToVector2 (y, x) = Vector2 x y

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn = minimumBy . comparing

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn = groupBy . ((==) `on`)

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f = groupOn f . sortOn f

-- ^ If unfocused, will enters the given child when entered
toWidgetBiasedWithKeys :: Keys ModKey -> Cursor -> KGrid key f -> Widget f
toWidgetBiasedWithKeys keys (Vector2 x y) =
  helper keys $ \size children ->
  maybeOverride children <$> combineMEnters size children
  where
    maybeOverride children enter dir =
      case dir of
      Direction.Outside -> biased
      Direction.PrevFocalArea _ -> biased
      Direction.Point _ -> unbiased
      where
        unbiased = enter dir
        biased = maybe unbiased ($ dir) . join $ index y children >>= index x

toWidgetBiased :: Cursor -> KGrid key f -> Widget f
toWidgetBiased = toWidgetBiasedWithKeys stdKeys

toWidgetWithKeys :: Keys ModKey -> KGrid key f -> Widget f
toWidgetWithKeys keys = helper keys combineMEnters

toWidget :: KGrid key f -> Widget f
toWidget = toWidgetWithKeys stdKeys

combineMEnters :: Widget.Size -> [[Widget.MEnter f]] -> Widget.MEnter f
combineMEnters size children = chooseClosest childEnters
  where
    childEnters =
      mapMaybe indexIntoMaybe .
      concat . (Lens.mapped . Lens.mapped . _1 %~ tupleToVector2) $
      enumerate2d children

    chooseClosest [] = Nothing
    chooseClosest _ = Just byDirection

    byDirection dir =
      minimumOn
      (Vector2.uncurry (+) . abs . modifyDistance .
       distance dirRect . (^. Widget.enterResultRect)) .
      map ($ dir) $ filteredByEdge edge
      where
        removeUninterestingAxis = ((1 - abs (fromIntegral <$> edge)) *)
        (modifyDistance, dirRect) = case dir of
          Direction.Outside -> (id, Rect 0 0)
          Direction.PrevFocalArea x -> (removeUninterestingAxis, x)
          Direction.Point x -> (id, Rect x 0)
        edge = asEdge size dirRect

    distance = (-) `on` (^. Rect.center)

    filteredByEdge = memo $ \(Vector2 hEdge vEdge) ->
      map snd .
      safeHead . groupSortOn ((* (-hEdge)) . (^._1._1)) .
      safeHead . groupSortOn ((* (-vEdge)) . (^._1._2)) $
      childEnters
    indexIntoMaybe (i, m) = (,) i <$> m

safeHead :: Monoid a => [a] -> a
safeHead = mconcat . take 1

asEdge :: Vector2 R -> Rect -> Vector2 Int
asEdge size rect =
  Vector2 hEdge vEdge
  where
    hEdge = boolToInt rightEdge - boolToInt leftEdge
    vEdge = boolToInt bottomEdge - boolToInt topEdge
    boolToInt False = 0
    boolToInt True = 1
    Vector2 leftEdge topEdge =
      (<= 0) <$> (rect ^. Rect.bottomRight)
    Vector2 rightEdge bottomEdge =
      liftA2 (>=) (rect ^. Rect.topLeft) size
