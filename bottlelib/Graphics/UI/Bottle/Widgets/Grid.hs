{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Graphics.UI.Bottle.Widgets.Grid
  ( Grid, KGrid(..)
  , make, makeKeyed, makeAlign, makeCentered
  , unkey
  , Alignment
  , gridMCursor, gridSize, gridContent
  , Element
  , elementAlign, elementRect, elementOriginalWidget
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
import           Data.List (foldl', transpose, find)
import           Data.List.Utils (index, groupOn, sortOn, minimumOn)
import           Data.MRUMemo (memo)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Monoid(..), (<>))
import           Data.Traversable (Traversable)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.UI.Bottle.Direction as Direction
import qualified Graphics.UI.Bottle.EventMap as EventMap
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.ModKey as ModKey
import           Graphics.UI.Bottle.Rect (Rect(..))
import qualified Graphics.UI.Bottle.Rect as Rect
import           Graphics.UI.Bottle.View (View(..))
import           Graphics.UI.Bottle.Widget (R, Widget(..), wEventMap, wFocalArea)
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

mkNavDests :: Widget.Size -> Rect -> [[Maybe (Widget.Enter f)]] -> Cursor -> NavDests f
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

addNavEventmap ::
  Keys ModKey -> NavDests f ->
  Widget.EventHandlers f ->
  Widget.EventHandlers f
addNavEventmap Keys{..} navDests eventMap =
  strongMap <> eventMap <> weakMap
  where
    weakMap =
      [ movement "left"       (keysLeft  keysDir) leftOfCursor
      , movement "right"      (keysRight keysDir) rightOfCursor
      , movement "up"         (keysUp    keysDir) aboveCursor
      , movement "down"       (keysDown  keysDir) belowCursor
      , movement "more left"  keysMoreLeft        leftMostCursor
      , movement "more right" keysMoreRight       rightMostCursor
      ] ^. Lens.traverse . Lens._Just
    strongMap =
      [ movement "top"       keysTop       topCursor
      , movement "bottom"    keysBottom    bottomCursor
      , movement "leftmost"  keysLeftMost  leftMostCursor
      , movement "rightmost" keysRightMost rightMostCursor
      ] ^. Lens.traverse . Lens._Just
    movement dirName events f =
      (EventMap.keyPresses
       events
       (EventMap.Doc ["Navigation", "Move", dirName]) .
       (^. Widget.enterResultEvent)) <$>
      f navDests

enumerate2d :: [[a]] -> [(Vector2 Int, a)]
enumerate2d xss =
  xss ^@.. Lens.traversed <.> Lens.traversed
  <&> _1 %~ uncurry (flip Vector2)

index2d :: [[a]] -> Vector2 Int -> a
index2d xss (Vector2 x y) = xss !! y !! x

getCursor :: [[Widget k]] -> Maybe Cursor
getCursor widgets =
  widgets
  & enumerate2d
  & find (_wIsFocused . snd)
  <&> fst

data Element f = Element
  { __elementAlign :: Alignment
  , __elementRect :: Rect
  , __elementOriginalWidget :: Widget f
  }

data KGrid key f = KGrid
  { __gridMCursor :: Maybe Cursor
  , __gridSize :: Widget.Size
  , __gridContent :: [[(key, Element f)]]
  }

Lens.makeLenses ''Element
Lens.makeLenses ''KGrid

{-# INLINE elementAlign #-}
elementAlign :: Lens.Getter (Element f) Alignment
elementAlign = _elementAlign

{-# INLINE elementRect #-}
elementRect :: Lens.Getter (Element f) Rect
elementRect = _elementRect

{-# INLINE elementOriginalWidget #-}
elementOriginalWidget :: Lens.Getter (Element f) (Widget f)
elementOriginalWidget = _elementOriginalWidget

{-# INLINE gridMCursor #-}
gridMCursor :: Lens.Getter (KGrid key f) (Maybe Cursor)
gridMCursor = _gridMCursor

{-# INLINE gridSize #-}
gridSize :: Lens.Getter (KGrid key f) Widget.Size
gridSize = _gridSize

{-# INLINE gridContent #-}
gridContent :: Lens.Getter (KGrid key f) [[(key, Element f)]]
gridContent = _gridContent

type Grid = KGrid ()

makeKeyed :: [[(key, (Alignment, Widget f))]] -> KGrid key f
makeKeyed children = KGrid
  { __gridMCursor = getCursor $ (map . map) (snd . snd) children
  , __gridSize = size
  , __gridContent = content
  }
  where
    (size, content) =
      children
      & Lens.mapped . Lens.mapped %~ toTriplet
      & GridView.makePlacements
      & _2 . Lens.mapped . Lens.mapped %~ toElement
    toTriplet (key, (alignment, widget)) =
      (alignment, widget ^. Widget.wSize, (key, widget))
    toElement (alignment, rect, (key, widget)) =
      (key, Element alignment rect widget)

unkey :: [[(Alignment, Widget f)]] -> [[((), (Alignment, Widget f))]]
unkey = (map . map) ((,) ())

make :: [[(Alignment, Widget f)]] -> Grid f
make = makeKeyed . unkey

makeAlign :: Alignment -> [[Widget f]] -> Grid f
makeAlign alignment = make . (map . map) ((,) alignment)

makeCentered :: [[Widget f]] -> Grid f
makeCentered = makeAlign 0.5

toWidgetCommon ::
  Keys ModKey -> (Widget.Size -> [[Maybe (Widget.Enter f)]] -> Maybe (Widget.Enter f)) ->
  KGrid key f -> Widget f
toWidgetCommon keys combineEnters (KGrid mCursor size sChildren) =
  Widget
  { _wIsFocused = Lens.has Lens._Just mCursor
  , _wView = View size frame
  , _wMaybeEnter = combineEnters size mEnterss
  , _wEventMap = eventMap
  , _wFocalArea = focalArea
  }
  where
    frame = widgets ^. Lens.traverse . Lens.traverse . Widget.wAnimFrame
    translateChildWidget (_key, Element _align rect widget) =
      Widget.translate (rect ^. Rect.topLeft) widget
    widgets =
      sChildren & Lens.mapped . Lens.mapped %~ translateChildWidget
    mEnterss = widgets & Lens.mapped . Lens.mapped %~ _wMaybeEnter
    (eventMap, focalArea) =
      case mCursor of
      Nothing -> (mempty, Rect 0 0)
      Just cursor ->
        ( selectedWidget ^. wEventMap & addNavEventmap keys navDests
        , selectedWidget ^. wFocalArea
        )
        where
          selectedWidget = index2d widgets cursor
          navDests = mkNavDests size (selectedWidget ^. wFocalArea) mEnterss cursor

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f = groupOn f . sortOn f

-- ^ If unfocused, will enters the given child when entered
toWidgetBiasedWithKeys :: Keys ModKey -> Cursor -> KGrid key f -> Widget f
toWidgetBiasedWithKeys keys (Vector2 x y) =
  toWidgetCommon keys $ \size children ->
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
toWidgetWithKeys keys = toWidgetCommon keys combineMEnters

toWidget :: KGrid key f -> Widget f
toWidget = toWidgetWithKeys stdKeys

combineMEnters :: Widget.Size -> [[Maybe (Widget.Enter f)]] -> Maybe (Widget.Enter f)
combineMEnters size children = chooseClosest childEnters
  where
    childEnters =
        (enumerate2d children <&> Lens.sequenceAOf _2)
        ^.. Lens.traverse . Lens._Just

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
