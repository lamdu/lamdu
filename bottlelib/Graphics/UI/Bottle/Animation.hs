{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}

module Graphics.UI.Bottle.Animation
  ( R, Size, Layer
  , Image(..), iUnitImage, iRect
  , Frame(..), frameImagesMap, unitImages
  , draw, nextFrame, mapIdentities
  , unitSquare, unitHStripedSquare, emptyRectangle
  , backgroundColor
  , translate, scale, layers
  , unitIntoRect
  , simpleFrame, sizedFrame
  , module Graphics.UI.Bottle.Animation.Id
  ) where

import           Control.Applicative (Applicative(..), liftA2)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (void)
import qualified Data.ByteString.Char8 as SBS
import qualified Data.List as List
import           Data.List.Utils (groupOn, sortOn)
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Data.Maybe (isJust)
import           Data.Monoid (Monoid(..))
import           Data.Vector.Vector2 (Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import           Graphics.DrawingCombinators (R, (%%))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import           Graphics.UI.Bottle.Animation.Id
import           Graphics.UI.Bottle.Rect (Rect(Rect))
import qualified Graphics.UI.Bottle.Rect as Rect

type Layer = Int
type Size = Vector2 R

data Image = Image
  { _iLayer :: Layer
  , _iUnitImage :: Draw.Image ()
    -- iUnitImage always occupies (0,0)..(1,1),
    -- the translation/scaling occurs when drawing
  , _iRect :: Rect
  }
Lens.makeLenses ''Image

newtype Frame = Frame
  { _frameImagesMap :: Map AnimId [Image]
  }
Lens.makeLenses ''Frame

{-# INLINE images #-}
images :: Lens.Traversal' Frame Image
images = frameImagesMap . Lens.traversed . Lens.traversed

{-# INLINE layers #-}
layers :: Lens.Traversal' Frame Layer
layers = images . iLayer

{-# INLINE unitImages #-}
unitImages :: Lens.Traversal' Frame (Draw.Image ())
unitImages = images . iUnitImage

simpleFrame :: AnimId -> Draw.Image () -> Frame
simpleFrame animId image =
  Frame $ Map.singleton animId [Image 0 image (Rect 0 1)]

sizedFrame :: AnimId -> Size -> Draw.Image () -> Frame
sizedFrame animId size =
  scale size .
  simpleFrame animId .
  (DrawUtils.scale (1 / size) %%)

instance Monoid Frame where
  mempty = Frame mempty
  mappend (Frame m0) (Frame m1) = Frame $ Map.unionWith (++) m0 m1

unitX :: Draw.Image ()
unitX = void $ mconcat
  [ Draw.line (0, 0) (1, 1)
  , Draw.line (1, 0) (0, 1)
  ]

red :: Draw.Color
red = Draw.Color 1 0 0 1

draw :: Frame -> Draw.Image ()
draw frame =
  frame
  ^. frameImagesMap
  & Map.elems
  <&> markConflicts
  & concat
  <&> posImage
  & sortOn (^. _1) <&> snd
  & mconcat
  where
    redX = Draw.tint red unitX
    markConflicts imgs@(_:_:_) =
      imgs <&> iUnitImage %~ mappend redX
    markConflicts imgs = imgs
    posImage (Image layer img rect) =
      ( layer
      , DrawUtils.translate (rect ^. Rect.topLeft) %%
        DrawUtils.scale (rect ^. Rect.size) %%
        img
      )

prefixRects :: Map AnimId Image -> Map AnimId Rect
prefixRects src =
  Map.fromList . filter (not . null . fst) . map perGroup $ groupOn fst $ sortOn fst prefixItems
  where
    perGroup xs =
      (fst (head xs), List.foldl1' joinRects (map snd xs))
    prefixItems = do
      (key, img) <- Map.toList src
      prefix <- List.inits key
      return (prefix, img ^. iRect)
    joinRects a b =
      Rect {
        Rect._topLeft = tl,
        Rect._size = br - tl
      }
      where
        tl =
          liftA2 min (a ^. Rect.topLeft) (b ^. Rect.topLeft)
        br =
          liftA2 max (a ^. Rect.bottomRight) (b ^. Rect.bottomRight)

findPrefix :: Ord a => [a] -> Map [a] b -> Maybe [a]
findPrefix key dict =
  List.find (`Map.member` dict) . reverse $ List.inits key

relocateSubRect :: Rect -> Rect -> Rect -> Rect
relocateSubRect srcSubRect srcSuperRect dstSuperRect =
  Rect
  { Rect._topLeft =
       dstSuperRect ^. Rect.topLeft +
       sizeRatio *
       (srcSubRect ^. Rect.topLeft -
        srcSuperRect ^. Rect.topLeft)
  , Rect._size = sizeRatio * srcSubRect ^. Rect.size
  }
  where
    sizeRatio =
      dstSuperRect ^. Rect.size /
      fmap (max 1) (srcSuperRect ^. Rect.size)

isVirtuallySame :: Frame -> Frame -> Bool
isVirtuallySame (Frame a) (Frame b) =
  Map.keysSet a == Map.keysSet b &&
  diffRects < equalityThreshold
  where
    equalityThreshold = 0.2
    diffRects =
      maximum . Map.elems $
      Map.intersectionWith subtractRect
        (rectMap a) (rectMap b)
    subtractRect ra rb =
      Vector2.uncurry max $
      liftA2 max
        (abs (ra ^. Rect.topLeft - rb ^. Rect.topLeft))
        (abs (ra ^. Rect.bottomRight - rb ^. Rect.bottomRight))
    rectMap = Map.mapMaybe (^? Lens.traversed . iRect)

mapIdentities :: (AnimId -> AnimId) -> Frame -> Frame
mapIdentities f = frameImagesMap %~ Map.mapKeys f

nextFrame :: R -> Frame -> Frame -> Maybe Frame
nextFrame movement dest cur
  | isVirtuallySame dest cur = Nothing
  | otherwise = Just $ makeNextFrame movement dest cur

makeNextFrame :: R -> Frame -> Frame -> Frame
makeNextFrame movement (Frame dests) (Frame curs) =
  Frame . Map.map (:[]) . Map.mapMaybe id $
  mconcat
  [ Map.mapWithKey add $ Map.difference dest cur
  , Map.mapWithKey del $ Map.difference cur dest
  , Map.intersectionWith modify dest cur
  ]
  where
    dest = Map.map head dests
    cur = Map.map head curs
    animSpeed = pure movement
    curPrefixMap = prefixRects cur
    destPrefixMap = prefixRects dest
    add key destImg =
      destImg & iRect .~ curRect & Just
      where
        destRect = destImg ^. iRect
        curRect =
          findPrefix key curPrefixMap
          & maybe (Rect (destRect ^. Rect.center) 0) genRect
        genRect prefix = relocateSubRect destRect (destPrefixMap ! prefix) (curPrefixMap ! prefix)
    del key curImg
      | isJust (findPrefix key destPrefixMap)
      || Vector2.sqrNorm (curImg ^. iRect . Rect.size) < 1 = Nothing
      | otherwise =
        curImg
        & iRect . Rect.centeredSize *~ (1 - animSpeed)
        & Just
    modify destImg curImg =
      destImg
      & iRect .~
        Rect
        (animSpeed * destTopLeft + (1 - animSpeed) * curTopLeft)
        (animSpeed * destSize    + (1 - animSpeed) * curSize   )
      & Just
      where
        Rect destTopLeft destSize = destImg ^. iRect
        Rect curTopLeft curSize = curImg ^. iRect

unitSquare :: AnimId -> Frame
unitSquare animId = simpleFrame animId DrawUtils.square

emptyRectangle :: Vector2 R -> Vector2 R -> AnimId -> Frame
emptyRectangle (Vector2 fX fY) totalSize@(Vector2 sX sY) animId =
  mconcat
  [ rect 0                      (Vector2 sX fY)
  , rect (Vector2 0 (sY - fY))  (Vector2 sX fY)
  , rect (Vector2 0 fY)         (Vector2 fX (sY - fY*2))
  , rect (Vector2 (sX - fX) fY) (Vector2 fX (sY - fY*2))
  ]
  & sizedFrame animId totalSize
  where
    rect origin size =
      DrawUtils.square
      & (DrawUtils.scale size %%)
      & (DrawUtils.translate origin %%)

-- Size is 1. Built from multiple vertical rectangles
unitHStripedSquare :: Int -> AnimId -> Frame
unitHStripedSquare n animId =
  mconcat
  [ scale (Vector2 (1/hunits) 1) $
    translate (Vector2 pos 0) $
    square i
  | (i, pos) <- zip [0..] [0,2..hunits-1]
  ]
  where
    hunits = fromIntegral n * 2 - 1 -- light/dark unit count
    square i = unitSquare $ animId ++ [SBS.pack (show (i :: Int))]

backgroundColor :: AnimId -> Layer -> Draw.Color -> Vector2 R -> Frame -> Frame
backgroundColor animId layer color size frame =
  unitSquare animId
  & images . iUnitImage %~ Draw.tint color
  & scale size
  & layers +~ layer
  & mappend frame

translate :: Vector2 R -> Frame -> Frame
translate pos = images . iRect . Rect.topLeft +~ pos

scale :: Vector2 R -> Frame -> Frame
scale factor = images . iRect . Rect.topLeftAndSize *~ factor

-- Scale/translate a Unit-sized frame into a given rect
unitIntoRect :: Rect -> Frame -> Frame
unitIntoRect r =
  translate (r ^. Rect.topLeft) .
  scale (r ^. Rect.size)
