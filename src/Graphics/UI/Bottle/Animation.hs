{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

module Graphics.UI.Bottle.Animation(
  R, AnimId,
  Rect(..), atRectTopLeft, atRectSize, bottomRight, center,
  PositionedImage(..), atPiImage, atPiRect,
  Frame(..), onImages,
  draw, nextFrame, mapIdentities, backgroundColor,
  translate, scale, onDepth,
  simpleFrame, simpleFrameDownscale,
  joinId, subId)
where

import Control.Applicative(liftA2)
import Control.Arrow(first, second)
import Control.Newtype(over)
import Control.Newtype.TH(mkNewTypes)
import Data.Function(on)
import Data.List(isPrefixOf)
import Data.Map(Map, (!))
import Data.Maybe(isJust)
import Data.Monoid(Monoid(..))
import Data.Ord(comparing)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators(R, (%%))
import Graphics.DrawingCombinators.Utils(square)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.ByteString as SBS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw

type AnimId = [SBS.ByteString]
type Layer = Int

data Rect = Rect {
  rectTopLeft :: Vector2 R,
  rectSize :: Vector2 R
  } deriving Show
AtFieldTH.make ''Rect

data PositionedImage = PositionedImage {
  piImage :: Draw.Image (), -- Image always occupies (0,0)..(1,1), the translation/scaling occurs when drawing
  piRect :: Rect
  }
AtFieldTH.make ''PositionedImage

newtype Frame = Frame {
  iSubImages :: Map AnimId (Layer, PositionedImage)
  }
$(mkNewTypes [''Frame])

joinId :: AnimId -> AnimId -> AnimId
joinId = (++)

subId :: AnimId -> AnimId -> Maybe AnimId
subId folder path
  | folder `isPrefixOf` path = Just $ drop (length folder) path
  | otherwise = Nothing

simpleFrame :: AnimId -> Draw.Image () -> Frame
simpleFrame animId image =
  Frame $ Map.singleton animId (0, PositionedImage image (Rect 0 1))

simpleFrameDownscale :: AnimId -> Vector2 R -> Draw.Image () -> Frame
simpleFrameDownscale animId size@(Vector2 w h) =
  scale size .
  simpleFrame animId .
  (Draw.scale (1 / w) (1 / h) %%)

instance Monoid Frame where
  mempty = Frame mempty
  mappend (Frame x) (Frame y) =
    Frame $
    Map.unionWithKey (error . ("Attempt to unify same-id sub-images: " ++) . show) x y

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = List.sortBy . comparing

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = List.groupBy ((==) `on` f)

draw :: Frame -> Draw.Image ()
draw = mconcat . map (posImage . snd) . sortOn fst . Map.elems . iSubImages
  where
    posImage (PositionedImage img (Rect { rectTopLeft = Vector2 t l, rectSize = Vector2 w h })) =
      Draw.translate (t, l) %% Draw.scale w h %% img

center :: Rect -> Vector2 R
center (Rect tl size) = tl + size / 2

bottomRight :: Rect -> Vector2 R
bottomRight (Rect tl size) = tl + size

sqrNorm :: Num a => Vector2 a -> a
sqrNorm = Vector2.vector2 (+) . (^ (2::Int))

animSpeed :: Fractional a => a
animSpeed = 0.2

prefixRects :: Map AnimId (Layer, PositionedImage) -> Map AnimId Rect
prefixRects src =
  Map.fromList . filter (not . null . fst) . map perGroup $ groupOn fst $ sortOn fst prefixItems
  where
    perGroup xs =
      (fst (head xs), List.foldl1' joinRects (map snd xs))
    prefixItems = do
      (key, (_, PositionedImage _ rect)) <- Map.toList src
      prefix <- List.inits key
      return (prefix, rect)
    joinRects a b =
      Rect {
        rectTopLeft = tl,
        rectSize = br - tl
      }
      where
        tl = liftA2 min (rectTopLeft a) (rectTopLeft b)
        br = liftA2 max (bottomRight a) (bottomRight b)

findPrefix :: Ord a => [a] -> Map [a] b -> Maybe [a]
findPrefix key dict =
  List.find (`Map.member` dict) . reverse $ List.inits key

relocateSubRect :: Rect -> Rect -> Rect -> Rect
relocateSubRect srcSubRect srcSuperRect dstSuperRect =
  Rect {
    rectTopLeft = rectTopLeft dstSuperRect + sizeRatio * (rectTopLeft srcSubRect - rectTopLeft srcSuperRect),
    rectSize = sizeRatio * rectSize srcSubRect
  }
  where
    sizeRatio = rectSize dstSuperRect / fmap (max 1) (rectSize srcSuperRect)

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
        (fmap abs (rectTopLeft ra - rectTopLeft rb))
        (fmap abs (bottomRight ra - bottomRight rb))
    rectMap = Map.map (piRect . snd)

mapIdentities :: (AnimId -> AnimId) -> Frame -> Frame
mapIdentities = over Frame . Map.mapKeys

nextFrame :: Frame -> Frame -> Maybe Frame
nextFrame dest cur
  | isVirtuallySame dest cur = Nothing
  | otherwise = Just $ makeNextFrame dest cur

makeNextFrame :: Frame -> Frame -> Frame
makeNextFrame (Frame dest) (Frame cur) =
  Frame . Map.mapMaybe id $
  mconcat [
    Map.mapWithKey add $ Map.difference dest cur,
    Map.mapWithKey del $ Map.difference cur dest,
    Map.intersectionWith modify dest cur
  ]
  where
    curPrefixMap = prefixRects cur
    destPrefixMap = prefixRects dest
    add key (layer, PositionedImage img r) =
      Just (layer, PositionedImage img rect)
      where
        rect = maybe (Rect (center r) 0) genRect $ findPrefix key curPrefixMap
        genRect prefix = relocateSubRect r (destPrefixMap ! prefix) (curPrefixMap ! prefix)
    del key (layer, PositionedImage img (Rect pos size))
      | isJust (findPrefix key destPrefixMap)
      || sqrNorm size < 1 = Nothing
      | otherwise = Just (layer, PositionedImage img (Rect (pos + size/2 * animSpeed) (size * (1 - animSpeed))))
    modify
      (layer, PositionedImage destImg (Rect destTopLeft destSize))
      (_, PositionedImage _ (Rect curTopLeft curSize)) =
      Just (
        layer,
        PositionedImage destImg
        (Rect
          (animSpeed * destTopLeft + (1 - animSpeed) * curTopLeft)
          (animSpeed * destSize + (1 - animSpeed) * curSize)))

backgroundColor :: AnimId -> Layer -> Draw.Color -> Vector2 R -> Frame -> Frame
backgroundColor animId layer color size =
  flip mappend . onDepth (+layer) . scale size . simpleFrame animId $ Draw.tint color square

translate :: Vector2 R -> Frame -> Frame
translate pos =
  over Frame $ (fmap . second) moveImage
  where
    moveImage (PositionedImage img (Rect tl size)) =
      PositionedImage img (Rect (tl + pos) size)

scale :: Vector2 R -> Frame -> Frame
scale factor =
  over Frame $ (fmap . second) scaleImage
  where
    scaleImage (PositionedImage img (Rect tl size)) =
      PositionedImage img (Rect (tl * factor) (size * factor))

onDepth :: (Int -> Int) -> Frame -> Frame
onDepth = over Frame . fmap . first

onImages :: (Draw.Image () -> Draw.Image ()) -> Frame -> Frame
onImages = over Frame . Map.map . second . atPiImage
