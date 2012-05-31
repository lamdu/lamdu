{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

module Graphics.UI.Bottle.Animation
  ( R, AnimId
  , PositionedImage(..), atPiImage, atPiRect
  , Frame(..), atFSubImages, onImages
  , draw, nextFrame, mapIdentities
  , unitSquare, backgroundColor
  , translate, scale, onDepth
  , simpleFrame, simpleFrameDownscale
  , joinId, subId
  , weaker, stronger)
where

import Control.Applicative(Applicative(..), liftA2)
import Control.Arrow(first, second)
import Control.Monad(void)
import Data.List(isPrefixOf)
import Data.List.Utils(groupOn, sortOn)
import Data.Map(Map, (!))
import Data.Maybe(isJust)
import Data.Monoid(Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators(R, (%%))
import Graphics.UI.Bottle.Rect(Rect(..))
import qualified Data.AtFieldTH as AtFieldTH
import qualified Data.ByteString as SBS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Graphics.UI.Bottle.Rect as Rect

type AnimId = [SBS.ByteString]
type Layer = Int

data PositionedImage = PositionedImage {
  piImage :: Draw.Image (), -- Image always occupies (0,0)..(1,1), the translation/scaling occurs when drawing
  piRect :: Rect
  }
AtFieldTH.make ''PositionedImage

newtype Frame = Frame {
  fSubImages :: Map AnimId [(Layer, PositionedImage)]
  }
AtFieldTH.make ''Frame

joinId :: AnimId -> AnimId -> AnimId
joinId = (++)

subId :: AnimId -> AnimId -> Maybe AnimId
subId folder path
  | folder `isPrefixOf` path = Just $ drop (length folder) path
  | otherwise = Nothing

simpleFrame :: AnimId -> Draw.Image () -> Frame
simpleFrame animId image =
  Frame $ Map.singleton animId [(0, PositionedImage image (Rect 0 1))]

simpleFrameDownscale :: AnimId -> Vector2 R -> Draw.Image () -> Frame
simpleFrameDownscale animId size@(Vector2 w h) =
  scale size .
  simpleFrame animId .
  (Draw.scale (1 / w) (1 / h) %%)

inFrame2
  :: (Map AnimId [(Layer, PositionedImage)]
      -> Map AnimId [(Layer, PositionedImage)]
      -> Map AnimId [(Layer, PositionedImage)])
  -> Frame -> Frame -> Frame
inFrame2 f (Frame x) (Frame y) = Frame (f x y)

stronger :: Frame -> Frame -> Frame
stronger = inFrame2 Map.union

weaker :: Frame -> Frame -> Frame
weaker = flip stronger

instance Monoid Frame where
  mempty = Frame mempty
  mappend = inFrame2 $ Map.unionWith (++)

unitX :: Draw.Image ()
unitX = void $ mconcat
  [ Draw.line (0, 0) (1, 1)
  , Draw.line (1, 0) (0, 1)
  ]

red :: Draw.Color
red = Draw.Color 1 0 0 1

draw :: Frame -> Draw.Image ()
draw = mconcat . map (posImages . map snd) . sortOn (fst . head) . Map.elems . fSubImages
  where
    putXOn (PositionedImage img r) = PositionedImage (mappend (Draw.tint red unitX) img) r
    posImages [x] = posImage x
    posImages xs = mconcat $ map (posImage . putXOn) xs
    posImage (PositionedImage img (Rect { rectTopLeft = Vector2 t l, rectSize = Vector2 w h })) =
      Draw.translate (t, l) %% Draw.scale w h %% img

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
        tl = liftA2 min (Rect.rectTopLeft a) (Rect.rectTopLeft b)
        br = liftA2 max (Rect.bottomRight a) (Rect.bottomRight b)

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
        (fmap abs (Rect.rectTopLeft ra - Rect.rectTopLeft rb))
        (fmap abs (Rect.bottomRight ra - Rect.bottomRight rb))
    rectMap = Map.map (piRect . snd . head)

mapIdentities :: (AnimId -> AnimId) -> Frame -> Frame
mapIdentities = atFSubImages . Map.mapKeys

nextFrame :: R -> Frame -> Frame -> Maybe Frame
nextFrame movement dest cur
  | isVirtuallySame dest cur = Nothing
  | otherwise = Just $ makeNextFrame movement dest cur

makeNextFrame :: R -> Frame -> Frame -> Frame
makeNextFrame movement (Frame dests) (Frame curs) =
  Frame . Map.map (:[]) . Map.mapMaybe id $
  mconcat [
    Map.mapWithKey add $ Map.difference dest cur,
    Map.mapWithKey del $ Map.difference cur dest,
    Map.intersectionWith modify dest cur
  ]
  where
    dest = Map.map head dests
    cur = Map.map head curs
    animSpeed = pure movement
    curPrefixMap = prefixRects cur
    destPrefixMap = prefixRects dest
    add key (layer, PositionedImage img r) =
      Just (layer, PositionedImage img rect)
      where
        rect = maybe (Rect (Rect.center r) 0) genRect $ findPrefix key curPrefixMap
        genRect prefix = relocateSubRect r (destPrefixMap ! prefix) (curPrefixMap ! prefix)
    del key (layer, PositionedImage img (Rect pos size))
      | isJust (findPrefix key destPrefixMap)
      || Vector2.sqrNorm size < 1 = Nothing
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

unitSquare :: AnimId -> Frame
unitSquare animId = simpleFrame animId DrawUtils.square

backgroundColor :: AnimId -> Layer -> Draw.Color -> Vector2 R -> Frame -> Frame
backgroundColor animId layer color size =
  flip mappend . onDepth (+layer) . scale size . onImages (Draw.tint color) $ unitSquare animId

translate :: Vector2 R -> Frame -> Frame
translate pos =
  atFSubImages $ (Map.map . map . second) moveImage
  where
    moveImage (PositionedImage img (Rect tl size)) =
      PositionedImage img (Rect (tl + pos) size)

scale :: Vector2 R -> Frame -> Frame
scale factor =
  atFSubImages $ (Map.map . map . second) scaleImage
  where
    scaleImage (PositionedImage img (Rect tl size)) =
      PositionedImage img (Rect (tl * factor) (size * factor))

onDepth :: (Int -> Int) -> Frame -> Frame
onDepth = atFSubImages . Map.map . map . first

onImages :: (Draw.Image () -> Draw.Image ()) -> Frame -> Frame
onImages = atFSubImages . Map.map . map . second . atPiImage
