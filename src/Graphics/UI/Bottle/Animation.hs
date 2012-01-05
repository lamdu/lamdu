{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

module Graphics.UI.Bottle.Animation(
  AnimId, Rect(..), PositionedImage(..), Frame(..),
  draw, nextFrame, backgroundColor,
  translate, scale, onDepth,
  simpleFrame, simpleFrameDownscale)
where

import Control.Arrow(first, second)
import Control.Newtype(over)
import Control.Newtype.TH(mkNewTypes)
import Data.ByteString.Char8() -- IsString instance
import Data.List(sortBy)
import Data.Map(Map)
import Data.Monoid(Monoid(..))
import Data.Ord(comparing)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators((%%))
import Graphics.DrawingCombinators.Utils(square)
import qualified Data.ByteString as SBS
import qualified Data.Map as Map
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw

type AnimId = [SBS.ByteString]
type Layer = Int

data Rect = Rect {
  rectTopLeft :: Vector2 Draw.R,
  rectSize :: Vector2 Draw.R
  }

data PositionedImage = PositionedImage {
  piImage :: Draw.Image (), -- Image always occupies (0,0)..(1,1), the translation/scaling occurs when drawing
  piRect :: Rect
  }

newtype Frame = Frame {
  iSubImages :: Map AnimId (Layer, PositionedImage)
  }
$(mkNewTypes [''Frame])

simpleFrame :: AnimId -> Draw.Image () -> Frame
simpleFrame animId image =
  Frame $ Map.singleton animId $ (0, PositionedImage image (Rect 0 1))

simpleFrameDownscale :: AnimId -> Vector2 Draw.R -> Draw.Image () -> Frame
simpleFrameDownscale animId size@(Vector2 w h) =
  scale size .
  simpleFrame animId .
  (Draw.scale (1 / w) (1 / h) %%)

instance Monoid Frame where
  mempty = Frame mempty
  mappend (Frame x) (Frame y) =
    Frame $
    Map.unionWith (error "Attempt to unify same-id sub-images!") x y

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

draw :: Frame -> Draw.Image ()
draw = mconcat . map posImage . map snd . sortOn fst . Map.elems . iSubImages
  where
    posImage (PositionedImage img (Rect { rectTopLeft = Vector2 t l, rectSize = Vector2 w h })) =
      Draw.translate (t, l) %% Draw.scale w h %% img

center :: Rect -> Vector2 Draw.R
center (Rect tl size) = tl + size / 2

sqrNorm :: Num a => Vector2 a -> a
sqrNorm = Vector2.vector2 (+) . (^ (2::Int))

animSpeed :: Fractional a => a
animSpeed = 0.5

nextFrame :: Frame -> Frame -> Frame
nextFrame (Frame dest) (Frame cur) =
  Frame . Map.mapMaybe id $
  mconcat [
    fmap add $ Map.difference dest cur,
    fmap del $ Map.difference cur dest,
    Map.intersectionWith modify dest cur
  ]
  where
    add (layer, PositionedImage img r) =
      Just (layer, PositionedImage img (Rect (center r) 0))
    del (layer, PositionedImage img (Rect pos size))
      | sqrNorm size < 1 = Nothing
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

backgroundColor :: AnimId -> Layer -> Draw.Color -> Vector2 Draw.R -> Frame -> Frame
backgroundColor animId layer color size =
  flip mappend . onDepth (+layer) . scale size . simpleFrame animId $ Draw.tint color square

translate :: Vector2 Draw.R -> Frame -> Frame
translate pos =
  over Frame $ (fmap . second) moveImage
  where
    moveImage (PositionedImage img (Rect tl size)) =
      PositionedImage img (Rect (tl + pos) size)

scale :: Vector2 Draw.R -> Frame -> Frame
scale factor =
  over Frame $ (fmap . second) scaleImage
  where
    scaleImage (PositionedImage img (Rect tl size)) =
      PositionedImage img (Rect (tl * factor) (size * factor))

onDepth :: (Int -> Int) -> Frame -> Frame
onDepth = over Frame . fmap . first
