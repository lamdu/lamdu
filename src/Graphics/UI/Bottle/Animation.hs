module Graphics.UI.Bottle.Animation(
  AnimId, Rect(..), PositionedImage(..), Frame(..),
  draw, nextFrame, augmentId, simpleFrame)
where

import Data.ByteString.Char8() -- IsString instance
import Data.Map.Ordered(OrderedMap)
import Data.Monoid(Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators((%%))
import qualified Data.ByteString as SBS
import qualified Data.Map.Ordered as OrderedMap
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw

type AnimId = [SBS.ByteString]

data Rect = Rect {
  rectTopLeft :: Vector2 Draw.R,
  rectSize :: Vector2 Draw.R
  }

data PositionedImage = PositionedImage {
  piImage :: Draw.Image (), -- Image always occupies (0,0)..(1,1), the translation/scaling occurs when drawing
  piRect :: Rect
  }

data Frame = Frame {
  iSubImages :: OrderedMap AnimId PositionedImage
  }

simpleFrame :: Vector2 Draw.R -> Draw.Image () -> Frame
simpleFrame size image =
  Frame $ OrderedMap.singleton mempty $ PositionedImage image (Rect 0 size)

instance Monoid Frame where
  mempty = Frame mempty
  mappend (Frame x) (Frame y) =
    Frame $
    OrderedMap.unionWith (error "Attempt to unify same-id sub-images!") x y

draw :: Frame -> Draw.Image ()
draw = mconcat . map posImage . OrderedMap.elems . iSubImages
  where
    posImage (PositionedImage img (Rect { rectTopLeft = Vector2 t l, rectSize = Vector2 w h })) =
      Draw.translate (t, l) %% Draw.scale w h %% img

center :: Rect -> Vector2 Draw.R
center (Rect tl size) = tl + size / 2

sqrNorm :: Num a => Vector2 a -> a
sqrNorm = Vector2.vector2 (+) . (^ (2::Int))

animSpeed :: Fractional a => a
animSpeed = 0.2

nextFrame :: Frame -> Frame -> Frame
nextFrame (Frame dest) (Frame cur) =
  Frame . OrderedMap.mapMaybe id $
  mconcat [
    fmap add $ OrderedMap.difference dest cur,
    fmap del $ OrderedMap.difference cur dest,
    OrderedMap.intersectionWith modify dest cur
  ]
  where
    add (PositionedImage img r) =
      Just . PositionedImage img $ Rect (center r) 0
    del (PositionedImage img (Rect pos size))
      | sqrNorm size < 1 = Nothing
      | otherwise = Just $ PositionedImage img (Rect (pos + size/2 * animSpeed) (size * (1 - animSpeed)))
    modify
      (PositionedImage destImg (Rect destTopLeft destSize))
      (PositionedImage _ (Rect curTopLeft curSize)) =
      Just $
        PositionedImage destImg
        (Rect
          (animSpeed * destTopLeft + (1 - animSpeed) * curTopLeft)
          (animSpeed * destSize + (1 - animSpeed) * curSize))

augmentId :: AnimId -> Frame -> Frame
augmentId prefix (Frame frame) = Frame $ OrderedMap.mapKeys (mappend prefix) frame
