{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative (pure, (<$))
import Data.ByteString.Char8() -- IsString instance
import Data.IORef
import Data.Map(Map)
import Data.Monoid(Monoid(..))
import Data.Vector.Vector2 (Vector2(..))
import qualified Data.Vector.Vector2 as Vector2
import Data.Fixed(mod')
import Graphics.DrawingCombinators((%%))
import Graphics.DrawingCombinators.Utils (drawTextLines)
import qualified Data.ByteString as SBS
import qualified Data.Map as Map
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.MainLoop as MainLoop
import qualified System.Info

type Identity = [SBS.ByteString]

data Rect = Rect {
  rectTopLeft :: Vector2 Draw.R,
  rectSize :: Vector2 Draw.R
  }

data PositionedImage = PositionedImage {
  piImage :: Draw.Image (), -- Image always occupies (0,0)..(1,1), the translation/scaling occurs when drawing
  piRect :: Rect
  }

data Image = Image {
  iSubImages :: Map Identity PositionedImage
  }

instance Monoid Image where
  mempty = Image mempty
  mappend (Image x) (Image y) = Image $ mappend x y

draw :: Image -> Draw.Image ()
draw = mconcat . map posImage . Map.elems . iSubImages
  where
    posImage (PositionedImage img (Rect { rectTopLeft = Vector2 t l, rectSize = Vector2 w h })) =
      Draw.translate (t, l) %% Draw.scale w h %% img

center :: Rect -> Vector2 Draw.R
center (Rect tl size) = tl + size / 2

sqrNorm :: Num a => Vector2 a -> a
sqrNorm = Vector2.vector2 (+) . (^ (2::Int))

animSpeed :: Fractional a => a
animSpeed = 0.2

nextImage :: Image -> Image -> Image
nextImage (Image dest) (Image cur) =
  Image . Map.mapMaybe id $
  mconcat [
    fmap add $ Map.difference dest cur,
    fmap del $ Map.difference cur dest,
    Map.intersectionWith modify dest cur
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

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

red :: Draw.Color
red = Draw.Color 1 0 0 1

exampleImage :: Draw.R -> Draw.Font -> Image
exampleImage x font = Image . Map.fromList $ [
  (["shraga"],
   PositionedImage (() <$ circle)
   (Rect (Vector2 x 0) (Vector2 100 100))),
  (["shraga", "name"],
   PositionedImage (() <$ drawTextLines font 1 ["Shraga"])
   (Rect (Vector2 x 100) (Vector2 20 20))),
  (["yosi"],
   PositionedImage (() <$ red `Draw.tint` circle)
   (Rect (Vector2 (800 - x) 0) (Vector2 200 100))),
  (["yosi", "name"],
   PositionedImage (() <$ red `Draw.tint` drawTextLines font 1 ["Yosi"])
   (Rect (Vector2 (800 - x) 100) (Vector2 20 20)))
  ]
  where
    circle =
      Draw.scale 0.5 0.5 %% Draw.translate (1, 1) %% Draw.circle

main :: IO ()
main = do
  font <- Draw.openFont (defaultFont System.Info.os)
  xRef <- newIORef 100
  imageRef <- newIORef mempty
  let
    mkDestImage = do
      x <- readIORef xRef
      return $ exampleImage x font
    mkImage = do
      dest <- mkDestImage
      prevImage <- readIORef imageRef
      let image = nextImage dest prevImage
      writeIORef imageRef image
      return $ draw image
    eventHandler = modifyIORef xRef ((`mod'` 1000) . (+100))
  MainLoop.mainLoop ((pure . pure) eventHandler) (pure mkImage)
