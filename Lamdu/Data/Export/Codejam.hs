{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Data.Export.Codejam
    ( exportFancy
    ) where

import           Codec.Picture (Image, PixelRGB8(..), withImage, writePng)
import           Control.Lens.Operators
import           Data.Store.Transaction (Transaction)
import qualified Foreign as F
import           Lamdu.Data.DbLayout (ViewM)
import qualified Graphics.Rendering.OpenGL as GL

import           Prelude.Compat

type T = Transaction

takeScreenshot :: IO (Image PixelRGB8)
takeScreenshot =
    do
        (pos, size@(GL.Size wGl hGl)) <- GL.get GL.viewport
        let width = fromIntegral wGl
        let height = fromIntegral hGl
        let pixelSize = 3
        -- glY converts top-origin coordinates to OpenGL's bottom-origin system
        let glY y = height - 1 - y
        let pixelOffset x y = ((glY y) * width + x) * pixelSize
        F.allocaBytes (pixelSize * width * height) $
            \ptr ->
            do
                GL.readPixels pos size (GL.PixelData GL.RGB GL.UnsignedByte ptr)
                let readPixel x y =
                        F.plusPtr ptr (pixelOffset x y)
                        & F.peekArray pixelSize
                        <&> (\[r, g, b] -> PixelRGB8 r g b)
                withImage width height readPixel

saveScreenshot :: IO ()
saveScreenshot = takeScreenshot >>= writePng "screenshot.png"

exportFancy :: T ViewM (IO ())
exportFancy = return saveScreenshot
