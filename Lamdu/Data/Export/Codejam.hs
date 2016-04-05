{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Data.Export.Codejam
    ( exportFancy
    ) where

import           Codec.Picture (Image, PixelRGB8(..), withImage, writePng)
import           Control.Lens.Operators
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LBS
import           Data.List (isPrefixOf)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Foreign as F
import qualified Lamdu.Data.DbLayout as DbLayout
import           Lamdu.Data.Export.JSON (jsonExportRepl)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Val (Val)
import qualified Graphics.Rendering.OpenGL as GL
import           System.Directory (createDirectory, copyFile)

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

removeReadmeMeta :: String -> String
removeReadmeMeta =
    unlines . dropWhile (not . isPrefixOf "##") . tail . lines

readRepl :: T DbLayout.ViewM (Val (ExprIRef.ValI DbLayout.ViewM))
readRepl =
    DbLayout.repl DbLayout.codeIRefs & Transaction.readIRef
    >>= ExprIRef.readVal

exportFancy :: T DbLayout.ViewM (IO ())
exportFancy =
    do
        exportedCode <- jsonExportRepl
        _todoRepl <- readRepl
        return $
            do
                createDirectory "codeExport"
                takeScreenshot >>= writePng "codeExport/screenshot.png"
                AesonPretty.encodePretty exportedCode &
                    LBS.writeFile "codeExport/source.lamdu"
                createDirectory "codeExport/js"
                copyFile "js/rts.js" "codeExport/js/rts.js"
                readFile "doc/CodeJamReadMe.md"
                    <&> removeReadmeMeta
                    >>= writeFile "codeExport/README.md"
                -- TODO create fast js runable in codeExport/js/main.js
