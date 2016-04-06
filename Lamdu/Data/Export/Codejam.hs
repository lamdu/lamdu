{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Data.Export.Codejam
    ( exportFancy
    ) where

import qualified Codec.Archive.Zip as Zip
import           Codec.Picture (Image, PixelRGB8(..), withImage, encodePng)
import           Control.Lens.Operators
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LBS
import           Data.List (isPrefixOf)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.String (IsString(..))
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Foreign as F
import qualified Graphics.Rendering.OpenGL as GL
import qualified Lamdu.Data.DbLayout as DbLayout
import           Lamdu.Data.Export.JSON (jsonExportRepl)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Val (Val)

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
        exportedCode <-
            jsonExportRepl <&> AesonPretty.encodePretty
        _todoRepl <- readRepl
        -- TODO also export RESULT of repl to separate output.txt file
        return $
            do
                now <- getPOSIXTime <&> round
                screenshot <- takeScreenshot <&> encodePng
                readme <-
                    readFile "doc/CodeJamReadMe.md"
                    <&> removeReadmeMeta <&> fromString
                rts <- readFile "js/rts.js" <&> fromString
                -- TODO create fast js runable in codeExport/js/main.js
                let addFile archive (filename, contents) =
                        Zip.addEntryToArchive
                        (Zip.toEntry ("export/" ++ filename) now contents)
                        archive
                foldl addFile Zip.emptyArchive
                    [ ("source.lamdu", exportedCode)
                    , ("screenshot.png", screenshot)
                    , ("README.md", readme)
                    , ("js/rts.js", rts)
                    ] & Zip.fromArchive & LBS.writeFile "export.zip"
