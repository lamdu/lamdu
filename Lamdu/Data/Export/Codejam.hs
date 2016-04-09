{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lamdu.Data.Export.Codejam
    ( exportFancy
    ) where

import qualified Codec.Archive.Zip as Zip
import           Codec.Picture (Image, PixelRGB8(..), withImage)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Writer (execWriterT, tell)
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (fromMaybe)
import qualified Data.Store.IRef as IRef
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.String (IsString(..))
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Foreign as F
import qualified Graphics.Rendering.OpenGL as GL
import qualified Lamdu.Builtins.PrimVal as PrimVal
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.DbLayout (ViewM)
import qualified Lamdu.Data.DbLayout as DbLayout
import           Lamdu.Data.Export.JSON (jsonExportRepl)
import qualified Lamdu.DataFile as DataFile
import qualified Lamdu.Eval.JS.Compiler as Compiler
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.Eval.Results as EV
import           Lamdu.Expr.IRef (ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Val (Val)
import qualified Lamdu.Expr.Val as V

import           Prelude.Compat

type T = Transaction

_takeScreenshot :: IO (Image PixelRGB8)
_takeScreenshot =
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
    unlines . tail . dropWhile (/= "== ExportFromHere ==") . lines

readRepl :: T ViewM (Val (ValI ViewM))
readRepl =
    DbLayout.repl DbLayout.codeIRefs & Transaction.readIRef
    >>= ExprIRef.readVal

compile :: Val (ValI ViewM) -> T ViewM String
compile val =
    val <&> valId
    & Compiler.compile actions
    & execWriterT
    <&> unlines
    where
        valId = Compiler.ValId . IRef.uuid . ExprIRef.unValI
        actions =
            Compiler.Actions
            { Compiler.output = tell . (:[])
            , Compiler.loggingMode = Compiler.FastSilent
            , Compiler.readAssocName =
                lift . Transaction.getP . Anchors.assocNameRef
            , Compiler.readGlobal =
                  \globalId ->
                  ExprIRef.defI globalId & Transaction.readIRef
                  >>= traverse ExprIRef.readVal
                  & lift
                  <&> Lens.mapped . Lens.mapped %~ valId
            }

formatResult :: EV.Val a -> SBS.ByteString
formatResult (EV.Val _ b) =
    case b of
    EV.RPrimVal prim ->
        case PrimVal.toKnown prim of
        PrimVal.Bytes x -> x
        PrimVal.Float x -> show x & fromString
    EV.RInject inj -> inj ^. V.injectVal & formatResult
    _ -> "<TODO: Format result>"

readDataFile :: FilePath -> IO String
readDataFile path = DataFile.getPath path >>= readFile

exportFancy :: EvalResults (ValI ViewM) -> T ViewM (IO ())
exportFancy evalResults =
    do
        exportedCode <- jsonExportRepl <&> AesonPretty.encodePretty
        repl <- readRepl
        let replResult =
                evalResults
                ^? EV.erExprValues
                . Lens.ix (repl ^. V.payload)
                . Lens.ix EV.topLevelScopeId
                <&> formatResult
                & fromMaybe "<NO RESULT>"
        replJs <- compile repl <&> fromString
        return $
            do
                now <- getPOSIXTime <&> round
                -- screenshot <- takeScreenshot <&> encodePng
                readme <-
                    readDataFile "doc/CodeJamReadMe.md"
                    <&> removeReadmeMeta <&> fromString
                rts <- readDataFile "js/rts.js" <&> fromString
                rtsConf <- readDataFile "js/codeJamRtsConfig.js" <&> fromString
                let addFile archive (filename, contents) =
                        Zip.addEntryToArchive
                        (Zip.toEntry ("export/" ++ filename) now contents)
                        archive
                SBS.writeFile "output.txt" replResult
                foldl addFile Zip.emptyArchive
                    [ ("source.lamdu", exportedCode)
                    -- , ("screenshot.png", screenshot)
                    , ("README.md", readme)
                    , ("js/main.js", replJs)
                    , ("js/rts.js", rts)
                    , ("js/rtsConfig.js", rtsConf)
                    ] & Zip.fromArchive & LBS.writeFile "export.zip"
