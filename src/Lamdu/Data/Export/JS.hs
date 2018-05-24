module Lamdu.Data.Export.JS
    ( exportFancy
    , -- | This compiles in the fast/minimal export-js mode
      compile
    ) where

import qualified Codec.Archive.Zip as Zip
import           Codec.Picture (Image, PixelRGB8(..), withImage)
import qualified Control.Lens as Lens
import           Control.Monad.Trans.FastWriter (execWriterT, tell)
import           Control.Monad.Transaction (getP)
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Property as Property
import           Data.String (IsString(..))
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Foreign as F
import qualified Graphics.Rendering.OpenGL as GL
import qualified Lamdu.Builtins.PrimVal as PrimVal
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Data.Export.JSON (jsonExportRepl)
import qualified Lamdu.Eval.JS.Compiler as Compiler
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.Eval.Results as EV
import           Lamdu.Expr.IRef (ValI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import qualified Lamdu.Paths as Paths
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

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
        let pixelOffset x y = (glY y * width + x) * pixelSize
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

compile :: Monad m => Def.Expr (Val (ValP m)) -> T m String
compile repl =
    repl <&> Lens.mapped %~ valId
    & Compiler.compileRepl actions
    & execWriterT
    <&> unlines
    where
        valId = Compiler.ValId . IRef.uuid . ExprIRef.unValI . Property.value
        actions =
            Compiler.Actions
            { Compiler.output = tell . (:[])
            , Compiler.loggingMode = Compiler.FastSilent
            , Compiler.readAssocTag = lift . getP . Anchors.assocTag
            , Compiler.readAssocName = lift . getP . Anchors.assocTagNameRef
            , Compiler.readGlobal =
                \globalId ->
                ExprIRef.defI globalId & ExprLoad.def
                <&> Def.defBody . Lens.mapped . Lens.mapped %~ valId
                <&> void
                & lift
            , Compiler.readGlobalType =
                \globalId ->
                ExprIRef.defI globalId & Transaction.readIRef & lift
                <&> (^. Def.defType)
            }

formatResult :: EV.Val a -> ByteString
formatResult (EV.Val _ b) =
    case b of
    EV.RPrimVal prim ->
        case PrimVal.toKnown prim of
        PrimVal.Bytes x -> x
        PrimVal.Float x -> show x & fromString
    EV.RInject inj -> inj ^. V.injectVal & formatResult
    _ -> "<TODO: Format result>"

readDataFile :: FilePath -> IO String
readDataFile path = Paths.getDataFileName path >>= readFile

readRepl :: T ViewM (Def.Expr (Val (ValP ViewM)))
readRepl = ExprLoad.defExpr (DbLayout.repl DbLayout.codeAnchors)

exportFancy :: EvalResults (ValI ViewM) -> T ViewM (IO ())
exportFancy evalResults =
    do
        exportedCode <- jsonExportRepl <&> AesonPretty.encodePretty
        repl <- readRepl
        let replResult =
                evalResults
                ^? EV.erExprValues
                . Lens.ix (repl ^. Def.expr . Val.payload . Property.pVal)
                . Lens.ix EV.topLevelScopeId
                <&> formatResult
                & fromMaybe "<NO RESULT>"
        replJs <- compile repl <&> fromString
        pure $
            do
                now <- getPOSIXTime <&> round
                -- screenshot <- takeScreenshot <&> encodePng
                readme <-
                    readDataFile "doc/JSExportReadMe.md"
                    <&> removeReadmeMeta <&> fromString
                rts <- readDataFile "js/rts.js" <&> fromString
                rtsAnchors <- readDataFile "js/anchors.js" <&> fromString
                rtsConf <- readDataFile "js/export/rtsConfig.js" <&> fromString
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
                    , ("js/anchors.js", rtsAnchors)
                    , ("js/rtsConfig.js", rtsConf)
                    ] & Zip.fromArchive & LBS.writeFile "export.zip"
