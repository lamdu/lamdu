module Lamdu.Data.Export.JS
    ( exportFancy
    , -- | This compiles in the fast/minimal export-js mode
      compile
    ) where

import           AST (Ann(..), ann, annotations)
import qualified Codec.Archive.Zip as Zip
import qualified Control.Lens as Lens
import           Control.Monad.Trans.FastWriter (execWriterT, tell)
import           Control.Monad.Transaction (getP)
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Property as Property
import           Data.String (IsString(..))
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Lamdu.Builtins.PrimVal as PrimVal
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Db.Layout (ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Data.Export.JSON (jsonExportRepl)
import           Lamdu.Data.Tag (tagName)
import qualified Lamdu.Eval.JS.Compiler as Compiler
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.Eval.Results as EV
import           Lamdu.Expr.IRef (ValI, ValP)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import           Lamdu.Expr.UniqueId (toUUID)
import qualified Lamdu.Paths as Paths
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

removeReadmeMeta :: String -> String
removeReadmeMeta =
    unlines . tail . dropWhile (/= "== ExportFromHere ==") . lines

compile :: Monad m => Def.Expr (Val (ValP m)) -> T m String
compile repl =
    repl <&> annotations %~ valId
    & Compiler.compileRepl actions
    & execWriterT
    <&> unlines
    where
        valId = Compiler.ValId . toUUID . (^. Property.pVal)
        actions =
            Compiler.Actions
            { Compiler.output = tell . (:[])
            , Compiler.loggingMode = Compiler.FastSilent
            , Compiler.readAssocTag = lift . getP . Anchors.assocTag
            , Compiler.readAssocName = fmap (^. tagName) . lift . ExprIRef.readTagInfo
            , Compiler.readGlobal =
                \globalId ->
                ExprIRef.defI globalId & ExprLoad.def
                <&> Def.defBody . Lens.mapped . annotations %~ valId
                <&> void
                & lift
            , Compiler.readGlobalType =
                \globalId ->
                ExprIRef.defI globalId & Transaction.readIRef & lift
                <&> (^. Def.defType)
            }

formatResult :: EV.Val a -> ByteString
formatResult (Ann _ b) =
    case b of
    EV.RPrimVal prim ->
        case PrimVal.toKnown prim of
        PrimVal.Bytes x -> x
        PrimVal.Float x -> show x & fromString
    EV.RInject inj -> inj ^. V.injectVal & formatResult
    _ -> "<TODO: Format result>"

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
                . Lens.ix (repl ^. Def.expr . ann . Property.pVal)
                . Lens.ix EV.topLevelScopeId
                <&> formatResult
                & fromMaybe "<NO RESULT>"
        replJs <- compile repl <&> fromString
        pure $
            do
                now <- getPOSIXTime <&> round
                -- screenshot <- takeScreenshot <&> encodePng
                readme <-
                    Paths.readDataFile "doc/JSExportReadMe.md"
                    <&> removeReadmeMeta <&> fromString
                rts <- Paths.readDataFile "js/rts.js" <&> fromString
                rtsAnchors <- Paths.readDataFile "js/anchors.js" <&> fromString
                rtsConf <- Paths.readDataFile "js/export/rtsConfig.js" <&> fromString
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
