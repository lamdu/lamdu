{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}

module Lamdu.Data.Export.JS
    ( exportFancy
    , -- | This compiles in the fast/minimal export-js mode
      compile
    ) where

import qualified Codec.Archive.Zip as Zip
import qualified Control.Lens as Lens
import           Control.Monad.Trans.FastWriter (execWriterT, tell)
import           Control.Monad.Transaction (getP)
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import           Data.String (IsString(..))
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified GUI.Momentu.Direction as Dir
import           Hyper
import           Hyper.Type.Functor (_F)
import qualified Lamdu.Builtins.PrimVal as PrimVal
import           Lamdu.Calc.Term (Var)
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Db.Layout (ViewM)
import           Lamdu.Data.Export.JSON (ExportError, jsonExportDef)
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Data.Tag (getTagName, name)
import qualified Lamdu.Eval.JS.Compiler as Compiler
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.Eval.Results as EV
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as ExprLoad
import           Lamdu.Expr.UniqueId (toUUID)
import           Lamdu.I18N.LangId (LangId(..))
import qualified Lamdu.Paths as Paths
import           Revision.Deltum.Hyper (HRef)
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

type T = Transaction

removeReadmeMeta :: String -> String
removeReadmeMeta =
    unlines . tail . dropWhile (/= "== ExportFromHere ==") . lines

data CompileNameEnv = CompileNameEnv
    { _ceLanguage :: LangId
    , _ceLayout :: Dir.Layout
    }
Lens.makeLenses ''CompileNameEnv

instance Has LangId CompileNameEnv where has = ceLanguage
instance Has Dir.Layout CompileNameEnv where has = ceLayout

compileNameEnv :: CompileNameEnv
compileNameEnv = CompileNameEnv (LangId "english") Dir.LeftToRight

compile :: Monad m => Var -> T m String
compile repl =
    Compiler.compileRepl actions [repl]
    & execWriterT
    <&> unlines
    where
        valId :: HRef m # n -> Const Compiler.ValId # n
        valId = Const . Compiler.ValId . toUUID . (^. ExprIRef.iref)
        actions =
            Compiler.Actions
            { Compiler.output = tell . (:[])
            , Compiler.loggingMode = Compiler.Fast Compiler.ReleaseDontMemoDefs
            , Compiler.readAssocTag = lift . getP . Anchors.assocTag
            , Compiler.readAssocName =
                fmap ((^. _2 . name) . getTagName compileNameEnv) . lift .
                ExprIRef.readTagData
            , Compiler.readGlobal =
                \globalId ->
                ExprIRef.defI globalId & ExprLoad.def
                <&> Def.defBody . Lens.mapped . hflipped %~ hmap (const valId)
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
        PrimVal.Char x -> show x & fromString
    EV.RInject inj -> inj ^. EV.injectVal & formatResult
    _ -> "<TODO: Format result>"

exportFancy :: Var -> EvalResults -> T ViewM (Either (ExportError ViewM) (IO ()))
exportFancy def evalResults =
    jsonExportDef def >>= Lens._Right %%~
    \json ->
    do
        repl <- ExprLoad.def (ExprIRef.defI def) <&> (^?! Def.defBody . Def._BodyExpr)
        let replResult =
                evalResults
                ^? EV.erExprValues
                . Lens.ix (IRef.uuid (repl ^. Def.expr . hAnn . ExprIRef.iref . _F))
                . Lens.ix EV.topLevelScopeId
                <&> formatResult
                & fromMaybe "<NO RESULT>"
        replJs <- compile def <&> fromString
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
                    [ ("source.lamdu", AesonPretty.encodePretty json)
                    -- , ("screenshot.png", screenshot)
                    , ("README.md", readme)
                    , ("js/main.js", replJs)
                    , ("js/rts.js", rts)
                    , ("js/anchors.js", rtsAnchors)
                    , ("js/rtsConfig.js", rtsConf)
                    ] & Zip.fromArchive & LBS.writeFile "export.zip"
