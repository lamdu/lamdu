{-# LANGUAGE TupleSections #-}

module Lamdu.Editor.Exports (exportActions) where

import           Hyper.Type.Functor (_F)
import           GUI.Momentu.ModKey (ModKey)
import qualified GUI.Momentu.State as GuiState
import qualified Lamdu.Calc.Term as V
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Data.Db.Layout (ViewM, codeAnchors)
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Data.Export.JS (exportFancy)
import qualified Lamdu.Data.Export.JSON as Export
import qualified Lamdu.Data.Export.JSON.Import as Import
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.Main as GUIMain

import           Lamdu.Prelude
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Revision.Deltum.IRef (uuid)

exportActions :: Config ModKey -> EvalResults -> (V.Var -> IO ()) -> GUIMain.ExportActions ViewM
exportActions config evalResults executeIOProcess =
    GUIMain.ExportActions
    { GUIMain.exportAll = fileExport Export.fileExportAll
    , GUIMain.exportDef = fileExport . Export.fileExportDef
    , GUIMain.exportDefToJS = exportRes . (`exportFancy` evalResults)
    , GUIMain.executeDef = executeIOProcess
    , GUIMain.exportTag = fileExport . Export.fileExportTag
    , GUIMain.exportNominal = fileExport . Export.fileExportNominal
    , GUIMain.importAll = importAll
    }
    where
        exportPath = config ^. Config.export . Config.exportPath
        exportRes a =
            a
            >>= either goToError (pure . (, mempty))
            & IOTrans.liftTIO
        fileExport = exportRes . (exportPath &)
        importAll path = Import.fileImportAll path <&> snd & IOTrans.liftIOT
        goToError err =
            ( pure ()
            , err ^. Export.deletedDefUseTerm . _F & uuid & WidgetIds.fromUUID & GuiState.updateCursor
            ) <$
            (err ^. Export.deletedDefUsedInDef & Anchors.PaneDefinition & DataOps.newPane codeAnchors)
