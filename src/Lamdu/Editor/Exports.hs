module Lamdu.Editor.Exports (exportActions) where

import           GUI.Momentu.MetaKey (MetaKey)
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Data.Db.Layout (ViewM)
import           Lamdu.Data.Export.JS (exportFancy)
import qualified Lamdu.Data.Export.JSON as Export
import           Lamdu.Eval.Results (EvalResults)
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.Main as GUIMain

import           Lamdu.Prelude

exportActions ::
    Config MetaKey -> EvalResults -> IO () -> GUIMain.ExportActions ViewM
exportActions config evalResults executeIOProcess =
    GUIMain.ExportActions
    { GUIMain.exportReplActions =
        GUIMain.ExportRepl
        { GUIMain.exportRepl = fileExport Export.fileExportRepl
        , GUIMain.exportFancy = exportFancy evalResults & IOTrans.liftTIO
        , GUIMain.executeIOProcess = executeIOProcess
        }
    , GUIMain.exportAll = fileExport Export.fileExportAll
    , GUIMain.exportDef = fileExport . Export.fileExportDef
    , GUIMain.exportTag = fileExport . Export.fileExportTag
    , GUIMain.importAll = importAll
    }
    where
        exportPath = config ^. Config.export . Config.exportPath
        fileExport exporter = exporter exportPath & IOTrans.liftTIO
        importAll path = Export.fileImportAll path <&> snd & IOTrans.liftIOT
