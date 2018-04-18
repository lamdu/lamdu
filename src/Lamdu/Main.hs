{-# LANGUAGE TemplateHaskell, Rank2Types, DisambiguateRecordFields, NamedFieldPuns, MultiParamTypeClasses #-}
module Main
    ( main
    ) where

import           Control.Concurrent.MVar
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import           Control.Monad (replicateM_)
import           Control.Monad.Trans.FastWriter (writerT)
import           Data.CurAndPrev (current)
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           Data.Property (Property(..))
import qualified Data.Property as Property
import qualified Data.Tuple as Tuple
import           GHC.Conc (setNumCapabilities, getNumProcessors)
import           GHC.Stack (whoCreated)
import qualified GUI.Momentu as M
import           GUI.Momentu.Animation.Id (AnimId)
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Main as MainLoop
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Graphics.Rendering.OpenGL.GL as GL
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Sampler (Sampler, sConfig, sTheme)
import qualified Lamdu.Config.Sampler as ConfigSampler
import           Lamdu.Config.Theme (Theme(..))
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Data.Db as Db
import           Lamdu.Data.Db.Layout (DbM, ViewM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import           Lamdu.Data.Export.JS (exportFancy)
import qualified Lamdu.Data.Export.JSON as Export
import qualified Lamdu.Ekg as Ekg
import qualified Lamdu.Eval.Manager as EvalManager
import           Lamdu.Eval.Results (EvalResults)
import           Lamdu.Expr.IRef (ValI)
import           Lamdu.Font (FontSize, Fonts(..))
import qualified Lamdu.Font as Font
import           Lamdu.GUI.CodeEdit.AnnotationMode (AnnotationMode(..))
import           Lamdu.GUI.IOTrans (ioTrans)
import qualified Lamdu.GUI.IOTrans as IOTrans
import qualified Lamdu.GUI.Main as GUIMain
import qualified Lamdu.GUI.VersionControl.Config as VCConfig
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Opts as Opts
import           Lamdu.Settings (Settings(..))
import qualified Lamdu.Settings as Settings
import           Lamdu.Style (FontInfo(..))
import qualified Lamdu.Style as Style
import qualified Lamdu.Themes as Themes
import qualified Lamdu.VersionControl as VersionControl
import           Lamdu.VersionControl.Actions (mUndo)
import           Revision.Deltum.IRef (IRef)
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction
import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath
import           System.IO (hPutStrLn, stderr)

import           Lamdu.Prelude

type T = Transaction

data Env = Env
    { _envEvalRes :: GUIMain.EvalResults
    , _envExportActions :: GUIMain.ExportActions ViewM
    , _envConfig :: Config
    , _envTheme :: Theme
    , _envSettings :: Settings
    , _envStyle :: Style.Style
    , _envMainLoop :: MainLoop.Env
    , _envAnimIdPrefix :: AnimId
    }
Lens.makeLenses ''Env

instance GUIMain.HasExportActions Env ViewM where exportActions = envExportActions
instance GUIMain.HasEvalResults Env ViewM where evalResults = envEvalRes
instance Settings.HasSettings Env where settings = envSettings
instance Style.HasStyle Env where style = envStyle
instance MainLoop.HasMainLoopEnv Env where mainLoopEnv = envMainLoop
instance M.HasStdSpacing Env where stdSpacing = Theme.theme . Theme.stdSpacing
instance M.HasCursor Env
instance M.HasState Env where state = envMainLoop . M.state
instance TextEdit.HasStyle Env where style = envStyle . Style.styleBase
instance TextView.HasStyle Env where style = TextEdit.style . TextView.style
instance Theme.HasTheme Env where theme = envTheme
instance Config.HasConfig Env where config = envConfig
instance M.HasAnimIdPrefix Env where animIdPrefix = envAnimIdPrefix
instance Hover.HasStyle Env where style = envTheme . Hover.style
instance VCConfig.HasTheme Env where theme = envTheme . Theme.versionControl
instance VCConfig.HasConfig Env where config = envConfig . Config.versionControl

defaultFontPath :: ConfigSampler.Sample -> FilePath
defaultFontPath sample =
    configDir </> "fonts/Purisa.ttf"
    where
        configDir = FilePath.takeDirectory (sample ^. ConfigSampler.sConfigPath)

getLamduDir :: IO FilePath
getLamduDir = Directory.getHomeDirectory <&> (</> ".lamdu")

main :: IO ()
main =
    do
        setNumCapabilities =<< getNumProcessors
        Opts.Parsed{_pLamduDB,_pCommand,_pEkgPort} <- Opts.get
        foldMap Ekg.start _pEkgPort
        lamduDir <- maybe getLamduDir pure _pLamduDB
        let withDB = Db.withDB lamduDir
        case _pCommand of
            Opts.DeleteDb -> deleteDB lamduDir
            Opts.Undo n -> withDB (undoN n)
            Opts.Import path -> withDB (importPath path)
            Opts.Export path -> withDB (exportToPath path)
            Opts.Editor opts -> withDB (runEditor opts)
    `E.catch` \e@E.SomeException{} -> do
    hPutStrLn stderr $ "Main exiting due to exception: " ++ show e
    whoCreated e >>= mapM_ (hPutStrLn stderr)

deleteDB :: FilePath -> IO ()
deleteDB lamduDir =
    do
        putStrLn "Deleting DB..."
        Directory.removeDirectoryRecursive lamduDir

undoN :: Int -> Transaction.Store DbM -> IO ()
undoN n db =
    do
        putStrLn $ "Undoing " ++ show n ++ " times"
        DbLayout.runDbTransaction db $ replicateM_ n undo
    where
        undo =
            do
                actions <- VersionControl.makeActions
                fromMaybe (fail "Cannot undo any further") $ mUndo actions

importPath :: FilePath -> Transaction.Store DbM -> IO ()
importPath path db =
    Export.fileImportAll path
    <&> VersionControl.runAction
    >>= DbLayout.runDbTransaction db

exportToPath :: FilePath -> Transaction.Store DbM -> IO ()
exportToPath path db =
    Export.fileExportAll path
    & VersionControl.runAction
    & DbLayout.runDbTransaction db
    & join

createWindow :: String -> Opts.WindowMode -> IO M.Window
createWindow title mode =
    do
        monitor <-
            M.getPrimaryMonitor
            >>= maybe (fail "GLFW: Can't get primary monitor") pure
        videoModeSize <- M.getVideoModeSize monitor
        let createWin = M.createWindow title
        case mode of
            Opts.FullScreen         -> createWin (Just monitor) videoModeSize
            Opts.VideoModeSize      -> createWin Nothing videoModeSize

settingsChangeHandler :: Sampler -> EvalManager.Evaluator -> Maybe Settings -> Settings -> IO ()
settingsChangeHandler configSampler evaluator mOld new =
    do
        whenChanged Settings.sAnnotationMode $ \case
            Evaluation -> EvalManager.start evaluator
            _ -> EvalManager.stop evaluator
        whenChanged Settings.sSelectedTheme $ ConfigSampler.setTheme configSampler
    where
        whenChanged lens f =
            case mOld of
            Nothing -> f (new ^. lens)
            Just old -> when (old ^. lens /= new ^. lens) $ f (new ^. lens)

exportActions ::
    Config -> EvalResults (ValI ViewM) -> IO () -> GUIMain.ExportActions ViewM
exportActions config evalResults executeIOProcess =
    GUIMain.ExportActions
    { GUIMain.exportReplActions =
        GUIMain.ExportRepl
        { GUIMain.exportRepl = fileExport Export.fileExportRepl
        , GUIMain.exportFancy = exportFancy evalResults & execTIO
        , GUIMain.executeIOProcess = executeIOProcess
        }
    , GUIMain.exportAll = fileExport Export.fileExportAll
    , GUIMain.exportDef = fileExport . Export.fileExportDef
    , GUIMain.importAll = importAll
    }
    where
        exportPath = config ^. Config.export . Config.exportPath
        execTIO = IOTrans.liftTExecInMain . fmap MainLoop.ExecuteInMainThread
        fileExport exporter = exporter exportPath & execTIO
        importAll path = Export.fileImportAll path & IOTrans.liftIOT

makeRootWidget ::
    Fonts M.Font -> Transaction.Store DbM -> EvalManager.Evaluator -> Config -> Theme ->
    MainLoop.Env -> Property IO Settings -> IO (M.Widget (MainLoop.M IO M.Update))
makeRootWidget fonts db evaluator config theme mainLoopEnv settingsProp =
    do
        evalResults <- EvalManager.getResults evaluator
        let env = Env
                { _envEvalRes = evalResults
                , _envExportActions =
                    exportActions config
                    (evalResults ^. current)
                    (EvalManager.executeReplIOProcess evaluator)
                , _envConfig = config
                , _envTheme = theme
                , _envSettings = Property.value settingsProp
                , _envStyle = Style.makeStyle (theme ^. Theme.textColors) fonts
                , _envMainLoop = mainLoopEnv
                , _envAnimIdPrefix = mempty
                }
        let dbToIO action =
                case settingsProp ^. Property.pVal . Settings.sAnnotationMode of
                Evaluation ->
                    EvalManager.runTransactionAndMaybeRestartEvaluator evaluator action
                _ -> DbLayout.runDbTransaction db action
        mkWidgetWithFallback settingsProp dbToIO env

withMVarProtection :: a -> (MVar (Maybe a) -> IO b) -> IO b
withMVarProtection val =
    E.bracket (newMVar (Just val)) (\mvar -> modifyMVar_ mvar (\_ -> pure Nothing))

printGLVersion :: IO ()
printGLVersion =
    do
        ver <- GL.get GL.glVersion
        putStrLn $ "Using GL version: " ++ show ver

stateStorageInIRef :: Transaction.Store DbM -> IRef DbLayout.DbM M.GUIState -> MainLoop.StateStorage
stateStorageInIRef db stateIRef =
    MainLoop.StateStorage
    { readState = DbLayout.runDbTransaction db (Transaction.readIRef stateIRef)
    , writeState = DbLayout.runDbTransaction db . Transaction.writeIRef stateIRef
    }

newSettingsProp ::
    Settings -> Sampler -> EvalManager.Evaluator -> IO (IO (Property IO Settings))
newSettingsProp initial configSampler evaluator =
    do
        settingsRef <- newIORef initial
        let setSettings val =
                do
                    oldVal <- readIORef settingsRef
                    writeIORef settingsRef val
                    settingsChangeHandler configSampler evaluator (Just oldVal) val
        settingsChangeHandler configSampler evaluator Nothing initial
        readIORef settingsRef <&> (`Property` setSettings) & pure

newEvaluator ::
    IO () -> MVar (Maybe (Transaction.Store DbM)) -> Opts.EditorOpts -> IO EvalManager.Evaluator
newEvaluator refresh dbMVar opts =
    EvalManager.new EvalManager.NewParams
    { EvalManager.resultsUpdated = refresh
    , EvalManager.dbMVar = dbMVar
    , EvalManager.copyJSOutputPath = opts ^. Opts.eoCopyJSOutputPath
    }

runEditor :: Opts.EditorOpts -> Transaction.Store DbM -> IO ()
runEditor opts db =
    withMVarProtection db $ \dbMVar ->
    do
        refreshScheduler <- newRefreshScheduler
        let refresh = scheduleRefresh refreshScheduler

        -- Load config as early as possible, before we open any windows/etc
        evaluator <- newEvaluator refresh dbMVar opts
        let settingsVal = Settings.initial
        configSampler <-
            ConfigSampler.new (const refresh) (settingsVal ^. Settings.sSelectedTheme)
        mkSettingsProp <- newSettingsProp settingsVal configSampler evaluator

        M.withGLFW $ do
            win <-
                createWindow
                (opts ^. Opts.eoWindowTitle)
                (opts ^. Opts.eoWindowMode)
            printGLVersion
            mainLoop stateStorage subpixel win refreshScheduler configSampler $
                \fonts config theme env ->
                mkSettingsProp >>= makeRootWidget fonts db evaluator config theme env
    where
        stateStorage = stateStorageInIRef db DbLayout.guiState
        subpixel
            | opts ^. Opts.eoSubpixelEnabled = Font.LCDSubPixelEnabled
            | otherwise = Font.LCDSubPixelDisabled

newtype RefreshScheduler = RefreshScheduler (IORef Bool)
newRefreshScheduler :: IO RefreshScheduler
newRefreshScheduler = newIORef False <&> RefreshScheduler
isRefreshScheduled :: RefreshScheduler -> IO Bool
isRefreshScheduled (RefreshScheduler ref) = atomicModifyIORef ref $ \r -> (False, r)
scheduleRefresh :: RefreshScheduler -> IO ()
scheduleRefresh (RefreshScheduler ref) =
    do
        writeIORef ref True
        MainLoop.wakeUp

prependConfigPath :: ConfigSampler.Sample -> Fonts FilePath -> Fonts FilePath
prependConfigPath sample =
    Lens.mapped %~ (dir </>)
    where
        dir = FilePath.takeDirectory (sample ^. ConfigSampler.sConfigPath)

assignFontSizes :: Theme -> Fonts FilePath -> Fonts (FontSize, FilePath)
assignFontSizes theme fonts =
    fonts
    <&> (,) baseTextSize
    & Font.fontHelp . _1 .~ helpTextSize
    where
        baseTextSize = theme ^. Theme.baseTextSize
        helpTextSize = theme ^. Theme.help . Theme.helpTextSize

curSampleFonts :: ConfigSampler.Sample -> Fonts (FontSize, FilePath)
curSampleFonts sample =
    sample ^. sTheme . Theme.fonts
    & prependConfigPath sample
    & assignFontSizes (sample ^. sTheme)

makeGetFonts ::
    Font.LCDSubPixelEnabled ->
    IO (M.Zoom -> ConfigSampler.Sample -> IO (Fonts M.Font))
makeGetFonts subpixel =
    Font.new subpixel & uncurry & memoIO
    <&> f
    where
        f cachedLoadFonts zoom sample =
            do
                sizeFactor <- M.getZoomFactor zoom
                cachedLoadFonts
                    ( defaultFontPath sample
                    , curSampleFonts sample <&> _1 *~ sizeFactor
                    )

mainLoop ::
    MainLoop.StateStorage -> Font.LCDSubPixelEnabled ->
    M.Window -> RefreshScheduler -> Sampler ->
    (Fonts M.Font -> Config -> Theme -> MainLoop.Env ->
    IO (M.Widget (MainLoop.M IO M.Update))) -> IO ()
mainLoop stateStorage subpixel win refreshScheduler configSampler iteration =
    do
        getFonts <- makeGetFonts subpixel
        lastVersionNumRef <- newIORef []
        let makeWidget env =
                do
                    sample <- ConfigSampler.getSample configSampler
                    fonts <- getFonts (env ^. MainLoop.eZoom) sample
                    iteration fonts (sample ^. sConfig) (sample ^. sTheme) env
        let mkFontInfo zoom =
                do
                    sample <- ConfigSampler.getSample configSampler
                    fonts <- getFonts zoom sample
                    let height = fonts ^. Font.fontDefault & Font.height
                    pure FontInfo
                        { primaryFontHeight = height
                        , helpFont = fonts ^. Font.fontHelp
                        }
        let mkConfigTheme =
                ConfigSampler.getSample configSampler
                <&> \sample -> (sample ^. sConfig, sample ^. sTheme)
        M.mainLoopWidget win makeWidget MainLoop.Options
            { config = Style.mainLoopConfig mkFontInfo mkConfigTheme
            , tickHandler =
                do
                    sample <- ConfigSampler.getSample configSampler
                    let curVersionNum = ConfigSampler.sVersion sample
                    configChanged <- atomicModifyIORef lastVersionNumRef $
                        \lastVersionNum ->
                        (curVersionNum, lastVersionNum /= curVersionNum)
                    if configChanged
                        then pure True
                        else isRefreshScheduled refreshScheduler
            , stateStorage = stateStorage
            , debug = MainLoop.DebugOptions
                { fpsFont =
                  \zoom ->
                  do
                      sample <- ConfigSampler.getSample configSampler
                      if sample ^. sConfig . Config.debug . Config.debugShowFPS
                          then getFonts zoom sample <&> (^. Font.fontDefault) <&> Just
                          else pure Nothing
                , virtualCursorColor =
                    ConfigSampler.getSample configSampler
                    <&> (^. sConfig . Config.debug . Config.virtualCursorShown)
                    <&> \case
                        False -> Nothing
                        True -> Just (M.Color 1 1 0 0.5)
                }
            }

mkWidgetWithFallback ::
    Property IO Settings ->
    (forall a. T DbLayout.DbM a -> IO a) ->
    Env -> IO (M.Widget (MainLoop.M IO M.Update))
mkWidgetWithFallback settingsProp dbToIO env =
    do
        themeNames <- Themes.getNames
        let tryMakeGui = makeMainGui themeNames settingsProp dbToIO
        (isValid, widget) <-
            dbToIO $
            do
                candidateWidget <- tryMakeGui env
                (isValid, widget) <-
                    if M.isFocused candidateWidget
                    then pure (True, candidateWidget)
                    else
                        env & M.cursor .~ WidgetIds.defaultCursor
                        & tryMakeGui <&> (,) False
                unless (M.isFocused widget) $
                    fail "Root cursor did not match"
                pure (isValid, widget)
        unless isValid $ putStrLn $ "Invalid cursor: " ++ show (env ^. M.cursor)
        widget
            & M.backgroundColor (["background"] :: M.AnimId) (theme ^. bgColor isValid)
            & pure
    where
        theme = env ^. envTheme
        bgColor False = Theme.invalidCursorBGColor
        bgColor True = Theme.backgroundColor

makeMainGui ::
    [Themes.Selection] -> Property IO Settings ->
    (forall a. T DbLayout.DbM a -> IO a) ->
    Env -> T DbLayout.DbM (M.Widget (MainLoop.M IO M.Update))
makeMainGui themeNames settingsProp dbToIO env =
    GUIMain.make themeNames settingsProp env
    <&> Lens.mapped %~
    \act ->
    act ^. ioTrans . Lens._Wrapped
    <&> (^. Lens._Wrapped)
    <&> dbToIO
    & join
    <&> Tuple.swap
    & writerT
