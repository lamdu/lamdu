{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, OverloadedStrings, Rank2Types, DisambiguateRecordFields, NamedFieldPuns, MultiParamTypeClasses, LambdaCase #-}
module Main
    ( main
    ) where

import           Control.Concurrent.MVar
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import           Control.Monad (replicateM_)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.CurAndPrev (current)
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           GHC.Conc (setNumCapabilities, getNumProcessors)
import           GHC.Stack (whoCreated)
import qualified GUI.Momentu as M
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
import qualified Lamdu.Data.Db.Layout as DbLayout
import           Lamdu.Data.Export.JS (exportFancy)
import qualified Lamdu.Data.Export.JSON as Export
import qualified Lamdu.Eval.Manager as EvalManager
import           Lamdu.Eval.Results (EvalResults)
import           Lamdu.Expr.IRef (ValI)
import           Lamdu.Font (FontSize, Fonts(..))
import qualified Lamdu.Font as Font
import           Lamdu.GUI.CodeEdit.Settings (Settings(..))
import qualified Lamdu.GUI.CodeEdit.Settings as Settings
import           Lamdu.GUI.IOTrans (ioTrans)
import qualified Lamdu.GUI.Main as GUIMain
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Opts as Opts
import qualified Lamdu.Style as Style
import           Lamdu.Themes (defaultTheme, themeSwitchEventMap)
import qualified Lamdu.VersionControl as VersionControl
import           Lamdu.VersionControl.Actions (mUndo)
import           Revision.Deltum.Db (DB)
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
    , _envExportActions :: GUIMain.ExportActions DbLayout.ViewM
    , _envConfig :: Config
    , _envTheme :: Theme
    , _envSettings :: Settings
    , _envStyle :: Style.Style
    , _envMainLoop :: MainLoop.Env
    }
Lens.makeLenses ''Env

instance GUIMain.HasExportActions Env DbLayout.ViewM where exportActions = envExportActions
instance GUIMain.HasEvalResults Env DbLayout.ViewM where evalResults = envEvalRes
instance Settings.HasSettings Env where settings = envSettings
instance Style.HasStyle Env where style = envStyle
instance MainLoop.HasMainLoopEnv Env where mainLoopEnv = envMainLoop
instance M.HasStdSpacing Env where stdSpacing = Theme.theme . Theme.themeStdSpacing
instance M.HasCursor Env
instance M.HasState Env where state = envMainLoop . M.state
instance TextEdit.HasStyle Env where style = envStyle . Style.styleBase
instance TextView.HasStyle Env where style = TextEdit.style . TextView.style
instance Theme.HasTheme Env where theme = envTheme
instance Config.HasConfig Env where config = envConfig

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
        Opts.Parsed{_pLamduDB,_pCommand} <- Opts.get
        lamduDir <- maybe getLamduDir pure _pLamduDB
        let withDB = Db.withDB lamduDir
        case _pCommand of
            Opts.DeleteDb -> deleteDB lamduDir
            Opts.Undo n -> withDB (undoN n)
            Opts.Import path -> withDB (importPath path)
            Opts.Export path -> withDB (exportToPath path)
            Opts.Editor opts -> withDB $ runEditor opts
    `E.catch` \e@E.SomeException{} -> do
    hPutStrLn stderr $ "Main exiting due to exception: " ++ show e
    whoCreated e >>= mapM_ (hPutStrLn stderr)

deleteDB :: FilePath -> IO ()
deleteDB lamduDir =
    do
        putStrLn "Deleting DB..."
        Directory.removeDirectoryRecursive lamduDir

undoN :: Int -> DB -> IO ()
undoN n db =
    do
        putStrLn $ "Undoing " ++ show n ++ " times"
        DbLayout.runDbTransaction db $ replicateM_ n undo
    where
        undo =
            do
                actions <- VersionControl.makeActions
                fromMaybe (fail "Cannot undo any further") $ mUndo actions

importPath :: FilePath -> DB -> IO ()
importPath path db =
    Export.fileImportAll path
    <&> VersionControl.runAction
    >>= DbLayout.runDbTransaction db

exportToPath :: FilePath -> DB -> IO ()
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

settingsChangeHandler :: EvalManager.Evaluator -> Settings -> IO ()
settingsChangeHandler evaluator settings =
    case settings ^. Settings.sInfoMode of
    Settings.Evaluation -> EvalManager.start evaluator
    _ -> EvalManager.stop evaluator

exportActions ::
    Config -> EvalResults (ValI DbLayout.ViewM) -> IO () -> GUIMain.ExportActions DbLayout.ViewM
exportActions config evalResults executeIOProcess =
    GUIMain.ExportActions
    { GUIMain.exportReplActions =
        GUIMain.ExportRepl
        { GUIMain.exportRepl = fileExport Export.fileExportRepl
        , GUIMain.exportFancy = export (exportFancy evalResults)
        , GUIMain.executeIOProcess = executeIOProcess
        }
    , GUIMain.exportAll = fileExport Export.fileExportAll
    , GUIMain.exportDef = fileExport . Export.fileExportDef
    , GUIMain.importAll = importAll
    }
    where
        Config.Export{exportPath} = Config.export config
        export x = ioTrans # (x <&> (`MainLoop.EventResult` ()) & pure)
        fileExport exporter = exporter exportPath & export
        importAll path = ioTrans # (Export.fileImportAll path <&> fmap pure)

makeRootWidget ::
    Fonts M.Font -> DB -> IORef Settings -> EvalManager.Evaluator ->
    Config -> Theme -> MainLoop.Env -> IO (M.Widget (MainLoop.M M.Update))
makeRootWidget fonts db settingsRef evaluator config theme mainLoopEnv =
    do
        eventMap <- Settings.mkEventMap (settingsChangeHandler evaluator) config settingsRef
        evalResults <- EvalManager.getResults evaluator
        settings <- readIORef settingsRef
        let env = Env
                { _envEvalRes = evalResults
                , _envExportActions =
                    exportActions config (evalResults ^. current) (EvalManager.executeReplIOProcess evaluator)
                , _envConfig = config
                , _envTheme = theme
                , _envSettings = settings
                , _envStyle = Style.makeStyle (Theme.codeForegroundColors theme) fonts
                , _envMainLoop = mainLoopEnv
                }
        let dbToIO action =
                case settings ^. Settings.sInfoMode of
                Settings.Evaluation ->
                    EvalManager.runTransactionAndMaybeRestartEvaluator evaluator action
                _ -> DbLayout.runDbTransaction db action
        mkWidgetWithFallback dbToIO env
            <&> M.weakerEvents (eventMap <&> liftIO)

withMVarProtection :: a -> (MVar (Maybe a) -> IO b) -> IO b
withMVarProtection val =
    E.bracket (newMVar (Just val)) (\mvar -> modifyMVar_ mvar (\_ -> pure Nothing))

printGLVersion :: IO ()
printGLVersion =
    do
        ver <- GL.get GL.glVersion
        putStrLn $ "Using GL version: " ++ show ver

stateStorageInIRef :: DB -> IRef DbLayout.DbM M.GUIState -> MainLoop.StateStorage
stateStorageInIRef db stateIRef =
    MainLoop.StateStorage
    { readState = DbLayout.runDbTransaction db (Transaction.readIRef stateIRef)
    , writeState = DbLayout.runDbTransaction db . Transaction.writeIRef stateIRef
    }

runEditor :: Opts.EditorOpts -> DB -> IO ()
runEditor opts db =
    do
        -- Load config as early as possible, before we open any windows/etc
        themeRef <- newIORef defaultTheme
        configSampler <- ConfigSampler.new defaultTheme

        M.withGLFW $ do
            win <-
                createWindow
                (opts ^. Opts.eoWindowTitle)
                (opts ^. Opts.eoWindowMode)
            printGLVersion
            refreshScheduler <- newRefreshScheduler
            withMVarProtection db $ \dbMVar ->
                do
                    evaluator <-
                        EvalManager.new EvalManager.NewParams
                        { EvalManager.resultsUpdated = scheduleRefresh refreshScheduler
                        , EvalManager.dbMVar = dbMVar
                        , EvalManager.copyJSOutputPath = opts ^. Opts.eoCopyJSOutputPath
                        }
                    let initialSettings = Settings Settings.defaultInfoMode
                    settingsRef <- newIORef initialSettings
                    settingsChangeHandler evaluator initialSettings
                    mainLoop stateStorage subpixel win refreshScheduler configSampler $
                        \fonts config theme env ->
                        let themeEvents =
                                themeSwitchEventMap (Config.changeThemeKeys config) configSampler themeRef
                                <&> liftIO
                        in  makeRootWidget fonts db settingsRef evaluator
                            config theme env
                            <&> M.weakerEvents themeEvents
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
    & Font.lfontHelp . _1 .~ helpTextSize
    where
        baseTextSize = Theme.baseTextSize theme
        helpTextSize = Theme.helpTextSize (Theme.help theme)

curSampleFonts :: ConfigSampler.Sample -> Fonts (FontSize, FilePath)
curSampleFonts sample =
    sample ^. sTheme
    & Theme.fonts
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
    IO (M.Widget (MainLoop.M M.Update))) -> IO ()
mainLoop stateStorage subpixel win refreshScheduler configSampler iteration =
    do
        getFonts <- makeGetFonts subpixel
        lastVersionNumRef <- newIORef []
        let makeWidget env =
                do
                    sample <- ConfigSampler.getSample configSampler
                    fonts <- getFonts (env ^. MainLoop.eZoom) sample
                    iteration fonts (sample ^. sConfig) (sample ^. sTheme) env
        M.mainLoopWidget win makeWidget MainLoop.Options
            { getConfig =
                \zoom -> do
                    sample <- ConfigSampler.getSample configSampler
                    fonts <- getFonts zoom sample
                    let height = fontDefault fonts & Font.height
                    Style.mainLoopConfig height (Font.fontHelp fonts)
                        (sample ^. sConfig) (sample ^. sTheme)
                        & pure
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
                      if sample ^. sConfig & Config.debug & Config.debugShowFPS
                          then getFonts zoom sample <&> fontDefault <&> Just
                          else pure Nothing
                , virtualCursorColor =
                    ConfigSampler.getSample configSampler
                    <&> (^. sConfig)
                    <&> Config.debug
                    <&> Config.virtualCursorShown
                    <&> \case
                        False -> Nothing
                        True -> Just (M.Color 1 1 0 0.5)
                }
            }

mkWidgetWithFallback ::
    (forall a. T DbLayout.DbM a -> IO a) ->
    Env -> IO (M.Widget (MainLoop.M M.Update))
mkWidgetWithFallback dbToIO env =
    do
        (isValid, widget) <-
            dbToIO $
            do
                candidateWidget <- makeMainGui dbToIO env
                (isValid, widget) <-
                    if M.isFocused candidateWidget
                    then pure (True, candidateWidget)
                    else
                        env & M.cursor .~ WidgetIds.defaultCursor
                        & makeMainGui dbToIO
                        <&> (,) False
                unless (M.isFocused widget) $
                    fail "Root cursor did not match"
                pure (isValid, widget)
        unless isValid $ putStrLn $ "Invalid cursor: " ++ show (env ^. M.cursor)
        widget
            & M.backgroundColor (["background"] :: M.AnimId) (bgColor isValid theme)
            & pure
    where
        theme = env ^. envTheme
        bgColor False = Theme.invalidCursorBGColor
        bgColor True = Theme.backgroundColor

makeMainGui ::
    (forall a. T DbLayout.DbM a -> IO a) ->
    Env -> T DbLayout.DbM (M.Widget (MainLoop.M M.Update))
makeMainGui dbToIO env =
    GUIMain.make env
    <&> Lens.mapped %~ \act ->
    act ^. ioTrans
    <&> dbToIO
    & join
    & MainLoop.M
