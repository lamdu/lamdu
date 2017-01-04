{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, Rank2Types, DisambiguateRecordFields, NamedFieldPuns, DeriveDataTypeable #-}
module Main
    ( main
    ) where

import           Control.Concurrent.MVar
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import           Control.Monad (join, replicateM_)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.CurAndPrev (current)
import           Data.IORef
import qualified Data.Monoid as Monoid
import           Data.Store.Db (Db)
import qualified Data.Store.IRef as IRef
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.Time.Clock (getCurrentTime)
import           GHC.Conc (setNumCapabilities, getNumProcessors)
import           GHC.Stack (whoCreated)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.Bottle.Main as MainLoop
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils
import           Lamdu.Config (Config(Config))
import qualified Lamdu.Config as Config
import           Lamdu.Config.Sampler (Sampler)
import qualified Lamdu.Config.Sampler as ConfigSampler
import qualified Lamdu.Data.DbInit as DbInit
import qualified Lamdu.Data.DbLayout as DbLayout
import           Lamdu.Data.Export.Codejam (exportFancy)
import qualified Lamdu.Data.Export.JSON as Export
import           Lamdu.DataFile (getLamduDir)
import qualified Lamdu.Eval.Manager as EvalManager
import           Lamdu.Eval.Results (EvalResults)
import           Lamdu.Expr.IRef (ValI)
import           Lamdu.Font (FontSize, Fonts(..))
import qualified Lamdu.Font as Font
import           Lamdu.GUI.CodeEdit.Settings (Settings(..))
import qualified Lamdu.GUI.CodeEdit.Settings as Settings
import qualified Lamdu.GUI.Main as GUIMain
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.GUI.Zoom (Zoom)
import qualified Lamdu.GUI.Zoom as Zoom
import qualified Lamdu.Opts as Opts
import qualified Lamdu.Style as Style
import qualified Lamdu.VersionControl as VersionControl
import           Lamdu.VersionControl.Actions (mUndo)
import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath
import           System.IO (hPutStrLn, stderr)

import           Lamdu.Prelude

type T = Transaction

defaultFonts :: Fonts FilePath
defaultFonts =
    Fonts defaultFont defaultFont defaultFont defaultFont defaultFont
    where
        defaultFont = "fonts/Purisa.ttf"

main :: IO ()
main =
    do
        setNumCapabilities =<< getNumProcessors
        Opts.Parsed{_poShouldDeleteDB,_poUndoCount,_poWindowMode,_poCopyJSOutputPath,_poLamduDB,_poWindowTitle} <-
            either fail return =<< Opts.get
        lamduDir <- maybe getLamduDir return _poLamduDB
        let withDB = DbInit.withDB lamduDir
        let windowTitle = fromMaybe "Lamdu" _poWindowTitle
        if _poShouldDeleteDB
            then deleteDB lamduDir
            else withDB $
                 if _poUndoCount > 0
                 then undoN _poUndoCount
                 else runEditor windowTitle _poCopyJSOutputPath _poWindowMode
    `E.catch` \e@E.SomeException{} -> do
    hPutStrLn stderr $ "Main exiting due to exception: " ++ show e
    mapM_ (hPutStrLn stderr) =<< whoCreated e
    return ()

deleteDB :: FilePath -> IO ()
deleteDB lamduDir =
    do
        putStrLn "Deleting DB..."
        Directory.removeDirectoryRecursive lamduDir

undoN :: Int -> Db -> IO ()
undoN n db =
    do
        putStrLn $ "Undoing " ++ show n ++ " times"
        DbLayout.runDbTransaction db $ replicateM_ n undo
    where
        undo =
            do
                actions <- VersionControl.makeActions
                fromMaybe (fail "Cannot undo any further") $ mUndo actions

createWindow :: String -> Opts.WindowMode -> IO GLFW.Window
createWindow title mode =
    do
        monitor <-
            GLFW.getPrimaryMonitor
            >>= maybe (fail "GLFW: Can't get primary monitor") return
        videoModeSize <- GLFWUtils.getVideoModeSize monitor
        let createWin = GLFWUtils.createWindow title
        case mode of
            Opts.FullScreen         -> createWin (Just monitor) videoModeSize
            Opts.VideoModeSize      -> createWin Nothing videoModeSize
            Opts.WindowSize winSize -> createWin Nothing winSize

settingsChangeHandler :: EvalManager.Evaluator -> Settings -> IO ()
settingsChangeHandler evaluator settings =
    case settings ^. Settings.sInfoMode of
    Settings.Evaluation -> EvalManager.start evaluator
    _ -> EvalManager.stop evaluator

exportActions ::
    Config -> EvalResults (ValI DbLayout.ViewM) -> GUIMain.ExportActions DbLayout.ViewM
exportActions config evalResults =
    GUIMain.ExportActions
    { GUIMain.exportRepl = fileExport Export.fileExportRepl
    , GUIMain.exportAll = fileExport Export.fileExportAll
    , GUIMain.exportFancy = export (exportFancy evalResults)
    , GUIMain.exportDef = fileExport . Export.fileExportDef
    , GUIMain.importAll = importAll
    }
    where
        Config.Export{exportPath} = Config.export config
        export x = x <&> flip (,) () & return & GUIMain.M
        fileExport exporter = exporter exportPath & export
        importAll path = Export.fileImportAll path <&> fmap ((,) (pure ())) & GUIMain.M

makeRootWidget ::
    Fonts Draw.Font -> Db -> Zoom -> IORef Settings -> EvalManager.Evaluator ->
    Config -> Widget.Size -> IO (Widget (MainLoop.M Widget.EventResult))
makeRootWidget fonts db zoom settingsRef evaluator config size =
    do
        cursor <-
            DbLayout.cursor DbLayout.revisionProps
            & Transaction.getP
            & DbLayout.runDbTransaction db
        globalEventMap <- Settings.mkEventMap (settingsChangeHandler evaluator) config settingsRef
        let eventMap = globalEventMap `mappend` Zoom.eventMap zoom (Config.zoom config)
        evalResults <- EvalManager.getResults evaluator
        settings <- readIORef settingsRef
        let env = GUIMain.Env
                { _envEvalRes = evalResults
                , _envExportActions =
                    exportActions config (evalResults ^. current)
                , _envConfig = config
                , _envSettings = settings
                , _envStyle = Style.style config fonts
                , _envFullSize = size
                , _envCursor = cursor
                }
        let dbToIO action =
                case settings ^. Settings.sInfoMode of
                Settings.Evaluation ->
                    EvalManager.runTransactionAndMaybeRestartEvaluator evaluator action
                _ -> DbLayout.runDbTransaction db action
        mkWidgetWithFallback dbToIO env
            <&> Widget.weakerEvents (eventMap <&> liftIO)

withMVarProtection :: a -> (MVar (Maybe a) -> IO b) -> IO b
withMVarProtection val =
    E.bracket (newMVar (Just val)) (\mvar -> modifyMVar_ mvar (\_ -> return Nothing))

printGLVersion :: IO ()
printGLVersion =
    do
        ver <- GL.get GL.glVersion
        putStrLn $ "Using GL version: " ++ show ver

zoomConfig :: Widget.R -> Zoom -> Config -> IO Config
zoomConfig displayScale zoom config =
    do
        factor <- Zoom.getSizeFactor zoom
        return config
            { Config.baseTextSize = baseTextSize * realToFrac factor * scale
            , Config.help =
              help { Config.helpTextSize = helpTextSize * scale }
            }
    where
        scale = realToFrac displayScale
        Config{help, baseTextSize} = config
        Config.Help{helpTextSize} = help

runEditor :: String -> Maybe FilePath -> Opts.WindowMode -> Db -> IO ()
runEditor title copyJSOutputPath windowMode db =
    do
        -- Load config as early as possible, before we open any windows/etc
        rawConfigSampler <- ConfigSampler.new

        GLFWUtils.withGLFW $ do
            win <- createWindow title windowMode
            printGLVersion
            -- Fonts must be loaded after the GL context is created..
            refreshScheduler <- newRefreshScheduler
            withMVarProtection db $ \dbMVar ->
                do
                    evaluator <-
                        EvalManager.new EvalManager.NewParams
                        { EvalManager.invalidateCache = scheduleRefresh refreshScheduler
                        , EvalManager.dbMVar = dbMVar
                        , EvalManager.copyJSOutputPath = copyJSOutputPath
                        }
                    displayScale <- GLFWUtils.getDisplayScale win <&> (^. _2)
                    zoom <- Zoom.make
                    let configSampler =
                            ConfigSampler.onEachSample (zoomConfig displayScale zoom)
                            rawConfigSampler
                    let initialSettings = Settings Settings.defaultInfoMode
                    settingsRef <- newIORef initialSettings
                    settingsChangeHandler evaluator initialSettings
                    addHelp <- EventMapDoc.makeToggledHelpAdder EventMapDoc.HelpNotShown

                    mainLoop win refreshScheduler configSampler $
                        \fonts config size ->
                        makeRootWidget fonts db zoom settingsRef evaluator config size
                        >>= addHelp (Style.help (Font.fontHelp fonts) (Config.help config)) size

newtype RefreshScheduler = RefreshScheduler (IORef Bool)
newRefreshScheduler :: IO RefreshScheduler
newRefreshScheduler = newIORef False <&> RefreshScheduler
isRefreshScheduled :: RefreshScheduler -> IO Bool
isRefreshScheduled (RefreshScheduler ref) = atomicModifyIORef ref $ \r -> (False, r)
scheduleRefresh :: RefreshScheduler -> IO ()
scheduleRefresh (RefreshScheduler ref) = writeIORef ref True

prependConfigPath :: ConfigSampler.Sample Config -> Fonts FilePath -> Fonts FilePath
prependConfigPath sample =
    Lens.mapped %~ (dir </>)
    where
        dir = FilePath.takeDirectory (ConfigSampler.sFilePath sample)

assignFontSizes ::
    ConfigSampler.Sample Config -> Fonts FilePath -> Fonts (FontSize, FilePath)
assignFontSizes sample fonts =
    fonts
    <&> (,) baseTextSize
    & Font.lfontHelp . _1 .~ helpTextSize
    where
        baseTextSize = Config.baseTextSize config
        helpTextSize = Config.helpTextSize (Config.help config)
        config = ConfigSampler.sValue sample

curSampleFonts :: ConfigSampler.Sample Config -> Fonts (FontSize, FilePath)
curSampleFonts sample =
    ConfigSampler.sValue sample
    & Config.fonts
    & prependConfigPath sample
    & assignFontSizes sample

withFontLoop :: Sampler Config -> (IO (Fonts Draw.Font) -> IO a) -> IO a
withFontLoop configSampler act =
    do
        let loadFonts (absFonts, defaultFontsAbs) =
                Font.new absFonts
                `E.catch` \E.SomeException {} ->
                Font.new defaultFontsAbs
        let getFontsDef =
                do
                    sample <- ConfigSampler.getSample configSampler
                    return
                        ( curSampleFonts sample
                        , prependConfigPath sample defaultFonts & assignFontSizes sample
                        )
        startFontsDef <- getFontsDef
        fontsDefRef <- newIORef startFontsDef
        fontsRef <- loadFonts startFontsDef >>= newIORef
        let fontGetter =
                do
                    newFontsDef <- getFontsDef
                    curFontsDef <- readIORef fontsDefRef
                    when (newFontsDef /= curFontsDef) $
                        do
                            writeIORef fontsDefRef newFontsDef
                            loadFonts newFontsDef >>= writeIORef fontsRef
                    readIORef fontsRef
        act fontGetter

mainLoop ::
    GLFW.Window -> RefreshScheduler -> Sampler Config ->
    (Fonts Draw.Font -> Config -> Widget.Size ->
    IO (Widget (MainLoop.M Widget.EventResult))) -> IO ()
mainLoop win refreshScheduler configSampler iteration =
    do
        looper <- MainLoop.newLooper
        withFontLoop configSampler $ \getFonts ->
            do
                lastVersionNumRef <- newIORef =<< getCurrentTime
                let getConfig =
                        ConfigSampler.getSample configSampler
                        <&> Style.mainLoopConfig . ConfigSampler.sValue
                let makeWidget size =
                        do
                            config <-
                                ConfigSampler.getSample configSampler
                                <&> ConfigSampler.sValue
                            fonts <- getFonts
                            iteration fonts config size
                let tickHandler =
                        do
                            curVersionNum <-
                                ConfigSampler.getSample configSampler
                                <&> ConfigSampler.sVersion
                            configChanged <- atomicModifyIORef lastVersionNumRef $ \lastVersionNum ->
                                (curVersionNum, lastVersionNum /= curVersionNum)
                            if configChanged
                                then return True
                                else isRefreshScheduled refreshScheduler
                MainLoop.runLooper looper win tickHandler makeWidget getConfig

rootCursor :: Widget.Id
rootCursor = WidgetIds.fromUUID $ IRef.uuid $ DbLayout.panes DbLayout.codeIRefs

mkWidgetWithFallback ::
    (forall a. T DbLayout.DbM a -> IO a) ->
    GUIMain.Env -> IO (Widget (MainLoop.M Widget.EventResult))
mkWidgetWithFallback dbToIO env =
    do
        (isValid, widget) <-
            dbToIO $
            do
                candidateWidget <- makeMainGui dbToIO env
                (isValid, widget) <-
                    if Widget.isFocused candidateWidget
                    then return (True, candidateWidget)
                    else do
                        finalWidget <-
                            env & GUIMain.envCursor .~ rootCursor & makeMainGui dbToIO
                        Transaction.setP (DbLayout.cursor DbLayout.revisionProps) rootCursor
                        return (False, finalWidget)
                unless (Widget.isFocused widget) $
                    fail "Root cursor did not match"
                return (isValid, widget)
        unless isValid $ putStrLn $ "Invalid cursor: " ++ show (env ^. GUIMain.envCursor)
        widget
            & Widget.backgroundColor ["background"] (bgColor isValid config)
            & return
    where
        config = env ^. GUIMain.envConfig
        bgColor False = Config.invalidCursorBGColor
        bgColor True = Config.backgroundColor

makeMainGui ::
    (forall a. T DbLayout.DbM a -> IO a) ->
    GUIMain.Env -> T DbLayout.DbM (Widget (MainLoop.M Widget.EventResult))
makeMainGui dbToIO env =
    GUIMain.make env rootCursor
    <&> Widget.events %~ \act ->
    act ^. GUIMain.m
    & Lens.mapped %~ (>>= _2 attachCursor)
    <&> dbToIO
    & join
    <&> uncurry MainLoop.EventResult & MainLoop.M
    where
        attachCursor eventResult =
            eventResult ^. Widget.eCursor
            & Monoid.getLast
            & maybe (return ())
              (Transaction.setP (DbLayout.cursor DbLayout.revisionProps))
            & (eventResult <$)
