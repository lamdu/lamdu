-- | The GUI editor
{-# LANGUAGE NamedFieldPuns, RankNTypes, DisambiguateRecordFields #-}
module Lamdu.Editor
    ( run
    ) where

import           Control.Concurrent.MVar
import           Control.DeepSeq (deepseq)
import qualified Control.Exception as E
import qualified Control.Lens.Extended as Lens
import           Data.CurAndPrev (current)
import           Data.IORef
import           Data.Property (Property(..), MkProperty', mkProperty)
import qualified Data.Property as Property
import           GHC.Stack (SrcLoc(..))
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Main as MainLoop
import qualified GUI.Momentu.Widget as Widget
import           Graphics.UI.GLFW.Utils (printGLVersion)
import           Lamdu.Cache (Cache)
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Config as Config
import           Lamdu.Config.Sampler (Sampler, sConfig, sTheme)
import qualified Lamdu.Config.Sampler as ConfigSampler
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.Config.Theme.Fonts (Fonts(..))
import qualified Lamdu.Config.Theme.Fonts as Fonts
import           Lamdu.Data.Db.Layout (DbM)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Debug as Debug
import           Lamdu.Editor.Exports (exportActions)
import qualified Lamdu.Editor.Fonts as EditorFonts
import qualified Lamdu.Editor.Settings as EditorSettings
import qualified Lamdu.Eval.Manager as EvalManager
import qualified Lamdu.Font as Font
import           Lamdu.GUI.IOTrans (ioTrans)
import qualified Lamdu.GUI.Main as GUIMain
import           Lamdu.Main.Env (Env(..))
import qualified Lamdu.Main.Env as Env
import qualified Lamdu.Opts as Opts
import           Lamdu.Settings (Settings(..))
import qualified Lamdu.Settings as Settings
import           Lamdu.Style (FontInfo(..))
import qualified Lamdu.Style as Style
import           Lamdu.Sugar.Convert.Input (AnnotationMode(..))
import qualified Lamdu.Themes as Themes
import           Revision.Deltum.IRef (IRef)
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction
import qualified System.Environment as Env
import           System.IO (hPutStrLn, hFlush, stderr)
import qualified System.Metrics as Metrics
import qualified System.Metrics.Distribution as Distribution
import           System.Process (spawnProcess)
import qualified System.Remote.Monitoring.Shim as Ekg

import           Lamdu.Prelude

type T = Transaction

newtype RefreshScheduler = RefreshScheduler (IORef Bool)

getRefreshScheduled :: RefreshScheduler -> IO Bool
getRefreshScheduled (RefreshScheduler ref) = atomicModifyIORef ref ((,) False)

scheduleRefresh :: RefreshScheduler -> IO ()
scheduleRefresh (RefreshScheduler ref) =
    do
        writeIORef ref True
        MainLoop.wakeUp

stateStorageInIRef ::
    Transaction.Store DbM -> IRef DbLayout.DbM M.GUIState ->
    MkProperty' IO M.GUIState
stateStorageInIRef db stateIRef =
    Transaction.mkPropertyFromIRef stateIRef
    & Property.mkHoist' (DbLayout.runDbTransaction db)

withMVarProtection :: a -> (MVar (Maybe a) -> IO b) -> IO b
withMVarProtection val =
    E.bracket (newMVar (Just val)) (\mvar -> modifyMVar_ mvar (\_ -> pure Nothing))

newEvaluator ::
    IO () -> MVar (Maybe (Transaction.Store DbM)) -> Opts.EditorOpts -> IO EvalManager.Evaluator
newEvaluator refresh dbMVar opts =
    EvalManager.new EvalManager.NewParams
    { EvalManager.resultsUpdated = refresh
    , EvalManager.dbMVar = dbMVar
    , EvalManager.jsDebugPaths = opts ^. Opts.eoJSDebugPaths
    }

makeReportPerfCounters :: Ekg.Server -> IO (MainLoop.PerfCounters -> IO ())
makeReportPerfCounters ekg =
    do
        renderDist <- Metrics.createDistribution "Render time" store
        swapDist <- Metrics.createDistribution "SwapBuffers time" store
        pure $ \(MainLoop.PerfCounters renderTime swapBufferTime) ->
            do
                Distribution.add renderDist renderTime
                Distribution.add swapDist swapBufferTime
    where
        store = Ekg.serverMetricStore ekg

jumpToSource :: SrcLoc -> IO ()
jumpToSource SrcLoc{srcLocFile, srcLocStartLine, srcLocStartCol} =
    Env.lookupEnv "EDITOR"
    >>= \case
    Nothing ->
        do
            hPutStrLn stderr "EDITOR not defined"
            hFlush stderr
    Just editor ->
        spawnProcess editor
        [ "+" ++ show srcLocStartLine ++ ":" ++ show srcLocStartCol
        , srcLocFile
        ] & void

mainLoop ::
    Maybe Ekg.Server -> MkProperty' IO M.GUIState -> Font.LCDSubPixelEnabled ->
    M.Window -> RefreshScheduler -> Sampler ->
    EvalManager.Evaluator -> Transaction.Store DbM ->
    MkProperty' IO Settings -> Cache -> Cache.Functions -> Debug.Monitors ->
    IO ()
mainLoop ekg stateStorage subpixel win refreshScheduler configSampler
    evaluator db mkSettingsProp cache cachedFunctions monitors =
    do
        getFonts <- EditorFonts.makeGetFonts subpixel
        lastVersionNumRef <- newIORef []
        let makeWidget env =
                do
                    sample <- ConfigSampler.getSample configSampler
                    when (sample ^. sConfig . Config.debug . Config.printCursor)
                        (putStrLn ("Cursor: " <> show (env ^. M.cursor)))
                    fonts <- getFonts (env ^. MainLoop.eZoom) sample
                    Cache.fence cache
                    mkSettingsProp ^. mkProperty
                        >>= makeRootWidget cachedFunctions monitors fonts db evaluator sample env
        let mkFontInfo zoom =
                do
                    sample <- ConfigSampler.getSample configSampler
                    getFonts zoom sample
                        <&> (^. Fonts.base) <&> Font.height <&> FontInfo
        let mkConfigTheme =
                ConfigSampler.getSample configSampler
                <&> \sample -> (sample ^. sConfig, sample ^. sTheme)
        reportPerfCounters <- traverse makeReportPerfCounters ekg
        MainLoop.mainLoopWidget win makeWidget MainLoop.Options
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
                        else getRefreshScheduled refreshScheduler
            , stateStorage = stateStorage
            , debug = MainLoop.DebugOptions
                { fpsFont =
                  \zoom ->
                  do
                      sample <- ConfigSampler.getSample configSampler
                      if sample ^. sConfig . Config.debug . Config.debugShowFPS
                          then getFonts zoom sample <&> (^. Fonts.debugInfo) <&> Just
                          else pure Nothing
                , virtualCursorColor =
                    ConfigSampler.getSample configSampler
                    <&> (^. sConfig . Config.debug . Config.virtualCursorShown)
                    <&> \case
                        False -> Nothing
                        True -> Just (M.Color 1 1 0 0.5)
                , reportPerfCounters = fromMaybe (const (pure ())) reportPerfCounters
                , jumpToSource = jumpToSource
                , jumpToSourceKeys =
                    ConfigSampler.getSample configSampler
                    <&> (^. sConfig . Config.debug . Config.jumpToSourceKeys)
                }
            }

makeMainGui ::
    HasCallStack =>
    [Themes.Selection] -> Property IO Settings ->
    (forall a. T DbLayout.DbM a -> IO a) ->
    Env -> T DbLayout.DbM (M.Widget (IO M.Update))
makeMainGui themeNames settingsProp dbToIO env =
    GUIMain.make themeNames settingsProp env
    <&> Lens.mapped %~
    \act ->
    act ^. ioTrans . Lens._Wrapped
    <&> (^. Lens._Wrapped)
    <&> dbToIO
    & join
    >>= runExtraIO
    where
        runExtraIO (extraAct, res) = res <$ extraAct

mkWidgetWithFallback ::
    HasCallStack =>
    Property IO Settings ->
    (forall a. T DbLayout.DbM a -> IO a) ->
    Env -> IO (M.Widget (IO M.Update))
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
                        env & M.cursor .~ mempty
                        & tryMakeGui <&> (,) False
                unless (M.isFocused widget) $
                    fail "Root cursor did not match"
                pure (isValid, widget)
        unless isValid $ putStrLn $ "Invalid cursor: " ++ show (env ^. M.cursor)
        widget
            & M.backgroundColor (["background"] :: M.AnimId) (theme ^. bgColor isValid)
            & pure
    where
        theme = env ^. Env.theme
        bgColor False = Theme.invalidCursorBGColor
        bgColor True = Theme.backgroundColor

makeRootWidget ::
    HasCallStack =>
    Cache.Functions -> Debug.Monitors -> Fonts M.Font ->
    Transaction.Store DbM -> EvalManager.Evaluator -> ConfigSampler.Sample ->
    MainLoop.Env -> Property IO Settings ->
    IO (M.Widget (IO M.Update))
makeRootWidget cachedFunctions perfMonitors fonts db evaluator sample mainLoopEnv settingsProp =
    do
        evalResults <- EvalManager.getResults evaluator
        let env = Env
                { _evalRes = evalResults
                , _exportActions =
                    exportActions (sample ^. sConfig)
                    (evalResults ^. current)
                    (EvalManager.executeReplIOProcess evaluator)
                , _config = sample ^. sConfig
                , _theme = sample ^. sTheme
                , _settings = Property.value settingsProp
                , _style = Style.make fonts (sample ^. sTheme)
                , _mainLoop = mainLoopEnv
                , _animIdPrefix = mempty
                , _debugMonitors = monitors
                , _cachedFunctions = cachedFunctions
                }
        let dbToIO action =
                case settingsProp ^. Property.pVal . Settings.sAnnotationMode of
                Evaluation ->
                    EvalManager.runTransactionAndMaybeRestartEvaluator evaluator action
                _ -> DbLayout.runDbTransaction db action
        let measureLayout w =
                -- Hopefully measuring the forcing of these is enough to figure out the layout -
                -- it's where's the cursors at etc.
                report w
                & Widget.wState . Widget._StateFocused . Lens.mapped %~ f
                where
                    Debug.Evaluator report = monitors ^. Debug.layout . Debug.mPure
                    f x = report ((x ^. Widget.fFocalAreas) `deepseq` x)
        mkWidgetWithFallback settingsProp dbToIO env
            <&> measureLayout
    where
        monitors =
            Debug.addBreakPoints
            (sample ^. sConfig . Config.debug . Config.breakpoints)
            perfMonitors

run :: HasCallStack => Opts.EditorOpts -> Transaction.Store DbM -> IO ()
run opts rawDb =
    do
        refreshScheduler <- newIORef False <&> RefreshScheduler
        let refresh = scheduleRefresh refreshScheduler
        ekg <- traverse Ekg.start (opts ^. Opts.eoEkgPort)
        monitors <-
            traverse Debug.makeCounters ekg
            >>= maybe (pure Debug.noopMonitors) Debug.makeMonitors
        -- Load config as early as possible, before we open any windows/etc
        configSampler <-
            ConfigSampler.new (const refresh) (EditorSettings.initial ^. Settings.sSelectedTheme)
        (cache, cachedFunctions) <- Cache.make
        let Debug.EvaluatorM reportDb = monitors ^. Debug.database . Debug.mAction
        let db = Transaction.onStoreM reportDb rawDb
        let stateStorage = stateStorageInIRef db DbLayout.guiState
        withMVarProtection db $
            \dbMVar ->
            M.withGLFW $
            do
                win <-
                    M.createWindow
                    (opts ^. Opts.eoWindowTitle)
                    (opts ^. Opts.eoWindowMode)
                printGLVersion
                evaluator <- newEvaluator refresh dbMVar opts
                mkSettingsProp <- EditorSettings.newProp configSampler evaluator
                mainLoop ekg stateStorage subpixel win refreshScheduler
                    configSampler evaluator db mkSettingsProp cache cachedFunctions monitors
    where
        subpixel
            | opts ^. Opts.eoSubpixelEnabled = Font.LCDSubPixelEnabled
            | otherwise = Font.LCDSubPixelDisabled
