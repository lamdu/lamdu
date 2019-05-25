-- | The GUI editor
{-# LANGUAGE NamedFieldPuns, RankNTypes, DisambiguateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
module Lamdu.Editor
    ( run
    ) where

import           Control.Concurrent.MVar
import           Control.DeepSeq (deepseq)
import qualified Control.Exception as E
import qualified Control.Lens.Extended as Lens
import           Control.Monad.Trans.FastWriter (evalWriterT)
import qualified Data.Aeson.Config as AesonConfig
import           Data.CurAndPrev (current)
import           Data.Property (Property(..), MkProperty', mkProperty)
import qualified Data.Property as Property
import           GHC.Stack (SrcLoc(..))
import qualified GUI.Momentu as M
import           GUI.Momentu.Main (MainLoop, Handlers(..))
import qualified GUI.Momentu.Main as MainLoop
import           GUI.Momentu.State (Gui)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import           Graphics.UI.GLFW.Utils (printGLVersion)
import qualified Lamdu.Annotations as Annotations
import           Lamdu.Cache (Cache)
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Config as Config
import           Lamdu.Config.Folder (Selection(..))
import qualified Lamdu.Config.Folder as Folder
import           Lamdu.Config.Sampler
    ( Sampler, sConfigData, sThemeData, sLanguageData, sSpritesData )
import qualified Lamdu.Config.Sampler as ConfigSampler
import           Lamdu.Config.Theme (Theme)
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
import           Lamdu.GUI.Main (TitledSelection(..))
import qualified Lamdu.GUI.Main as GUIMain
import           Lamdu.I18N.Language (Language)
import qualified Lamdu.I18N.Language as Language
import           Lamdu.Main.Env (Env(..))
import qualified Lamdu.Main.Env as Env
import qualified Lamdu.Opts as Opts
import           Lamdu.Settings (Settings(..))
import qualified Lamdu.Settings as Settings
import qualified Lamdu.Style.Make as MakeStyle
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

stateStorageInIRef ::
    Transaction.Store DbM -> IRef DbLayout.DbM M.GUIState ->
    MkProperty' IO M.GUIState
stateStorageInIRef db stateIRef =
    Transaction.mkPropertyFromIRef stateIRef
    & Property.mkHoist' (DbLayout.runDbTransaction db)

withMVarProtection :: a -> (MVar (Maybe a) -> IO b) -> IO b
withMVarProtection val =
    E.bracket (newMVar (Just val)) (`modifyMVar_` (\_ -> pure Nothing))

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

mainLoopOptions ::
    MkProperty' IO Settings -> Sampler -> (M.Zoom -> IO (Fonts M.Font)) ->
    MkProperty' IO M.GUIState ->
    Maybe (MainLoop.PerfCounters -> IO ()) -> MainLoop.Options
mainLoopOptions mkSettingsProp configSampler getFonts stateStorage
    reportPerfCounters =
    MainLoop.Options
    { config = MakeStyle.mainLoopConfig helpProp getFonts getConfig
    , stateStorage = stateStorage
    , debug = MainLoop.DebugOptions
        { fpsFont =
          \zoom ->
          do
              sample <- ConfigSampler.getSample configSampler
              if sample ^. sConfigData . Config.debug . Config.debugShowFPS
                  then getFonts zoom <&> (^. Fonts.debugInfo) <&> Just
                  else pure Nothing
        , virtualCursorColor =
            ConfigSampler.getSample configSampler
            <&> (^. sConfigData . Config.debug . Config.virtualCursorShown)
            <&> \case
                False -> Nothing
                True -> Just (M.Color 1 1 0 0.5)
        , reportPerfCounters = fromMaybe (const (pure ())) reportPerfCounters
        , jumpToSource = jumpToSource
        , jumpToSourceKeys =
            ConfigSampler.getSample configSampler
            <&> (^. sConfigData . Config.debug . Config.jumpToSourceKeys)
        }
    , getTexts = ConfigSampler.getSample configSampler <&> MainLoop.makeAllTexts . (^. sLanguageData)
    }
    where
        helpProp =
            mkSettingsProp
            & Property.prop %~ Property.composeLens Settings.sHelpShown

        getConfig =
            ConfigSampler.getSample configSampler
            <&> \sample ->
            ( sample ^. sConfigData
            , sample ^. sThemeData
            , sample ^. sLanguageData
            )

mkEnv ::
    Cache.Functions -> Debug.Monitors -> Fonts M.Font ->
    EvalManager.Evaluator -> Sampler -> MainLoop.Env ->
    Property IO Settings ->
    IO Env
mkEnv cachedFunctions monitors fonts evaluator configSampler mainEnv settings =
    do
        sample <- ConfigSampler.getSample configSampler
        evalResults <- EvalManager.getResults evaluator
        pure Env
            { _evalRes = evalResults
            , _exportActions =
                exportActions (sample ^. sConfigData)
                (evalResults ^. current)
                (EvalManager.executeReplIOProcess evaluator)
            , _config = sample ^. sConfigData
            , _theme = sample ^. sThemeData
            , _settings = settings
            , _style =
                MakeStyle.make fonts (sample ^. sSpritesData) (sample ^. sThemeData)
            , _mainLoop = mainEnv
            , _animIdPrefix = mempty
            , _debugMonitors = monitors
            , _cachedFunctions = cachedFunctions
            , _language = sample ^. sLanguageData
            }

runMainLoop ::
    Maybe Ekg.Server -> MkProperty' IO M.GUIState -> Font.LCDSubPixelEnabled ->
    M.Window -> MainLoop Handlers -> Sampler ->
    EvalManager.Evaluator -> Transaction.Store DbM ->
    MkProperty' IO Settings -> Cache -> Cache.Functions -> Debug.Monitors ->
    IO ()
runMainLoop ekg stateStorage subpixel win mainLoop configSampler
    evaluator db mkSettingsProp cache cachedFunctions monitors
    =
    do
        getFonts <- EditorFonts.makeGetFonts configSampler subpixel
        let makeWidget mainEnv =
                do
                    sample <- ConfigSampler.getSample configSampler
                    when (sample ^. sConfigData . Config.debug . Config.printCursor)
                        (putStrLn ("Cursor: " <> show (mainEnv ^. M.cursor)))
                    fonts <- getFonts (mainEnv ^. MainLoop.eZoom)
                    Cache.fence cache
                    settingsProp <- mkSettingsProp ^. mkProperty
                    env <-
                        mkEnv cachedFunctions monitors fonts evaluator
                        configSampler mainEnv settingsProp
                    makeRootWidget env monitors db evaluator sample
        reportPerfCounters <- traverse makeReportPerfCounters ekg
        MainLoop.run mainLoop win MainLoop.Handlers
            { makeWidget = makeWidget
            , options =
                mainLoopOptions mkSettingsProp configSampler getFonts
                stateStorage reportPerfCounters
            }

makeMainGui ::
    HasCallStack =>
    [TitledSelection Folder.Theme] -> [TitledSelection Folder.Language] ->
    (forall a. T DbLayout.DbM a -> IO a) ->
    Env -> T DbLayout.DbM (Gui Widget IO)
makeMainGui themeNames langNames dbToIO env =
    GUIMain.make themeNames langNames (env ^. Env.settings) env
    <&> Lens.mapped %~
    \act ->
    act ^. ioTrans . Lens._Wrapped
    <&> (^. Lens._Wrapped)
    <&> dbToIO
    & join
    >>= runExtraIO
    where
        runExtraIO (extraAct, res) = res <$ extraAct

backgroundId :: M.AnimId
backgroundId = ["background"]

titledLangSelection ::
    Selection Folder.Language -> IO (TitledSelection Folder.Language)
titledLangSelection sel =
    Folder.selectionToPath (Proxy @Language) sel
    >>= evalWriterT . AesonConfig.load
    <&> \conf ->
    TitledSelection
    { _title = conf ^. Language.lTitle
    , _selection = sel
    }

titledThemeSelection ::
    Selection Folder.Language ->
    Selection Folder.Theme ->
    IO (TitledSelection Folder.Theme)
titledThemeSelection (Selection lang) sel =
    Folder.selectionToPath (Proxy @Theme) sel
    >>= evalWriterT . AesonConfig.load
    <&> \conf ->
    TitledSelection
    { _title =
        conf ^. Theme.title . Lens.at lang
        & fromMaybe (conf ^. Theme.title . Lens.ix "english")
    , _selection = sel
    }

makeRootWidget ::
    HasCallStack =>
    Env -> Debug.Monitors ->
    Transaction.Store DbM -> EvalManager.Evaluator -> ConfigSampler.Sample ->
    IO (Gui Widget IO)
makeRootWidget env perfMonitors db evaluator sample =
    do
        themeNames <-
            Folder.getSelections (Proxy @Theme)
            >>= traverse (titledThemeSelection selectedLang)
        langNames <-
            Folder.getSelections (Proxy @Language)
            >>= traverse titledLangSelection
        let bgColor = env ^. Env.theme . Theme.backgroundColor
        dbToIO $ makeMainGui themeNames langNames dbToIO env
            <&> M.backgroundColor backgroundId bgColor
            <&> measureLayout
    where
        selectedLang = env ^. Env.settings . Property.pVal . Settings.sSelectedLanguage
        monitors =
            Debug.addBreakPoints
            (sample ^. sConfigData . Config.debug . Config.breakpoints)
            perfMonitors
        dbToIO action =
            case env ^. Env.settings . Property.pVal . Settings.sAnnotationMode of
            Annotations.Evaluation ->
                EvalManager.runTransactionAndMaybeRestartEvaluator evaluator action
            _ -> DbLayout.runDbTransaction db action
        measureLayout w =
            -- Hopefully measuring the forcing of these is enough to figure out the layout -
            -- it's where's the cursors at etc.
            report w
            & Widget.wState . Widget._StateFocused . Lens.mapped %~ f
            where
                Debug.Evaluator report = monitors ^. Debug.layout . Debug.mPure
                f x = report ((x ^. Widget.fFocalAreas) `deepseq` x)

run :: HasCallStack => Opts.EditorOpts -> Transaction.Store DbM -> IO ()
run opts rawDb =
    do
        mainLoop <- MainLoop.mainLoopWidget
        let refresh = MainLoop.wakeUp mainLoop
        ekg <- traverse Ekg.start (opts ^. Opts.eoEkgPort)
        monitors <-
            traverse Debug.makeCounters ekg
            >>= maybe (pure Debug.noopMonitors) Debug.makeMonitors
        -- Load config as early as possible, before we open any windows/etc
        initialSettings <- EditorSettings.read
        let initialTheme = initialSettings ^. Settings.sSelectedTheme
        let initialLanguage = initialSettings ^. Settings.sSelectedLanguage
        configSampler <- ConfigSampler.new (const refresh) initialTheme initialLanguage
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
                mkSettingsProp <-
                    EditorSettings.newProp initialSettings configSampler evaluator
                runMainLoop ekg stateStorage subpixel win mainLoop
                    configSampler evaluator db mkSettingsProp cache cachedFunctions monitors
    where
        subpixel
            | opts ^. Opts.eoSubpixelEnabled = Font.LCDSubPixelEnabled
            | otherwise = Font.LCDSubPixelDisabled
