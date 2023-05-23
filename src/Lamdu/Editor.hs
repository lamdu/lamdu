-- | The GUI editor
{-# LANGUAGE NamedFieldPuns, RankNTypes, DisambiguateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
module Lamdu.Editor
    ( run
    ) where

import qualified Codec.Image.STB as Image
import           Control.Concurrent.MVar
import           Control.DeepSeq (deepseq)
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT, _OnceT, OnceState, MonadOnce(..), runOnceT)
import           Control.Monad.State (mapStateT)
import           Control.Monad.Trans.FastWriter (evalWriterT)
import qualified Data.Aeson.Config as AesonConfig
import qualified Data.Bitmap as Bitmap
import qualified Data.Bitmap.Pure.Pixels as BitmapPixels
import           Data.CurAndPrev (current)
import           Data.List (sortOn)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.Property (Property(..), MkProperty', mkProperty)
import qualified Data.Property as Property
import           GHC.Stack (SrcLoc(..))
import           GUI.Momentu (Widget)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.Main (MainLoop, Handlers(..))
import qualified GUI.Momentu.Main as MainLoop
import qualified GUI.Momentu.Widget as Widget
import qualified Graphics.UI.GLFW as GLFW
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
import qualified Lamdu.Paths as Paths
import           Lamdu.Settings (Settings(..))
import qualified Lamdu.Settings as Settings
import qualified Lamdu.Style.Make as MakeStyle
import           Lamdu.Sugar (sugarWorkArea)
import           Lamdu.Sugar.Config (Sugars)
import qualified Lamdu.VersionControl as VersionControl
import           Revision.Deltum.IRef (IRef)
import           Revision.Deltum.Rev.Version (Version)
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
        spawnProcess editor params & void
        where
            params
                | editor == "code" = ["--goto", srcLocFile <> ":" <> pos]
                | otherwise = ["+" <> pos, srcLocFile]
            pos = show srcLocStartLine <> ":" <> show srcLocStartCol

mainLoopOptions ::
    MkProperty' IO Settings -> Sampler -> (M.Zoom -> IO (Fonts M.Font)) ->
    MkProperty' IO M.GUIState ->
    Maybe (MainLoop.PerfCounters -> IO ()) -> MainLoop.Options
mainLoopOptions mkSettingsProp configSampler getFonts stateStorage
    reportPerfCounters =
    MainLoop.Options
    { _oConfig = MakeStyle.mainLoopConfig helpProp getFonts getConfig
    , _oStateStorage = stateStorage
    , _oDebug = MainLoop.DebugOptions
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
        , postProcessEvent =
            \event ->
            do
                shouldPrintEvents <-
                    ConfigSampler.getSample configSampler
                    <&> (^. sConfigData . Config.debug . Config.printEvents)
                when shouldPrintEvents $ print event
        }
    , _oGetTexts = ConfigSampler.getSample configSampler <&> MainLoop.makeAllTexts . (^. sLanguageData)
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
    Property IO Settings -> Property IO (Sugars Bool) ->
    IO Env
mkEnv cachedFunctions monitors fonts evaluator configSampler mainEnv settings sugars =
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
            , _sugars = sugars
            , _style = MakeStyle.make fonts (sample ^. sThemeData)
            , _sprites = sample ^. sSpritesData
            , _mainLoop = mainEnv
            , _elemIdPrefix = mempty
            , _debugMonitors = monitors
            , _cachedFunctions = cachedFunctions
            , _language = sample ^. sLanguageData
            , _codeAnchors = DbLayout.codeAnchors
            }

runMainLoop ::
    Maybe Ekg.Server -> MkProperty' IO M.GUIState -> Font.LCDSubPixelEnabled ->
    M.Window -> MainLoop Handlers -> Sampler ->
    EvalManager.Evaluator -> Transaction.Store DbM ->
    MkProperty' IO Settings -> MkProperty' IO (Sugars Bool) -> Cache -> Cache.Functions -> Debug.Monitors ->
    IORef (Maybe EditorCache) ->
    IO ()
runMainLoop ekg stateStorage subpixel win mainLoop configSampler
    evaluator db mkSettingsProp mkSugarsProp cache cachedFunctions monitors cacheRef
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
                    sugarsProp <-
                        mkSugarsProp ^. mkProperty
                        <&> Property.pSet . Lens.mapped %~ (<* writeIORef cacheRef Nothing)
                    env <-
                        mkEnv cachedFunctions monitors fonts evaluator
                        configSampler mainEnv settingsProp sugarsProp
                    makeRootWidget env monitors db evaluator sample cacheRef
        reportPerfCounters <- traverse makeReportPerfCounters ekg
        MainLoop.run mainLoop win MainLoop.Handlers
            { makeWidget = makeWidget
            , options =
                mainLoopOptions mkSettingsProp configSampler getFonts
                stateStorage reportPerfCounters
            }

makeMainGui ::
    [TitledSelection Folder.Theme] -> [TitledSelection Folder.Language] ->
    (forall a. T DbLayout.DbM a -> IO a) ->
    Env -> GUIMain.Model Env DbLayout.ViewM ->
    OnceT (T DbLayout.DbM) (Widget IO)
makeMainGui themeNames langNames dbToIO env mkWorkArea =
    GUIMain.make themeNames langNames (env ^. Env.settings) (env ^. Env.sugars) env mkWorkArea
    <&> Widget.updates %~
    \act ->
    act ^. ioTrans . Lens._Wrapped
    <&> (^. Lens._Wrapped)
    >>= dbToIO
    >>= runExtraIO
    where
        runExtraIO (extraAct, res) = res <$ extraAct

backgroundId :: M.ElemId
backgroundId = "background"

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

data EditorCache = EditorCache
    { ecState :: OnceState
    , ecMkWorkArea :: GUIMain.Model Env DbLayout.ViewM
    , ecVer :: Version DbM
    }

initCache :: Transaction.Store DbM -> Env -> IORef (Maybe EditorCache) -> IO EditorCache
initCache db env cacheRef =
    do
        curVer <- DbLayout.runDbTransaction db VersionControl.getVersion
        readIORef cacheRef
            >>=
            \case
            Just cache | curVer == ecVer cache -> pure cache
            _ ->
                do
                    (x, s) <-
                        sugarWorkArea env & once
                        & runOnceT mempty
                        & VersionControl.runAction
                        & DbLayout.runDbTransaction db
                    let r = EditorCache
                            { ecState = s
                            , ecMkWorkArea = x
                            , ecVer = curVer
                            }
                    writeIORef cacheRef (Just r)
                    pure r

withCache ::
    Transaction.Store DbM -> Env -> IORef (Maybe EditorCache) ->
    (EditorCache -> OnceT IO a) ->
    IO a
withCache db env cacheRef action =
    do
        cache <- initCache db env cacheRef
        (r, s) <- action cache & runOnceT (ecState cache)
        writeIORef cacheRef (Just cache { ecState = s })
        pure r

addBackground :: Element.Element t => Env -> t -> t
addBackground env widget =
    M.backgroundColor (env ^. Env.theme . Theme.backgroundColor) (Element.pad 0 1000 widget env) backgroundId

makeRootWidget ::
    HasCallStack =>
    Env -> Debug.Monitors ->
    Transaction.Store DbM -> EvalManager.Evaluator -> ConfigSampler.Sample ->
    IORef (Maybe EditorCache) ->
    IO (Widget IO)
makeRootWidget env perfMonitors db evaluator sample cacheRef =
    do
        themeNames <-
            Folder.getSelections (Proxy @Theme)
            >>= traverse (titledThemeSelection selectedLang)
        langNames <-
            Folder.getSelections (Proxy @Language)
            >>= traverse titledLangSelection
            <&> sortOn (^. GUIMain.title)
        withCache db env cacheRef
            (\cache ->
                makeMainGui themeNames langNames dbToIO env (ecMkWorkArea cache)
                & _OnceT %~ mapStateT dbToIO
            )
            <&> addBackground env
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

setIcon :: GLFW.Window -> Image.Bitmap Bitmap.Word8 -> IO ()
setIcon win image =
    GLFW.setWindowIcon win [GLFW.mkImage width height getPixel]
    where
        getPixel x y =
            case BitmapPixels.unsafeReadPixel image (x, y) of
            [a] -> (a,a,a,a)
            [a, l] -> (a,a,a,l)
            [r, g, b] -> (r,g,b,1)
            [r, g, b, a] -> (r,g,b,a)
            _ -> (0, 0, 0, 0)
        (width, height) = Bitmap.bitmapSize image

run ::
    HasCallStack =>
    Opts.EditorOpts -> Transaction.Store DbM -> IO ()
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
        cacheRef <- newIORef Nothing
        let invalidate _ =
                do
                    writeIORef cacheRef Nothing
                    refresh
        configSampler <- ConfigSampler.new invalidate initialTheme initialLanguage
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

                lamduIconFile <- Paths.getDataFileName "Lamdu.png"
                eLamduIcon <- Image.loadImage lamduIconFile
                case eLamduIcon of
                    Left err -> "Failed to load Lamdu icon: " ++ err & hPutStrLn stderr
                    Right icon -> setIcon win icon

                printGLVersion
                evaluator <- newEvaluator refresh dbMVar opts
                mkSettingsProp <-
                    EditorSettings.newProp initialSettings configSampler evaluator
                mkSugarsProp <- pure True & newIORef <&> Property.fromIORef
                runMainLoop ekg stateStorage subpixel win mainLoop
                    configSampler evaluator db mkSettingsProp mkSugarsProp cache
                    cachedFunctions monitors cacheRef
    where
        subpixel
            | opts ^. Opts.eoSubpixelEnabled = Font.LCDSubPixelEnabled
            | otherwise = Font.LCDSubPixelDisabled
