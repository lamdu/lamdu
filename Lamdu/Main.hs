{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, Rank2Types, RecordWildCards, DeriveDataTypeable, ScopedTypeVariables, LambdaCase, BangPatterns #-}
module Main
    ( main
    ) where

import           Control.Concurrent.MVar
import qualified Control.Exception as E
import           Control.Lens.Operators
import           Control.Monad (when, join, unless, replicateM_)
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           Data.Maybe
import qualified Data.Monoid as Monoid
import           Data.Proxy (Proxy(..))
import           Data.Store.Db (Db)
import qualified Data.Store.IRef as IRef
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.Time.Clock (getCurrentTime)
import           Data.Typeable (Typeable)
import           GHC.Conc (setNumCapabilities, getNumProcessors)
import           GHC.Stack (whoCreated)
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.MainLoop (mainLoopWidget)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.Bottle.Widgets.FlyNav as FlyNav
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Sampler (Sampler)
import qualified Lamdu.Config.Sampler as ConfigSampler
import qualified Lamdu.Data.DbLayout as DbLayout
import qualified Lamdu.Data.ExampleDB as ExampleDB
import           Lamdu.DataFile (getLamduDir)
import qualified Lamdu.EvalManager as EvalManager
import           Lamdu.Font (Fonts(..))
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

import           Prelude.Compat

type T = Transaction

defaultFonts :: Fonts FilePath
defaultFonts =
    Fonts defaultFontName defaultFontName defaultFontName defaultFontName
    where
        defaultFontName = "fonts/Purisa.ttf"

main :: IO ()
main =
    do
        setNumCapabilities =<< getNumProcessors
        lamduDir <- getLamduDir
        opts <- either fail return =<< Opts.get
        let withDB = ExampleDB.withDB lamduDir
        case opts of
            Opts.Parsed{..}
                | _poShouldDeleteDB -> deleteDB lamduDir
                | _poUndoCount > 0  -> withDB $ undoN _poUndoCount
                | otherwise         -> withDB $ runEditor _poWindowMode
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

createWindow :: Opts.WindowMode -> IO GLFW.Window
createWindow Opts.VideoModeSize =
    GLFWUtils.getVideoModeSize >>=
    GLFWUtils.createWindow "Lamdu" Nothing
createWindow (Opts.WindowSize winSize) =
    GLFWUtils.createWindow "Lamdu" Nothing winSize
createWindow Opts.FullScreen =
    do
        mMonitor <- GLFW.getPrimaryMonitor
        GLFWUtils.getVideoModeSize >>= GLFWUtils.createWindow "Lamdu" mMonitor

settingsChangeHandler :: EvalManager.Evaluator -> Settings -> IO ()
settingsChangeHandler evaluator settings =
    case settings ^. Settings.sInfoMode of
    Settings.Evaluation -> EvalManager.start evaluator
    _ -> EvalManager.stop evaluator

data CachedWidgetInput = CachedWidgetInput
    { _cwiFontsVer :: Int
    , _cwiConfig :: Config
    , _cwiSize :: Widget.Size
    , _cwiFonts :: Fonts Draw.Font
    }

instance Eq CachedWidgetInput where
    CachedWidgetInput x0 y0 z0 _ == CachedWidgetInput x1 y1 z1 _ =
        (x0, y0, z0) == (x1, y1, z1)

makeRootWidget ::
    Db -> Zoom -> IORef Settings -> EvalManager.Evaluator ->
    CachedWidgetInput -> IO (Widget IO)
makeRootWidget db zoom settingsRef evaluator (CachedWidgetInput _fontsVer config size fonts) =
    do
        cursor <-
            DbLayout.cursor DbLayout.revisionProps
            & Transaction.getP
            & DbLayout.runDbTransaction db
        sizeFactor <- Zoom.getSizeFactor zoom
        globalEventMap <- Settings.mkEventMap (settingsChangeHandler evaluator) config settingsRef
        let eventMap = globalEventMap `mappend` Zoom.eventMap zoom (Config.zoom config)
        evalResults <- EvalManager.getResults evaluator
        settings <- readIORef settingsRef
        let env = GUIMain.Env
                { _envEvalRes = evalResults
                , _envConfig = config
                , _envSettings = settings
                , _envStyle = Style.style config fonts
                , _envFullSize = size / sizeFactor
                , _envCursor = cursor
                }
        let dbToIO =
                case settings ^. Settings.sInfoMode of
                Settings.Evaluation ->
                    EvalManager.runTransactionAndMaybeRestartEvaluator evaluator
                _ -> DbLayout.runDbTransaction db
        mkWidgetWithFallback dbToIO env
            <&> Widget.weakerEvents eventMap
            <&> Widget.scale sizeFactor

withMVarProtection :: a -> (MVar (Maybe a) -> IO b) -> IO b
withMVarProtection val =
    E.bracket (newMVar (Just val)) (\mvar -> modifyMVar_ mvar (\_ -> return Nothing))

runEditor :: Opts.WindowMode -> Db -> IO ()
runEditor windowMode db =
    do
        -- GLFW changes the directory from start directory, at least on macs.
        startDir <- Directory.getCurrentDirectory

        -- Load config as early as possible, before we open any windows/etc
        configSampler <- ConfigSampler.new startDir

        GLFWUtils.withGLFW $ do
            win <- createWindow windowMode
            -- Fonts must be loaded after the GL context is created..
            wrapFlyNav <- FlyNav.makeIO Style.flyNav WidgetIds.flyNav
            invalidateCacheRef <- newIORef (return ())
            let invalidateCache = join (readIORef invalidateCacheRef)
            withMVarProtection db $ \dbMVar ->
                do
                    evaluator <- EvalManager.new invalidateCache dbMVar
                    zoom <- Zoom.make =<< GLFWUtils.getDisplayScale win
                    let initialSettings = Settings Settings.defaultInfoMode
                    settingsRef <- newIORef initialSettings
                    settingsChangeHandler evaluator initialSettings
                    (invalidateCacheAction, makeRootWidgetCached) <-
                        makeRootWidget db zoom settingsRef evaluator
                        & memoizeMakeWidget
                    refreshScheduler <- newRefreshScheduler
                    writeIORef invalidateCacheRef $
                        do
                            invalidateCacheAction
                            scheduleRefresh refreshScheduler
                    addHelp <- EventMapDoc.makeToggledHelpAdder EventMapDoc.HelpNotShown
                    mainLoop win refreshScheduler configSampler $ \fontsVer fonts config size ->
                        makeRootWidgetCached (CachedWidgetInput fontsVer config size fonts)
                        >>= wrapFlyNav
                        >>= addHelp (Style.help (Font.fontDefault fonts) (Config.help config)) size

newtype RefreshScheduler = RefreshScheduler (IORef Bool)
newRefreshScheduler :: IO RefreshScheduler
newRefreshScheduler = newIORef False <&> RefreshScheduler
shouldRefresh :: RefreshScheduler -> IO Bool
shouldRefresh (RefreshScheduler ref) = atomicModifyIORef ref $ \r -> (False, r)
scheduleRefresh :: RefreshScheduler -> IO ()
scheduleRefresh (RefreshScheduler ref) = writeIORef ref True


data FontChanged = FontChanged
    deriving (Show, Typeable)
instance E.Exception FontChanged

loopWhileException :: forall a e. E.Exception e => Proxy e -> (Int -> IO a) -> IO a
loopWhileException _ act = loop 0
    where
        loop !n =
            (act n <&> Just)
            `E.catch` (\(_ :: e) -> return Nothing)
            >>= \case
            Nothing -> loop (n+1)
            Just res -> return res

prependConfigPath ::
    ConfigSampler.Sample Config ->
    Fonts FilePath ->
    Fonts FilePath
prependConfigPath sample =
    fmap (dir </>)
    where
        dir = FilePath.takeDirectory (ConfigSampler.sFilePath sample)

absFontsOfSample :: ConfigSampler.Sample Config -> Fonts FilePath
absFontsOfSample sample =
    prependConfigPath sample $ Config.fonts $ ConfigSampler.sValue sample

withFontLoop :: Sampler Config -> (Int -> IO () -> Fonts Draw.Font -> IO a) -> IO a
withFontLoop configSampler act =
    loopWhileException (Proxy :: Proxy FontChanged) $ \fontsVer -> do
        sample <- ConfigSampler.getSample configSampler
        let absFonts = absFontsOfSample sample
        let defaultFontsAbs = prependConfigPath sample defaultFonts
        let throwIfFontChanged =
                do
                    newAbsFonts <- ConfigSampler.getSample configSampler <&> absFontsOfSample
                    when (newAbsFonts /= absFonts) $ E.throwIO FontChanged
        let runAct = act fontsVer throwIfFontChanged
        res <-
            withFont (const (return Nothing)) absFonts $ \fonts ->
            Just <$> runAct fonts
        case res of
            Nothing -> withFont E.throwIO defaultFontsAbs runAct
            Just success -> return success
    where
        withFont err = Font.with (\x@E.SomeException{} -> err x)

mainLoop ::
    GLFW.Window -> RefreshScheduler -> Sampler Config ->
    (Int -> Fonts Draw.Font -> Config -> Widget.Size -> IO (Widget IO)) -> IO ()
mainLoop win refreshScheduler configSampler iteration =
    withFontLoop configSampler $ \fontsVer checkFonts fonts ->
    do
        lastVersionNumRef <- newIORef =<< getCurrentTime
        let getAnimHalfLife =
                ConfigSampler.getSample configSampler
                <&> Style.anim . ConfigSampler.sValue
            makeWidget size =
                do
                    config <-
                        ConfigSampler.getSample configSampler
                        <&> ConfigSampler.sValue
                    iteration fontsVer fonts config size
            tickHandler =
                do
                    checkFonts
                    curVersionNum <-
                        ConfigSampler.getSample configSampler
                        <&> ConfigSampler.sVersion
                    configChanged <- atomicModifyIORef lastVersionNumRef $ \lastVersionNum ->
                        (curVersionNum, lastVersionNum /= curVersionNum)
                    if configChanged
                        then return True
                        else shouldRefresh refreshScheduler
        mainLoopWidget win tickHandler makeWidget getAnimHalfLife

memoizeMakeWidget :: Eq a => (a -> IO (Widget IO)) -> IO (IO (), a -> IO (Widget IO))
memoizeMakeWidget mkWidget =
    do
        widgetCacheRef <- newIORef =<< memoIO mkWidget
        let invalidateCache = writeIORef widgetCacheRef =<< memoIO mkWidget
        return
            ( invalidateCache
            , \x ->
                readIORef widgetCacheRef
                >>= ($ x)
                <&> Widget.events %~ (<* invalidateCache)
            )

rootCursor :: Widget.Id
rootCursor = WidgetIds.fromGuid $ IRef.guid $ DbLayout.panes DbLayout.codeIRefs

mkWidgetWithFallback ::
    (forall a. T DbLayout.DbM a -> IO a) ->
    GUIMain.Env -> IO (Widget IO)
mkWidgetWithFallback dbToIO env =
    do
        (isValid, widget) <-
            dbToIO $
            do
                candidateWidget <- makeMainGui dbToIO env
                (isValid, widget) <-
                    if candidateWidget ^. Widget.isFocused
                    then return (True, candidateWidget)
                    else do
                        finalWidget <-
                            env & GUIMain.envCursor .~ rootCursor & makeMainGui dbToIO
                        Transaction.setP (DbLayout.cursor DbLayout.revisionProps) rootCursor
                        return (False, finalWidget)
                unless (widget ^. Widget.isFocused) $
                    fail "Root cursor did not match"
                return (isValid, widget)
        unless isValid $ putStrLn $ "Invalid cursor: " ++ show (env ^. GUIMain.envCursor)
        widget
            & Widget.backgroundColor
              (Config.layerMax (Config.layers config))
              ["background"] (bgColor isValid config)
            & return
    where
        config = env ^. GUIMain.envConfig
        bgColor False = Config.invalidCursorBGColor
        bgColor True = Config.backgroundColor

makeMainGui ::
    (forall a. T DbLayout.DbM a -> f a) ->
    GUIMain.Env -> T DbLayout.DbM (Widget f)
makeMainGui runTransaction env =
    GUIMain.make env rootCursor
    <&> Widget.events %~ runTransaction . (attachCursor =<<)
    where
        attachCursor eventResult =
            do
                maybe (return ()) (Transaction.setP (DbLayout.cursor DbLayout.revisionProps)) .
                    Monoid.getLast $ eventResult ^. Widget.eCursor
                return eventResult
