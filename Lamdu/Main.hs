{-# LANGUAGE OverloadedStrings, Rank2Types, RecordWildCards #-}
module Main
    ( main
    ) where

import           Control.Applicative (Applicative(..), (<*))
import           Control.Lens.Operators
import           Control.Monad (join, unless, replicateM_)
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           Data.Maybe
import           Data.Monoid (Monoid(..))
import qualified Data.Monoid as Monoid
import           Data.Store.Db (Db)
import           Data.Store.Guid (Guid)
import qualified Data.Store.IRef as IRef
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           GHC.Conc (setNumCapabilities, getNumProcessors)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as EventMap
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
import qualified Lamdu.DefEvaluators as DefEvaluators
import qualified Lamdu.Font as Font
import           Lamdu.GUI.CodeEdit.Settings (Settings(..))
import qualified Lamdu.GUI.CodeEdit.Settings as Settings
import qualified Lamdu.GUI.Main as GUIMain
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.GUI.Zoom as Zoom
import qualified Lamdu.Opts as Opts
import qualified Lamdu.Style as Style
import qualified Lamdu.VersionControl as VersionControl
import           Lamdu.VersionControl.Actions (mUndo)
import qualified System.Directory as Directory
import           System.FilePath ((</>))

undo :: Transaction DbLayout.DbM Widget.Id
undo =
    do
        actions <- VersionControl.makeActions
        fromMaybe (fail "Cannot undo any further") $ mUndo actions

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

main :: IO ()
main =
    do
        setNumCapabilities =<< getNumProcessors
        home <- Directory.getHomeDirectory
        let lamduDir = home </> ".lamdu"
        opts <- either fail return =<< Opts.get
        let withDB = ExampleDB.withDB lamduDir
        case opts of
            Opts.Parsed{..}
                | _poShouldDeleteDB -> deleteDB lamduDir
                | _poUndoCount > 0  -> withDB $ undoN _poUndoCount
                | otherwise         -> withDB $ runEditor _poMFontPath

runEditor :: Maybe FilePath -> Db -> IO ()
runEditor mFontPath db =
    do
        -- GLFW changes the directory from start directory, at least on macs.
        startDir <- Directory.getCurrentDirectory

        -- Load config as early as possible, before we open any windows/etc
        configSampler <- ConfigSampler.new startDir

        GLFWUtils.withGLFW $
            do
                win <- GLFWUtils.createWindow "Lamdu" =<< GLFWUtils.getVideoModeSize
                font <- Font.get startDir mFontPath
                -- Fonts must be loaded after the GL context is created..
                runDb win configSampler font db


newtype RefreshScheduler = RefreshScheduler (IORef Bool)
newRefreshScheduler :: IO RefreshScheduler
newRefreshScheduler = newIORef False <&> RefreshScheduler
shouldRefresh :: RefreshScheduler -> IO Bool
shouldRefresh (RefreshScheduler ref) = atomicModifyIORef ref $ \r -> (False, r)
scheduleRefresh :: RefreshScheduler -> IO ()
scheduleRefresh (RefreshScheduler ref) = writeIORef ref True

mainLoopDebugMode ::
    GLFW.Window -> RefreshScheduler -> Sampler Config ->
    ( Config -> Widget.Size ->
        ( IO (Widget IO)
        , Widget IO -> IO (Widget IO)
        )
    ) -> IO ()
mainLoopDebugMode win refreshScheduler configSampler iteration =
    do
        debugModeRef <- newIORef False
        lastVersionNumRef <- newIORef 0
        let getAnimHalfLife =
                do
                    (_, config) <- ConfigSampler.getConfig configSampler
                    isDebugMode <- readIORef debugModeRef
                    Style.anim config isDebugMode & return
            addDebugMode config widget =
                do
                    isDebugMode <- readIORef debugModeRef
                    let doc = EventMap.Doc $ "Debug Mode" : if isDebugMode then ["Disable"] else ["Enable"]
                        set = writeIORef debugModeRef (not isDebugMode)
                    return $
                        Widget.strongerEvents
                        (Widget.keysEventMap (Config.debugModeKeys config) doc set)
                        widget
            makeDebugModeWidget size =
                do
                    (_, config) <- ConfigSampler.getConfig configSampler
                    let (makeWidget, addHelp) = iteration config size
                    addHelp =<< addDebugMode config =<< makeWidget
            tickHandler =
                do
                    (curVersionNum, _) <- ConfigSampler.getConfig configSampler
                    configChanged <- atomicModifyIORef lastVersionNumRef $ \lastVersionNum ->
                        (curVersionNum, lastVersionNum /= curVersionNum)
                    if configChanged
                        then return True
                        else shouldRefresh refreshScheduler
        mainLoopWidget win tickHandler makeDebugModeWidget getAnimHalfLife

cacheMakeWidget :: Eq a => (a -> IO (Widget IO)) -> IO (IO (), a -> IO (Widget IO))
cacheMakeWidget mkWidget =
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

makeFlyNav :: IO (Widget IO -> IO (Widget IO))
makeFlyNav =
    do
        flyNavState <- newIORef FlyNav.initState
        return $ \widget ->
            do
                fnState <- readIORef flyNavState
                return $
                    FlyNav.make Style.flyNav WidgetIds.flyNav fnState (writeIORef flyNavState) widget

rootGuid :: Guid
rootGuid = IRef.guid $ DbLayout.panes DbLayout.codeIRefs

runDb :: GLFW.Window -> Sampler Config -> Draw.Font -> Db -> IO ()
runDb win configSampler font db =
    do
        zoom <- Zoom.make =<< GLFWUtils.getDisplayScale win
        addHelpWithStyle <- EventMapDoc.makeToggledHelpAdder EventMapDoc.HelpNotShown
        settingsRef <- newIORef Settings
            { _sInfoMode = Settings.defaultInfoMode
            }
        wrapFlyNav <- makeFlyNav
        invalidateCacheRef <- newIORef (return ())
        let invalidateCache = join (readIORef invalidateCacheRef)
        evaluators <-
            DefEvaluators.new invalidateCache (DbLayout.runDbTransaction db) $
            DbLayout.panes DbLayout.codeIRefs
        let makeWidget (config, size) =
                do
                    cursor <-
                        DbLayout.cursor DbLayout.revisionProps
                        & Transaction.getP
                        & DbLayout.runDbTransaction db
                    sizeFactor <- Zoom.getSizeFactor zoom
                    globalEventMap <- Settings.mkEventMap config settingsRef
                    let eventMap = globalEventMap `mappend` Zoom.eventMap zoom (Config.zoom config)
                    evalResults <- DefEvaluators.getResults evaluators
                    settings <- readIORef settingsRef
                    let env = GUIMain.Env
                            { envEvalMap = evalResults
                            , envConfig = config
                            , envSettings = settings
                            , envStyle = Style.base config font
                            , envFullSize = size / sizeFactor
                            , envCursor = cursor
                            }
                    let dbToIO = DefEvaluators.runTransactionAndMaybeRestartEvaluators evaluators
                    widget <- mkWidgetWithFallback dbToIO env
                    return . Widget.scale sizeFactor $ Widget.weakerEvents eventMap widget
        (invalidateCacheAction, makeWidgetCached) <- cacheMakeWidget makeWidget
        refreshScheduler <- newRefreshScheduler
        writeIORef invalidateCacheRef $
            do
                invalidateCacheAction
                scheduleRefresh refreshScheduler
        DefEvaluators.start evaluators

        mainLoopDebugMode win refreshScheduler configSampler $
            \config size ->
            ( wrapFlyNav =<< makeWidgetCached (config, size)
            , addHelpWithStyle (Style.help font (Config.help config)) size
            )

mkWidgetWithFallback ::
    (forall a. Transaction DbLayout.DbM a -> IO a) ->
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
                        finalWidget <- makeMainGui dbToIO env { GUIMain.envCursor = rootCursor }
                        Transaction.setP (DbLayout.cursor DbLayout.revisionProps) rootCursor
                        return (False, finalWidget)
                unless (widget ^. Widget.isFocused) $
                    fail "Root cursor did not match"
                return (isValid, widget)
        unless isValid $ putStrLn $ "Invalid cursor: " ++ show (GUIMain.envCursor env)
        widget
            & Widget.backgroundColor
              (Config.layerMax (Config.layers config))
              ["background"] (bgColor isValid config)
            & return
    where
        config = GUIMain.envConfig env
        bgColor False = Config.invalidCursorBGColor
        bgColor True = Config.backgroundColor
        rootCursor = WidgetIds.fromGuid rootGuid

makeMainGui ::
    (forall a. Transaction DbLayout.DbM a -> f a) ->
    GUIMain.Env -> Transaction DbLayout.DbM (Widget f)
makeMainGui runTransaction env =
    GUIMain.make env (WidgetIds.fromGuid rootGuid)
    <&> Widget.events %~ runTransaction . (attachCursor =<<)
    where
        attachCursor eventResult =
            do
                maybe (return ()) (Transaction.setP (DbLayout.cursor DbLayout.revisionProps)) .
                    Monoid.getLast $ eventResult ^. Widget.eCursor
                return eventResult
