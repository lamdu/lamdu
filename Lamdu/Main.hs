{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, Rank2Types, RecordWildCards #-}
module Main
    ( main
    ) where

import           Prelude.Compat

import qualified Control.Exception as E
import           Control.Lens.Operators
import           Control.Monad (join, unless, replicateM_)
import           Data.IORef
import           Data.MRUMemo (memoIO)
import           Data.Maybe
import qualified Data.Monoid as Monoid
import           Data.Store.Db (Db)
import qualified Data.Store.IRef as IRef
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           GHC.Conc (setNumCapabilities, getNumProcessors)
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
import           System.IO (hPutStrLn, stderr)

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
                | otherwise         -> withDB $ runEditor _poMFontPath
    `E.catch` \e@E.SomeException{} -> do
    hPutStrLn stderr $ "Main exiting due to exception: " ++ show e
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

runEditor :: Maybe FilePath -> Db -> IO ()
runEditor mFontPath db =
    do
        -- GLFW changes the directory from start directory, at least on macs.
        startDir <- Directory.getCurrentDirectory

        -- Load config as early as possible, before we open any windows/etc
        configSampler <- ConfigSampler.new startDir

        GLFWUtils.withGLFW $ do
            win <- GLFWUtils.createWindow "Lamdu" =<< GLFWUtils.getVideoModeSize
            Font.with startDir mFontPath $ \font -> do
                -- Fonts must be loaded after the GL context is created..
                zoom <- Zoom.make =<< GLFWUtils.getDisplayScale win
                addHelpWithStyle <- EventMapDoc.makeToggledHelpAdder EventMapDoc.HelpNotShown
                settingsRef <- newIORef Settings
                    { _sInfoMode = Settings.defaultInfoMode
                    }
                wrapFlyNav <- FlyNav.makeIO Style.flyNav WidgetIds.flyNav
                invalidateCacheRef <- newIORef (return ())
                let invalidateCache = join (readIORef invalidateCacheRef)
                evaluators <- DefEvaluators.new invalidateCache db
                let onSettingsChange settings =
                        case settings ^. Settings.sInfoMode of
                        Settings.Evaluation -> DefEvaluators.start evaluators
                        _ -> DefEvaluators.stop evaluators
                let makeWidget (config, size) =
                        do
                            cursor <-
                                DbLayout.cursor DbLayout.revisionProps
                                & Transaction.getP
                                & DbLayout.runDbTransaction db
                            sizeFactor <- Zoom.getSizeFactor zoom
                            globalEventMap <- Settings.mkEventMap onSettingsChange config settingsRef
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
                            let dbToIO =
                                    case settings ^. Settings.sInfoMode of
                                    Settings.Evaluation ->
                                        DefEvaluators.runTransactionAndMaybeRestartEvaluators evaluators
                                    _ -> DbLayout.runDbTransaction db
                            widget <- mkWidgetWithFallback dbToIO env
                            return . Widget.scale sizeFactor $ Widget.weakerEvents eventMap widget
                (invalidateCacheAction, makeWidgetCached) <- cacheMakeWidget makeWidget
                refreshScheduler <- newRefreshScheduler
                writeIORef invalidateCacheRef $
                    do
                        invalidateCacheAction
                        scheduleRefresh refreshScheduler
                DefEvaluators.start evaluators

                mainLoop win refreshScheduler configSampler $
                    \config size ->
                    ( wrapFlyNav =<< makeWidgetCached (config, size)
                    , addHelpWithStyle (Style.help font (Config.help config)) size
                    )

newtype RefreshScheduler = RefreshScheduler (IORef Bool)
newRefreshScheduler :: IO RefreshScheduler
newRefreshScheduler = newIORef False <&> RefreshScheduler
shouldRefresh :: RefreshScheduler -> IO Bool
shouldRefresh (RefreshScheduler ref) = atomicModifyIORef ref $ \r -> (False, r)
scheduleRefresh :: RefreshScheduler -> IO ()
scheduleRefresh (RefreshScheduler ref) = writeIORef ref True

mainLoop ::
    GLFW.Window -> RefreshScheduler -> Sampler Config ->
    ( Config -> Widget.Size ->
        ( IO (Widget IO)
        , Widget IO -> IO (Widget IO)
        )
    ) -> IO ()
mainLoop win refreshScheduler configSampler iteration =
    do
        lastVersionNumRef <- newIORef 0
        let getAnimHalfLife =
                ConfigSampler.getConfig configSampler <&> Style.anim . snd
            makeWidget size =
                do
                    (_, config) <- ConfigSampler.getConfig configSampler
                    let (makeBaseWidget, addHelp) = iteration config size
                    addHelp =<< makeBaseWidget
            tickHandler =
                do
                    (curVersionNum, _) <- ConfigSampler.getConfig configSampler
                    configChanged <- atomicModifyIORef lastVersionNumRef $ \lastVersionNum ->
                        (curVersionNum, lastVersionNum /= curVersionNum)
                    if configChanged
                        then return True
                        else shouldRefresh refreshScheduler
        mainLoopWidget win tickHandler makeWidget getAnimHalfLife

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

rootCursor :: Widget.Id
rootCursor = WidgetIds.fromGuid $ IRef.guid $ DbLayout.panes DbLayout.codeIRefs

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

makeMainGui ::
    (forall a. Transaction DbLayout.DbM a -> f a) ->
    GUIMain.Env -> Transaction DbLayout.DbM (Widget f)
makeMainGui runTransaction env =
    GUIMain.make env rootCursor
    <&> Widget.events %~ runTransaction . (attachCursor =<<)
    where
        attachCursor eventResult =
            do
                maybe (return ()) (Transaction.setP (DbLayout.cursor DbLayout.revisionProps)) .
                    Monoid.getLast $ eventResult ^. Widget.eCursor
                return eventResult
