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
import           Graphics.UI.Bottle.MainLoop (mainLoopWidget, AnimConfig (..))
import           Graphics.UI.Bottle.SizedFont (SizedFont(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.Bottle.Widgets.FlyNav as FlyNav
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.DbLayout as DbLayout
import qualified Lamdu.Data.ExampleDB as ExampleDB
import           Lamdu.DataFile (accessDataFile)
import qualified Lamdu.DefEvaluators as DefEvaluators
import           Lamdu.GUI.CodeEdit.Settings (Settings(..))
import qualified Lamdu.GUI.CodeEdit.Settings as Settings
import qualified Lamdu.GUI.Main as GUIMain
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.GUI.Zoom as Zoom
import qualified Lamdu.Opts as Opts
import qualified Lamdu.VersionControl as VersionControl
import           Lamdu.VersionControl.Actions (mUndo)
import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified Lamdu.Config.Sampler as ConfigSampler

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
                -- Fonts must be loaded after the GL context is created..
                let getFont path =
                        do
                            exists <- Directory.doesFileExist path
                            unless exists . ioError . userError $ path ++ " does not exist!"
                            Draw.openFont path
                font <-
                    case mFontPath of
                    Nothing -> accessDataFile startDir getFont "fonts/DejaVuSans.ttf"
                    Just path -> getFont path
                runDb win (ConfigSampler.getConfig configSampler) font db


mainLoopDebugMode ::
    GLFW.Window ->
    IO Bool ->
    IO (ConfigSampler.Version, Config) ->
    ( Config -> Widget.Size ->
        ( IO (Widget IO)
        , Widget IO -> IO (Widget IO)
        )
    ) -> IO ()
mainLoopDebugMode win shouldRefresh getConfig iteration =
    do
        debugModeRef <- newIORef False
        lastVersionNumRef <- newIORef 0
        let getAnimHalfLife =
                do
                    isDebugMode <- readIORef debugModeRef
                    if isDebugMode
                        then return $ AnimConfig 6.64 0.01
                        else do
                            (_, config) <- getConfig
                            return $
                                AnimConfig
                                (realToFrac (Config.animationTimePeriodSec config))
                                (realToFrac (Config.animationRemainInPeriod config))
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
                    (_, config) <- getConfig
                    let (makeWidget, addHelp) = iteration config size
                    addHelp =<< addDebugMode config =<< makeWidget
            tickHandler =
                do
                    (curVersionNum, _) <- getConfig
                    configChanged <- atomicModifyIORef lastVersionNumRef $ \lastVersionNum ->
                        (curVersionNum, lastVersionNum /= curVersionNum)
                    if configChanged
                        then return True
                        else shouldRefresh
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

flyNavConfig :: FlyNav.Config
flyNavConfig = FlyNav.Config
    { FlyNav.configLayer = -10000 -- that should cover it :-)
    }

makeFlyNav :: IO (Widget IO -> IO (Widget IO))
makeFlyNav =
    do
        flyNavState <- newIORef FlyNav.initState
        return $ \widget ->
            do
                fnState <- readIORef flyNavState
                return $
                    FlyNav.make flyNavConfig WidgetIds.flyNav fnState (writeIORef flyNavState) widget

helpConfig :: Draw.Font -> Config.Help -> EventMapDoc.Config
helpConfig font Config.Help{..} =
    EventMapDoc.Config
    { EventMapDoc.configStyle =
        TextView.Style
        { TextView._styleColor = helpTextColor
        , TextView._styleFont = SizedFont font helpTextSize
        }
    , EventMapDoc.configInputDocColor = helpInputDocColor
    , EventMapDoc.configBGColor = helpBGColor
    , EventMapDoc.configOverlayDocKeys = helpKeys
    }

baseStyle :: Config -> Draw.Font -> TextEdit.Style
baseStyle config font = TextEdit.Style
    { TextEdit._sTextViewStyle =
        TextView.Style
        { TextView._styleColor = Config.baseColor config
        , TextView._styleFont = SizedFont font (Config.baseTextSize config)
        }
    , TextEdit._sCursorColor = TextEdit.defaultCursorColor
    , TextEdit._sCursorWidth = TextEdit.defaultCursorWidth
    , TextEdit._sTextCursorId = WidgetIds.textCursorId
    , TextEdit._sBGColor = Config.cursorBGColor config
    , TextEdit._sEmptyUnfocusedString = ""
    , TextEdit._sEmptyFocusedString = ""
    }

rootGuid :: Guid
rootGuid = IRef.guid $ DbLayout.panes DbLayout.codeIRefs

runDb ::
    GLFW.Window -> IO (ConfigSampler.Version, Config) -> Draw.Font -> Db -> IO ()
runDb win getConfig font db =
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
                    globalEventMap <- mkGlobalEventMap config settingsRef
                    let eventMap = globalEventMap `mappend` Zoom.eventMap zoom (Config.zoom config)
                    evalResults <- DefEvaluators.getResults evaluators
                    settings <- readIORef settingsRef
                    let env = GUIMain.Env
                            { envEvalMap = evalResults
                            , envConfig = config
                            , envSettings = settings
                            , envStyle = baseStyle config font
                            , envFullSize = size / sizeFactor
                            , envCursor = cursor
                            }
                    let dbToIO = DefEvaluators.runTransactionAndMaybeRestartEvaluators evaluators
                    widget <- mkWidgetWithFallback dbToIO env
                    return . Widget.scale sizeFactor $ Widget.weakerEvents eventMap widget
        (invalidateCacheAction, makeWidgetCached) <- cacheMakeWidget makeWidget
        refreshRef <- newIORef False
        let shouldRefresh = atomicModifyIORef refreshRef $ \r -> (False, r)
        writeIORef invalidateCacheRef $
            do
                invalidateCacheAction
                writeIORef refreshRef True
        DefEvaluators.start evaluators

        mainLoopDebugMode win shouldRefresh getConfig $ \config size ->
            ( wrapFlyNav =<< makeWidgetCached (config, size)
            , addHelpWithStyle (helpConfig font (Config.help config)) size
            )

mkGlobalEventMap :: Config -> IORef Settings -> IO (Widget.EventHandlers IO)
mkGlobalEventMap config settingsRef =
    do
        settings <- readIORef settingsRef
        let curInfoMode = settings ^. Settings.sInfoMode
            next = Settings.nextInfoMode curInfoMode
            nextDoc = EventMap.Doc ["View", "Subtext", "Show " ++ show next]
        return .
            Widget.keysEventMap (Config.nextInfoModeKeys config) nextDoc .
            modifyIORef settingsRef $ Settings.sInfoMode .~ next

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
