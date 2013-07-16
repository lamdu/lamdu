{-# LANGUAGE OverloadedStrings, Rank2Types#-}
module Main(main) where

import Control.Applicative ((<$>), (<*))
import Control.Concurrent (threadDelay, forkIO, ThreadId)
import Control.Concurrent.MVar
import Control.Lens.Operators
import Control.Monad (unless, forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, runStateT, mapStateT)
import Data.Cache (Cache)
import Data.IORef
import Data.MRUMemo(memoIO)
import Data.Monoid(Monoid(..))
import Data.Store.Db (Db)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.MainLoop(mainLoopWidget)
import Graphics.UI.Bottle.Widget(Widget)
import Lamdu.Config (Config)
import Lamdu.GUI.CodeEdit.Settings (Settings(..))
import Lamdu.GUI.WidgetEnvT (runWidgetEnvT)
import Paths_lamdu (getDataFileName)
import System.Environment (getArgs)
import System.FilePath ((</>))
import qualified Control.Exception as E
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Cache as Cache
import qualified Data.Monoid as Monoid
import qualified Data.Store.Db as Db
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.Bottle.Widgets.FlyNav as FlyNav
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.DbLayout as DbLayout
import qualified Lamdu.Data.ExampleDB as ExampleDB
import qualified Lamdu.GUI.CodeEdit as CodeEdit
import qualified Lamdu.GUI.CodeEdit.Settings as Settings
import qualified Lamdu.GUI.VersionControl as VersionControlGUI
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.VersionControl as VersionControl
import qualified System.Directory as Directory

-- TODO: Remove this
import Lamdu.Data.Infer ()
import Lamdu.Data.Infer.Load ()
import Lamdu.Data.Infer.Deref ()

data ParsedOpts = ParsedOpts
  { poShouldDeleteDB :: Bool
  , poMFontPath :: Maybe FilePath
  }

parseArgs :: [String] -> Either String ParsedOpts
parseArgs =
  go (ParsedOpts False Nothing)
  where
    go args [] = return args
    go (ParsedOpts _ mPath) ("-deletedb" : args) =
      go (ParsedOpts True mPath) args
    go _ ("-font" : []) = failUsage "-font must be followed by a font name"
    go (ParsedOpts delDB mPath) ("-font" : fn : args) =
      case mPath of
      Nothing -> go (ParsedOpts delDB (Just fn)) args
      Just _ -> failUsage "Duplicate -font arguments"
    go _ (arg : _) = failUsage $ "Unexpected arg: " ++ show arg
    failUsage msg = fail $ unlines [ msg, usage ]
    usage = "Usage: lamdu [-deletedb] [-font <filename>]"

main :: IO ()
main = do
  args <- getArgs
  home <- Directory.getHomeDirectory
  let lamduDir = home </> ".lamdu"
  opts <- either fail return $ parseArgs args
  if poShouldDeleteDB opts
    then do
      putStrLn "Deleting DB..."
      Directory.removeDirectoryRecursive lamduDir
    else runEditor lamduDir $ poMFontPath opts

loadConfig :: FilePath -> IO Config
loadConfig configPath = do
  eConfig <- Aeson.eitherDecode' <$> LBS.readFile configPath
  either (fail . (msg ++)) return eConfig
  where
    msg = "Failed to parse config file contents at " ++ show configPath ++ ": "

accessDataFile :: FilePath -> (FilePath -> IO a) -> FilePath -> IO a
accessDataFile startDir accessor fileName =
  (accessor =<< getDataFileName fileName)
  `E.catch` \(E.SomeException _) ->
  accessor $ startDir </> fileName

type Version = Int

sampler :: Eq a => IO a -> IO (ThreadId, IO (Version, a))
sampler sample = do
  ref <- newMVar . (,) 0 =<< E.evaluate =<< sample
  let
    updateMVar new =
      modifyMVar_ ref $ \(ver, old) -> return $
      if old == new
      then (ver, old)
      else (ver+1, new)
  tid <-
    forkIO . forever $ do
      threadDelay 200000
      (updateMVar =<< sample) `E.catch` \E.SomeException {} -> return ()
  return (tid, readMVar ref)

runEditor :: FilePath -> Maybe FilePath -> IO ()
runEditor lamduDir mFontPath = do
  Directory.createDirectoryIfMissing False lamduDir
  -- GLFW changes the directory from start directory, at least on macs.
  startDir <- Directory.getCurrentDirectory

  -- Load config as early as possible, before we open any windows/etc
  (_, getConfig) <- sampler $ accessDataFile startDir loadConfig "config.json"

  GLFWUtils.withGLFW $ do
    Vector2 displayWidth displayHeight <- GLFWUtils.getVideoModeSize
    win <- GLFWUtils.createWindow displayWidth displayHeight "Lamdu"
    -- Fonts must be loaded after the GL context is created..
    let
      getFont path = do
        exists <- Directory.doesFileExist path
        unless exists . ioError . userError $ path ++ " does not exist!"
        Draw.openFont path
    font <-
      case mFontPath of
      Nothing -> accessDataFile startDir getFont "fonts/DejaVuSans.ttf"
      Just path -> getFont path
    Db.withDb (lamduDir </> "codeedit.sophia") $ runDb win getConfig font


mainLoopDebugMode ::
  GLFW.Window ->
  IO (Version, Config) ->
  ( Config -> Widget.Size ->
    ( IO (Widget IO)
    , Widget IO -> IO (Widget IO)
    )
  ) -> IO a
mainLoopDebugMode win getConfig iteration = do
  debugModeRef <- newIORef False
  lastVersionNumRef <- newIORef 0
  let
    getAnimHalfLife = do
      isDebugMode <- readIORef debugModeRef
      return $ if isDebugMode then 1.0 else 0.05
    addDebugMode config widget = do
      isDebugMode <- readIORef debugModeRef
      let
        doc = EventMap.Doc $ "Debug Mode" : if isDebugMode then ["Disable"] else ["Enable"]
        set = writeIORef debugModeRef (not isDebugMode)
      return $
        -- whenApply isDebugMode (Widget.wFrame %~ addAnnotations font) $
        Widget.strongerEvents
        (Widget.keysEventMap (Config.debugModeKeys config) doc set)
        widget
    makeDebugModeWidget size = do
      (_, config) <- getConfig
      let (makeWidget, addHelp) = iteration config size
      addHelp =<< addDebugMode config =<< makeWidget
    tickHandler = do
      (curVersionNum, _) <- getConfig
      atomicModifyIORef lastVersionNumRef $ \lastVersionNum ->
        (curVersionNum, lastVersionNum /= curVersionNum)
  mainLoopWidget win tickHandler makeDebugModeWidget getAnimHalfLife

cacheMakeWidget :: Eq a => (a -> IO (Widget IO)) -> IO (a -> IO (Widget IO))
cacheMakeWidget mkWidget = do
  widgetCacheRef <- newIORef =<< memoIO mkWidget
  let invalidateCache = writeIORef widgetCacheRef =<< memoIO mkWidget
  return $ \x -> do
    mkWidgetCached <- readIORef widgetCacheRef
    Widget.atEvents (<* invalidateCache) <$>
      mkWidgetCached x

makeFlyNav :: IO (Widget IO -> IO (Widget IO))
makeFlyNav = do
  flyNavState <- newIORef FlyNav.initState
  return $ \widget -> do
    fnState <- readIORef flyNavState
    return $ FlyNav.make WidgetIds.flyNav fnState (writeIORef flyNavState) widget

makeScaleFactor :: IO (IORef (Vector2 Widget.R), Config -> Widget.EventHandlers IO)
makeScaleFactor = do
  factor <- newIORef 1
  let
    eventMap config = mconcat
      [ Widget.keysEventMap (Config.enlargeBaseFontKeys config)
        (EventMap.Doc ["View", "Zoom", "Enlarge"]) $
        modifyIORef factor (* realToFrac (Config.enlargeFactor config))
      , Widget.keysEventMap (Config.shrinkBaseFontKeys config)
        (EventMap.Doc ["View", "Zoom", "Shrink"]) $
        modifyIORef factor (/ realToFrac (Config.shrinkFactor config))
      ]
  return (factor, eventMap)

helpConfig :: Draw.Font -> Config -> EventMapDoc.Config
helpConfig font config =
  EventMapDoc.Config
  { EventMapDoc.configStyle =
    TextView.Style
    { TextView._styleColor = Config.helpTextColor config
    , TextView._styleFont = font
    , TextView._styleFontSize = Config.helpTextSize config
    }
  , EventMapDoc.configInputDocColor = Config.helpInputDocColor config
  , EventMapDoc.configBGColor = Config.helpBGColor config
  , EventMapDoc.configOverlayDocKeys = Config.overlayDocKeys config
  }

baseStyle :: Config -> Draw.Font -> TextEdit.Style
baseStyle config font = TextEdit.Style
 { TextEdit._sTextViewStyle =
   TextView.Style
     { TextView._styleColor = Config.baseColor config
     , TextView._styleFont = font
     , TextView._styleFontSize = Config.baseTextSize config
     }
  , TextEdit._sCursorColor = TextEdit.defaultCursorColor
  , TextEdit._sCursorWidth = TextEdit.defaultCursorWidth
  , TextEdit._sTextCursorId = WidgetIds.textCursorId
  , TextEdit._sBackgroundCursorId = WidgetIds.backgroundCursorId
  , TextEdit._sBackgroundColor = Config.cursorBGColor config
  , TextEdit._sEmptyUnfocusedString = ""
  , TextEdit._sEmptyFocusedString = ""
  }

runDb :: GLFW.Window -> IO (Version, Config) -> Draw.Font -> Db -> IO a
runDb win getConfig font db = do
  ExampleDB.initDB (Guid.augment "ExampleDB") db
  (sizeFactorRef, sizeFactorEvents) <- makeScaleFactor
  addHelpWithStyle <- EventMapDoc.makeToggledHelpAdder EventMapDoc.HelpNotShown
  settingsRef <- newIORef Settings
    { _sInfoMode = Settings.defaultInfoMode
    }
  cacheRef <- newIORef $ Cache.new 0x10000000
  wrapFlyNav <- makeFlyNav
  let
    makeWidget (config, size) = do
      cursor <- dbToIO . Transaction.getP $ DbLayout.cursor DbLayout.revisionProps
      sizeFactor <- readIORef sizeFactorRef
      globalEventMap <- mkGlobalEventMap config settingsRef
      let eventMap = globalEventMap `mappend` sizeFactorEvents config
      prevCache <- readIORef cacheRef
      (widget, newCache) <-
        (`runStateT` prevCache) $
        mkWidgetWithFallback config settingsRef (baseStyle config font) dbToIO
        (size / sizeFactor, cursor)
      writeIORef cacheRef newCache
      return . Widget.scale sizeFactor $ Widget.weakerEvents eventMap widget
  makeWidgetCached <- cacheMakeWidget makeWidget
  mainLoopDebugMode win getConfig $ \config size ->
    ( wrapFlyNav =<< makeWidgetCached (config, size)
    , addHelpWithStyle (helpConfig font config) size
    )
  where
    dbToIO = DbLayout.runDbTransaction db

nextInfoMode :: Settings.InfoMode -> Settings.InfoMode
nextInfoMode Settings.None = Settings.Types
nextInfoMode Settings.Types = Settings.None -- Settings.Examples
nextInfoMode Settings.Examples = Settings.None

mkGlobalEventMap :: Config -> IORef Settings -> IO (Widget.EventHandlers IO)
mkGlobalEventMap config settingsRef = do
  settings <- readIORef settingsRef
  let
    curInfoMode = settings ^. Settings.sInfoMode
    next = nextInfoMode curInfoMode
    nextDoc = EventMap.Doc ["View", "Subtext", "Show " ++ show next]
  return .
    Widget.keysEventMap (Config.nextInfoModeKeys config) nextDoc .
    modifyIORef settingsRef $ Settings.sInfoMode .~ next

mkWidgetWithFallback ::
  Config -> IORef Settings ->
  TextEdit.Style ->
  (forall a. Transaction DbLayout.DbM a -> IO a) ->
  (Widget.Size, Widget.Id) ->
  StateT Cache IO (Widget IO)
mkWidgetWithFallback config settingsRef style dbToIO (size, cursor) = do
  settings <- lift $ readIORef settingsRef
  (isValid, widget) <-
    mapStateT dbToIO $ do
      candidateWidget <- fromCursor settings cursor
      (isValid, widget) <-
        if candidateWidget ^. Widget.wIsFocused
        then return (True, candidateWidget)
        else do
          finalWidget <- fromCursor settings rootCursor
          lift $ Transaction.setP (DbLayout.cursor DbLayout.revisionProps) rootCursor
          return (False, finalWidget)
      unless (widget ^. Widget.wIsFocused) $
        fail "Root cursor did not match"
      return (isValid, widget)
  if isValid
    then return widget
    else do
      lift . putStrLn $ "Invalid cursor: " ++ show cursor
      widget
        & Widget.backgroundColor (Config.layerMax (Config.layers config))
          ["invalid cursor bg"] (Config.invalidCursorBGColor config)
        & return
  where
    fromCursor settings = makeRootWidget config settings style dbToIO size
    rootCursor = WidgetIds.fromGuid rootGuid

rootGuid :: Guid
rootGuid = IRef.guid $ DbLayout.panes DbLayout.codeIRefs

makeRootWidget ::
  Config -> Settings -> TextEdit.Style ->
  (forall a. Transaction DbLayout.DbM a -> IO a) ->
  Widget.Size -> Widget.Id ->
  StateT Cache (Transaction DbLayout.DbM) (Widget IO)
makeRootWidget config settings style dbToIO size cursor = do
  actions <- lift VersionControl.makeActions
  mapStateT (runWidgetEnvT cursor style config) $ do
    codeEdit <-
      (fmap . Widget.atEvents) (VersionControl.runEvent cursor) .
      (mapStateT . WE.mapWidgetEnvT) VersionControl.runAction $
      CodeEdit.make env rootGuid
    branchGui <- lift $ VersionControlGUI.make id size actions codeEdit
    let
      quitEventMap =
        Widget.keysEventMap (Config.quitKeys config) (EventMap.Doc ["Quit"]) (error "Quit")
    return .
      Widget.atEvents (dbToIO . (attachCursor =<<)) $
      Widget.strongerEvents quitEventMap branchGui
  where
    env = CodeEdit.Env
      { CodeEdit.codeProps = DbLayout.codeProps
      , CodeEdit.totalSize = size
      , CodeEdit.settings = settings
      }
    attachCursor eventResult = do
      maybe (return ()) (Transaction.setP (DbLayout.cursor DbLayout.revisionProps)) .
        Monoid.getLast $ eventResult ^. Widget.eCursor
      return eventResult
