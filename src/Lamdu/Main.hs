{-# LANGUAGE OverloadedStrings, Rank2Types#-}
module Main(main) where

import Control.Arrow (second)
import Control.Lens ((^.))
import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, runStateT, mapStateT)
import Data.ByteString (unpack)
import Data.Cache (Cache)
import Data.IORef
import Data.List(intercalate)
import Data.Monoid(Monoid(..))
import Data.Store.Db (Db)
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2 (Vector2(..))
import Data.Word(Word8)
import Graphics.DrawingCombinators((%%))
import Graphics.UI.Bottle.Animation(AnimId)
import Graphics.UI.Bottle.MainLoop(mainLoopWidget)
import Graphics.UI.Bottle.Widget(Widget)
import Lamdu.CodeEdit.Settings (Settings(..))
import Lamdu.WidgetEnvT (runWidgetEnvT)
import Numeric (showHex)
import Paths_lamdu (getDataFileName)
import System.Environment (getArgs)
import System.FilePath ((</>))
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import qualified Data.Cache as Cache
import qualified Data.Map as Map
import qualified Data.Store.Db as Db
import qualified Data.Vector.Vector2 as Vector2
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.Bottle.Widgets.FlyNav as FlyNav
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils
import qualified Lamdu.Anchors as Anchors
import qualified Lamdu.BranchGUI as BranchGUI
import qualified Lamdu.CodeEdit as CodeEdit
import qualified Lamdu.CodeEdit.Settings as Settings
import qualified Lamdu.Config as Config
import qualified Lamdu.ExampleDB as ExampleDB
import qualified Lamdu.VersionControl as VersionControl
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds
import qualified System.Directory as Directory

data ParsedOpts = ParsedOpts
  { shouldDeleteDB :: Bool
  , mFontPath :: Maybe FilePath
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
  when (shouldDeleteDB opts) $
    Directory.removeDirectoryRecursive lamduDir
  Directory.createDirectoryIfMissing False lamduDir
  -- GLFW changes the directory from start directory, at least on macs.
  startDir <- Directory.getCurrentDirectory

  GLFWUtils.withGLFW $ do
    Vector2 displayWidth displayHeight <- GLFWUtils.getVideoModeSize
    GLFWUtils.openWindow GLFW.defaultDisplayOptions
      { GLFW.displayOptions_width = displayWidth
      , GLFW.displayOptions_height = displayHeight
      }
    -- Fonts must be loaded after the GL context is created..
    let
      getFont path = do
        exists <- Directory.doesFileExist path
        unless exists . ioError . userError $ path ++ " does not exist!"
        Draw.openFont path
    font <-
      case mFontPath opts of
      Nothing ->
        (getFont =<< getDataFileName "fonts/DejaVuSans.ttf")
        `E.catch` \(E.SomeException _) ->
        getFont $ startDir </> "fonts/DejaVuSans.ttf"
      Just path -> getFont path
    Db.withDb (lamduDir </> "codeedit.db") $ runDb font

rjust :: Int -> a -> [a] -> [a]
rjust len x xs = replicate (length xs - len) x ++ xs

encodeHex :: [Word8] -> String
encodeHex = concatMap (rjust 2 '0' . (`showHex` ""))

drawAnimId :: Draw.Font -> AnimId -> DrawUtils.Image
drawAnimId font = DrawUtils.drawText font . intercalate "." . map (encodeHex . take 2 . unpack)

annotationSize :: Vector2 Draw.R
annotationSize = 5

addAnnotations :: Draw.Font -> Anim.Frame -> Anim.Frame
addAnnotations font = Lens.over Anim.fSubImages $ Map.mapWithKey annotateItem
  where
    annotateItem animId = map . second $ annotatePosImage animId
    annotatePosImage animId posImage =
      flip (Lens.over Anim.piImage) posImage . mappend .
      (Vector2.uncurry Draw.scale antiScale %%) .
      (Draw.translate (0, -1) %%) $
      drawAnimId font animId
      where
        -- Cancel out on the scaling done in Anim so
        -- that our annotation is always the same size
        antiScale =
          annotationSize /
          fmap (max 1) (posImage ^. Anim.piRect . Rect.size)

whenApply :: Bool -> (a -> a) -> a -> a
whenApply False _ = id
whenApply True f = f

mainLoopDebugMode
  :: Draw.Font
  -> (Widget.Size -> IO (Widget IO))
  -> (Widget.Size -> Widget IO -> IO (Widget IO)) -> IO a
mainLoopDebugMode font makeWidget addHelp = do
  debugModeRef <- newIORef False
  let
    getAnimHalfLife = do
      isDebugMode <- readIORef debugModeRef
      return $ if isDebugMode then 1.0 else 0.05
    addDebugMode widget = do
      isDebugMode <- readIORef debugModeRef
      let
        doc = EventMap.Doc $ "Debug Mode" : if isDebugMode then ["Disable"] else ["Enable"]
        set = writeIORef debugModeRef (not isDebugMode)
      return .
        whenApply isDebugMode (Lens.over Widget.wFrame (addAnnotations font)) $
        Widget.strongerEvents
        (Widget.keysEventMap Config.debugModeKeys doc set)
        widget
    makeDebugModeWidget size = addHelp size =<< addDebugMode =<< makeWidget size
  mainLoopWidget makeDebugModeWidget getAnimHalfLife

makeFlyNav :: IO (Widget IO -> IO (Widget IO))
makeFlyNav = do
  flyNavState <- newIORef FlyNav.initState
  return $ \widget -> do
    fnState <- readIORef flyNavState
    return .
      FlyNav.make WidgetIds.flyNav
      fnState (writeIORef flyNavState) $
      widget

makeSizeFactor :: IO (IORef (Vector2 Widget.R), Widget.EventHandlers IO)
makeSizeFactor = do
  factor <- newIORef 1
  let
    eventMap = mconcat
      [ Widget.keysEventMap Config.enlargeBaseFontKeys (EventMap.Doc ["View", "Zoom", "Enlarge"]) $
        modifyIORef factor (* Config.enlargeFactor)
      , Widget.keysEventMap Config.shrinkBaseFontKeys (EventMap.Doc ["View", "Zoom", "Shrink"]) $
        modifyIORef factor (/ Config.shrinkFactor)
      ]
  return (factor, eventMap)

runDb :: Draw.Font -> Db -> IO a
runDb font db = do
  ExampleDB.initDB db
  (sizeFactorRef, sizeFactorEvents) <- makeSizeFactor
  flyNavMake <- makeFlyNav
  addHelpWithStyle <- EventMapDoc.makeToggledHelpAdder Config.overlayDocKeys
  settingsRef <- newIORef Settings
    { _sInfoMode = Settings.InfoTypes
    }
  cacheRef <- newIORef $ Cache.new 0x100000 -- TODO: Use a real cache size
  let
    addHelp = addHelpWithStyle $ Config.helpStyle font
    makeWidget size = do
      cursor <- dbToIO $ Anchors.getP Anchors.cursor
      sizeFactor <- readIORef sizeFactorRef
      globalEventMap <- mkGlobalEventMap settingsRef
      let eventMap = globalEventMap `mappend` sizeFactorEvents
      prevCache <- readIORef cacheRef
      (widget, newCache) <-
        (`runStateT` prevCache) $
        mkWidgetWithFallback settingsRef (Config.baseStyle font) dbToIO
        (size / sizeFactor, cursor)
      writeIORef cacheRef newCache
      flyNavMake . Widget.scale sizeFactor $ Widget.weakerEvents eventMap widget

  mainLoopDebugMode font makeWidget addHelp
  where
    dbToIO = Anchors.runDbTransaction db

infoStr :: Settings.InfoMode -> String
infoStr Settings.InfoNone = "None"
infoStr Settings.InfoTypes = "Types"
infoStr Settings.InfoExamples = "Examples"

nextInfoMode :: Settings.InfoMode -> Settings.InfoMode
nextInfoMode Settings.InfoNone = Settings.InfoTypes
nextInfoMode Settings.InfoTypes = Settings.InfoExamples
nextInfoMode Settings.InfoExamples = Settings.InfoNone

mkGlobalEventMap :: IORef Settings -> IO (Widget.EventHandlers IO)
mkGlobalEventMap settingsRef = do
  settings <- readIORef settingsRef
  let
    curInfoMode = settings ^. Settings.sInfoMode
    next = nextInfoMode curInfoMode
    nextDoc = EventMap.Doc ["View", "Subtext", "Show " ++ infoStr next]
  return .
    Widget.keysEventMap Config.nextInfoMode nextDoc .
    modifyIORef settingsRef $ Lens.set Settings.sInfoMode next

mkWidgetWithFallback
  :: IORef Settings
  -> TextEdit.Style
  -> (forall a. Transaction Anchors.DbM a -> IO a)
  -> (Widget.Size, Widget.Id)
  -> StateT Cache IO (Widget IO)
mkWidgetWithFallback settingsRef style dbToIO (size, cursor) = do
  settings <- lift $ readIORef settingsRef
  (isValid, widget) <-
    mapStateT dbToIO $ do
      candidateWidget <- fromCursor settings cursor
      (isValid, widget) <-
        if candidateWidget ^. Widget.wIsFocused
        then return (True, candidateWidget)
        else do
          finalWidget <- fromCursor settings rootCursor
          lift $ Anchors.setP Anchors.cursor rootCursor
          return (False, finalWidget)
      unless (widget ^. Widget.wIsFocused) $
        fail "Root cursor did not match"
      return (isValid, widget)
  unless isValid . lift . putStrLn $ "Invalid cursor: " ++ show cursor
  return widget
  where
    fromCursor settings = makeRootWidget settings style dbToIO size
    rootCursor = WidgetIds.fromIRef Anchors.panesIRef

makeRootWidget
  :: Settings
  -> TextEdit.Style
  -> (forall a. Transaction Anchors.DbM a -> IO a)
  -> Widget.Size
  -> Widget.Id
  -> StateT Cache (Transaction Anchors.DbM) (Widget IO)
makeRootWidget settings style dbToIO size cursor = do
  actions <- lift VersionControl.makeActions
  mapStateT (runWidgetEnvT cursor style) $ do
    codeEdit <-
      (fmap . Widget.atEvents) (VersionControl.runEvent cursor) .
      (mapStateT . WE.mapWidgetEnvT) VersionControl.runAction $
      CodeEdit.make settings
    (fmap . Widget.atEvents) (dbToIO . (attachCursor =<<)) .
      lift $ BranchGUI.make id size actions codeEdit
  where
    attachCursor eventResult = do
      maybe (return ()) (Anchors.setP Anchors.cursor) $
        eventResult ^. Widget.eCursor
      return eventResult
