{-# LANGUAGE OverloadedStrings, Rank2Types#-}
module Main(main) where

import Control.Applicative ((<*))
import Control.Arrow (second)
import Control.Lens ((^.))
import Control.Monad (liftM, unless, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT, runWriterT)
import Data.ByteString (unpack)
import Data.IORef
import Data.List(intercalate)
import Data.MRUMemo (memoIO)
import Data.Monoid(Last(..), Monoid(..))
import Data.Store.Transaction (Transaction)
import Data.Vector.Vector2(Vector2)
import Data.Word(Word8)
import Editor.Anchors (DBTag)
import Editor.OTransaction (runOTransaction)
import Graphics.DrawingCombinators((%%))
import Graphics.UI.Bottle.Animation(AnimId)
import Graphics.UI.Bottle.MainLoop(mainLoopWidget)
import Graphics.UI.Bottle.Widget(Widget)
import Numeric (showHex)
import Paths_bottle (getDataFileName)
import System.FilePath ((</>))
import qualified Control.Exception as E
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Map as Map
import qualified Data.Store.Db as Db
import qualified Data.Store.Transaction as Transaction
import qualified Data.Vector.Vector2 as Vector2
import qualified Editor.Anchors as Anchors
import qualified Editor.BranchGUI as BranchGUI
import qualified Editor.CodeEdit as CodeEdit
import qualified Editor.Config as Config
import qualified Editor.ExampleDB as ExampleDB
import qualified Editor.ITransaction as IT
import qualified Editor.OTransaction as OT
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.Bottle.Widgets.FlyNav as FlyNav
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified System.Directory as Directory

main :: IO ()
main = do
  home <- Directory.getHomeDirectory
  let bottleDir = home </> "bottle"
  Directory.createDirectoryIfMissing False bottleDir
  let
    getFont path = do
      exists <- Directory.doesFileExist path
      unless exists . ioError . userError $ path ++ " does not exist!"
      Draw.openFont path
  font <-
    (getFont =<< getDataFileName "fonts/DejaVuSans.ttf")
    `E.catch` \(E.SomeException _) ->
    getFont "fonts/DejaVuSans.ttf"
  Db.withDb (bottleDir </> "codeedit.db") $ runDbStore font . Anchors.dbStore

rjust :: Int -> a -> [a] -> [a]
rjust len x xs = replicate (length xs - len) x ++ xs

encodeHex :: [Word8] -> String
encodeHex = concatMap (rjust 2 '0' . (`showHex` ""))

drawAnimId :: Draw.Font -> AnimId -> DrawUtils.Image
drawAnimId font = DrawUtils.drawText font . intercalate "." . map (encodeHex . take 2 . unpack)

annotationSize :: Vector2 Draw.R
annotationSize = 5

addAnnotations :: Draw.Font -> Anim.Frame -> Anim.Frame
addAnnotations font = Anim.atFSubImages $ Map.mapWithKey annotateItem
  where
    annotateItem animId = map . second $ annotatePosImage animId
    annotatePosImage animId posImage =
      (`Anim.atPiImage` posImage) . mappend .
      (Vector2.uncurry Draw.scale antiScale %%) .
      (Draw.translate (0, -1) %%) $
      drawAnimId font animId
      where
        -- Cancel out on the scaling done in Anim so
        -- that our annotation is always the same size
        antiScale =
          annotationSize /
          fmap (max 1) (Anim.piRect posImage ^. Rect.size)

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
        doc = (if isDebugMode then "Disable" else "Enable") ++ " Debug Mode"
        set = writeIORef debugModeRef (not isDebugMode)
      return .
        whenApply isDebugMode (Widget.atWFrame (addAnnotations font)) $
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

-- Safely make an IORef whose initial value needs to refer to the same
-- IORef (for writes)
fixIORef :: ((a -> IO ()) -> IO a) -> IO (IORef a)
fixIORef mkInitial = do
  var <- newIORef undefined
  writeIORef var =<< mkInitial (writeIORef var)
  return var

makeSizeFactor :: IO (IORef (Vector2 Widget.R), Widget.EventHandlers IO)
makeSizeFactor = do
  factor <- newIORef 1
  let
    eventMap = mconcat
      [ Widget.keysEventMap Config.enlargeBaseFontKeys "Enlarge text" $
        modifyIORef factor (* Config.enlargeFactor)
      , Widget.keysEventMap Config.shrinkBaseFontKeys "Shrink text" $
        modifyIORef factor (/ Config.shrinkFactor)
      ]
  return (factor, eventMap)

runDbStore :: Draw.Font -> Transaction.Store DBTag IO -> IO a
runDbStore font store = do
  ExampleDB.initDB store
  (sizeFactorRef, sizeFactorEvents) <- makeSizeFactor
  flyNavMake <- makeFlyNav
  addHelpWithStyle <- EventMapDoc.makeToggledHelpAdder Config.overlayDocKeys
  settingsRef <- newIORef $ OT.Settings
    { OT._vsShowInferredTypes = True
    }
  let
    addHelp = addHelpWithStyle $ Config.helpStyle font
    updateCacheWith _             (Last Nothing) = return ()
    updateCacheWith writeNewCache (Last (Just newCache)) =
      writeNewCache newCache

    newMemoFromCache writeMemo sugarCache =
      memoIO .
      (liftM . fmap . Widget.weakerEvents)
      (fmap (<* writeCache sugarCache) sizeFactorEvents) $
      mkWidgetWithFallback settingsRef (Config.baseStyle font) dbToIO (updateCacheWith writeCache) sugarCache
      where
        writeCache = writeMemo <=< newMemoFromCache writeMemo

  initSugarCache <- dbToIO $ viewToDb CodeEdit.makeSugarCache
  memoRef <- fixIORef $ \writeMemo -> newMemoFromCache writeMemo initSugarCache

  let
    makeWidget size = do
      mkWidget <- readIORef memoRef
      cursor <- dbToIO $ Anchors.getP Anchors.cursor
      sizeFactor <- readIORef sizeFactorRef
      eventMap <- globalEventMap settingsRef
      flyNavMake =<<
        liftM (Widget.scale sizeFactor . Widget.weakerEvents eventMap)
        (mkWidget (size / sizeFactor, cursor))

  mainLoopDebugMode font makeWidget addHelp
  where
    dbToIO = Transaction.run store
    viewToDb act = do
      view <- Anchors.getP Anchors.view
      Transaction.run (Anchors.viewStore view) act

type SugarCache = CodeEdit.SugarCache (Transaction DBTag IO)

globalEventMap :: IORef OT.Settings -> IO (Widget.EventHandlers IO)
globalEventMap settingsRef = do
  settings <- readIORef settingsRef
  let
    togglePrefix
      | Lens.view OT.vsShowInferredTypes settings = "Disable"
      | otherwise = "Enable"
  return .
    Widget.keysEventMap Config.toggleShowInferredTypesKeys
    (togglePrefix ++ " showing inferred types") $
    (modifyIORef settingsRef . Lens.over OT.vsShowInferredTypes) not

mkWidgetWithFallback
  :: IORef OT.Settings
  -> TextEdit.Style
  -> (forall a. Transaction DBTag IO a -> IO a)
  -> (Last SugarCache -> IO ())
  -> SugarCache
  -> (Widget.Size, Widget.Id)
  -> IO (Widget IO)
mkWidgetWithFallback settingsRef style dbToIO updateCache sugarCache (size, cursor) = do
  settings <- readIORef settingsRef
  (isValid, widget) <-
    dbToIO $ do
      candidateWidget <- fromCursor settings cursor
      (isValid, widget) <-
        if Widget.wIsFocused candidateWidget
        then return (True, candidateWidget)
        else do
          finalWidget <- fromCursor settings rootCursor
          Anchors.setP Anchors.cursor rootCursor
          return (False, finalWidget)
      unless (Widget.wIsFocused widget) $
        fail "Root cursor did not match"
      return (isValid, widget)
  unless isValid . putStrLn $ "Invalid cursor: " ++ show cursor
  return $ Widget.atEvents (saveCache <=< runWriterT) widget
  where
    fromCursor settings = makeRootWidget settings style dbToIO sugarCache size
    saveCache (eventResult, mCacheCache) = do
      ~() <- updateCache mCacheCache
      return eventResult
    rootCursor = WidgetIds.fromIRef Anchors.panesIRef

makeRootWidget
  :: OT.Settings
  -> TextEdit.Style
  -> (forall a. Transaction DBTag IO a -> IO a)
  -> SugarCache
  -> Widget.Size
  -> Widget.Id
  -> Transaction DBTag IO (Widget (WriterT (Last SugarCache) IO))
makeRootWidget settings style dbToIO cache size cursor =
  liftM
    (Widget.atEvents
     (Writer.mapWriterT (dbToIO . IT.runITransaction) .
      (lift . attachCursor =<<))) .
    runOTransaction cursor style settings $
    makeCodeEdit cache
  where
    attachCursor eventResult = do
      maybe (return ()) (IT.transaction . Anchors.setP Anchors.cursor) $
        Widget.eCursor eventResult
      return eventResult
    makeCodeEdit =
      BranchGUI.makeRootWidget size CodeEdit.makeSugarCache .
      CodeEdit.makeCodeEdit
