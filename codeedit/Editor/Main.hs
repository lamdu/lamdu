{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Control.Arrow (second)
import Control.Monad (liftM, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (runWriterT)
import Data.ByteString (unpack)
import Data.IORef
import Data.List(intercalate)
import Data.MRUMemo (memo)
import Data.Monoid(Last(..), Monoid(..))
import Data.Vector.Vector2(Vector2)
import Data.Word(Word8)
import Editor.Anchors (DBTag)
import Editor.OTransaction (runOTransaction)
import Graphics.DrawingCombinators((%%))
import Graphics.UI.Bottle.Animation(AnimId)
import Graphics.UI.Bottle.MainLoop(mainLoopWidget)
import Graphics.UI.Bottle.Widget(Widget)
import Numeric (showHex)
import System.FilePath ((</>))
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
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Utils as DrawUtils
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Rect as Rect
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.Bottle.Widgets.FlyNav as FlyNav
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified System.Directory as Directory
import qualified System.Info

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

main :: IO ()
main = do
  home <- Directory.getHomeDirectory
  let bottleDir = home </> "bottle"
  Directory.createDirectoryIfMissing False bottleDir
  font <- Draw.openFont (defaultFont System.Info.os)
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
        antiScale = annotationSize / fmap (max 1) (Rect.rectSize (Anim.piRect posImage))

whenApply :: Bool -> (a -> a) -> a -> a
whenApply False _ = id
whenApply True f = f

mainLoopDebugMode :: Draw.Font -> IO (Widget IO) -> (Widget IO -> IO (Widget IO)) -> IO a
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
        whenApply isDebugMode (Widget.atFrame (addAnnotations font)) $
        Widget.strongerEvents (Widget.keysEventMap Config.debugModeKeys doc set) widget
    makeDebugModeWidget = addHelp =<< addDebugMode =<< makeWidget
  mainLoopWidget makeDebugModeWidget getAnimHalfLife

runDbStore :: Draw.Font -> Transaction.Store DBTag IO -> IO a
runDbStore font store = do
  ExampleDB.initDB store
  flyNavState <- newIORef FlyNav.initState
  addHelp <-
    EventMapDoc.makeToggledHelpAdder Config.overlayDocKeys helpStyle
  initCache <- Transaction.run store $ do
    view <- Anchors.getP Anchors.view
    liftM fromCacheCursor $
      Transaction.run (Anchors.viewStore view) CodeEdit.makeSugarCache
  initCursor <- Transaction.run store $ Anchors.getP Anchors.cursor
  cacheRef <- newIORef initCache

  let
    mkWidgetFromCursor fromCursor cursor = do
      candidateWidget <- fromCursor cursor
      (invalidCursor, widget) <-
        if Widget.isFocused candidateWidget
        then return (Nothing, candidateWidget)
        else do
          finalWidget <- fromCursor rootCursor
          Anchors.setP Anchors.cursor rootCursor
          return (Just cursor, finalWidget)
      unless (Widget.isFocused widget) $
        fail "Root cursor did not match"
      return (invalidCursor, widget)
  widgetCacheRef <-
    newIORef . (,) initCursor . snd =<<
    Transaction.run store (mkWidgetFromCursor initCache initCursor)
  let
    -- TODO: Move this logic to some more common place?
    makeWidget = do
      fromCursor <- readIORef cacheRef
      widget <- mkCacheDependentWidget fromCursor
      fnState <- readIORef flyNavState
      return .
        Widget.atMkSizeDependentWidgetData memo .
        FlyNav.make WidgetIds.flyNav
        fnState (writeIORef flyNavState) $
        Widget.atEvents saveCache widget

    mkCacheDependentWidget fromCursor = do
      (oldCursor, oldResult) <- readIORef widgetCacheRef
      (cursor, (invalidCursor, widget)) <- Transaction.run store $ do
        cursor <- Anchors.getP Anchors.cursor
        result <-
          if oldCursor == cursor
          then return (Nothing, oldResult)
          else mkWidgetFromCursor fromCursor cursor
        return (cursor, result)
      maybe (return ()) (putStrLn . ("Invalid cursor: " ++) . show)
        invalidCursor
      writeIORef widgetCacheRef (cursor, widget)
      return widget

    saveCache action = do
      (eventResult, mCacheCache) <- runWriterT action
      case mCacheCache of
        Last Nothing -> return ()
        Last (Just newCache) ->
          writeIORef cacheRef $ fromCacheCursor newCache
      return eventResult

  mainLoopDebugMode font makeWidget addHelp
  where
    rootCursor = WidgetIds.fromIRef Anchors.panesIRef
    helpStyle = TextView.Style {
      TextView.styleColor = Draw.Color 1 1 1 1,
      TextView.styleFont = font,
      TextView.styleFontSize = Config.helpTextSize
      }
    style = TextEdit.Style
      { TextEdit.sTextViewStyle =
        TextView.Style
          { TextView.styleColor = Draw.Color 1 1 1 1
          , TextView.styleFont = font
          , TextView.styleFontSize = Config.baseTextSize
          }
      , TextEdit.sCursorColor = TextEdit.defaultCursorColor
      , TextEdit.sCursorWidth = TextEdit.defaultCursorWidth
      , TextEdit.sTextCursorId = WidgetIds.textCursorId
      , TextEdit.sBackgroundCursorId = WidgetIds.backgroundCursorId
      , TextEdit.sEmptyUnfocusedString = ""
      , TextEdit.sEmptyFocusedString = ""
      }

    fromCacheCursor cache = memo $ \cursor ->
      -- Get rid of OTransaction/ITransaction wrappings
      liftM
        (Widget.atMkSizeDependentWidgetData memo .
         Widget.atEvents
         ((Writer.mapWriterT (Transaction.run store . IT.runITransaction)) .
          (lift . attachCursor =<<))) .
        runOTransaction cursor style $
        makeCodeEdit cache

    makeCodeEdit cache =
      BranchGUI.makeRootWidget CodeEdit.makeSugarCache $
      CodeEdit.makeCodeEdit cache

    attachCursor eventResult = do
      maybe (return ()) (IT.transaction . Anchors.setP Anchors.cursor) $ Widget.eCursor eventResult
      return eventResult
