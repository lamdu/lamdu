{-# OPTIONS -Wall #-}
import Prelude hiding (lookup)

import qualified GLFWWrap

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Applicative (pure)
import Control.Monad
import Data.IORef
import Data.Maybe
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Time.Clock
import Graphics.UI.GLFW
import qualified Graphics.DrawingCombinators as Draw
import Graphics.DrawingCombinators((%%))
import qualified System.Info
import EventMap
import qualified TextEdit
import qualified GridView
import           SizeRange                        (SizeRange(..))

data TypematicState = NoKey | TypematicRepeat { _tsKey :: Key, _tsCount :: Int, _tsStartTime :: UTCTime }

typematicKeyHandlerWrap :: (Int -> NominalDiffTime) -> (Key -> Bool -> IO ()) -> IO (Key -> Bool -> IO ())
typematicKeyHandlerWrap timeFunc handler = do
  stateVar <- newMVar NoKey
  _ <- forkIO . forever $ do
    sleepTime <- modifyMVar stateVar typematicIteration
    threadDelay . round $ 1000000 * sleepTime

  return $ \key isPress -> do
    newValue <-
      if isPress
        then fmap (TypematicRepeat key 0) getCurrentTime
        else return NoKey

    _ <- swapMVar stateVar newValue
    handler key isPress

  where
    typematicIteration state@(TypematicRepeat key count startTime) = do
      now <- getCurrentTime
      let timeDiff = diffUTCTime now startTime
      if timeDiff >= timeFunc count
        then do
          handler key True
          return (TypematicRepeat key (count + 1) startTime,
                  timeFunc (count + 1) - timeDiff)
        else
          return (state, timeFunc count - timeDiff)
    typematicIteration state@NoKey = return (state, timeFunc 0)

modStateFromKeySet :: Set Key -> ModState
modStateFromKeySet keySet =
  ModState {
    modCtrl = isPressed [KeyLeftCtrl, KeyRightCtrl],
    modMeta = False, -- TODO: GLFW doesn't support meta/winkey?
    modAlt = isPressed [KeyLeftAlt, KeyRightAlt],
    modShift = isPressed [KeyLeftShift, KeyRightShift]
    }
  where
    isPressed = any (`Set.member` keySet)

modifiersEventHandlerWrap :: (Event -> IO ()) -> IO (GLFWWrap.GLFWEvent -> IO ())
modifiersEventHandlerWrap wrappedHandler = do
  keySetVar <- newIORef Set.empty
  let
    handler (GLFWWrap.KeyEvent key True) = do
      modifyIORef keySetVar (Set.insert key)
      keySet <- readIORef keySetVar
      wrappedHandler $ KeyEvent (modStateFromKeySet keySet) key
    handler (GLFWWrap.KeyEvent key False) =
      modifyIORef keySetVar (Set.delete key)
    handler (GLFWWrap.CharEvent char True) = do
      keySet <- readIORef keySetVar
      when (modStateFromKeySet keySet `elem` [noMods, shift]) . wrappedHandler $ CharEvent char
    handler _ = return ()
  return handler

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

main :: IO ()
main = GLFWWrap.withGLFW $ do
  font <- Draw.openFont (defaultFont System.Info.os)
  GLFWWrap.openWindow defaultDisplayOptions

  modelVar <- newIORef (TextEdit.Model 4 "Text")
                        --TextEdit.Model 0 "Text"]

  modifiersHandler <- modifiersEventHandlerWrap (modifyIORef modelVar . updateModel font)

  let
    keyHandler key isPress = modifiersHandler $ GLFWWrap.KeyEvent key isPress
    typematicTime x = 0.5 + fromIntegral x * 0.05

  typematicKeyHandler <- typematicKeyHandlerWrap typematicTime keyHandler

  let
    handleEvent (GLFWWrap.KeyEvent key isPress) = typematicKeyHandler key isPress
    handleEvent GLFWWrap.WindowClose = error "Quit"
    handleEvent x = modifiersHandler x

  GLFWWrap.eventLoop $ \events -> do
    mapM_ handleEvent events
    Draw.clearRender . (Draw.scale (20/800) (20/600) %%) . fst . widget font =<< readIORef modelVar

widget :: Draw.Font -> TextEdit.Model -> (Draw.Image (), EventMap TextEdit.Model)
widget font model = (snd grid (pure 0), eventMap)
  where
    grid = GridView.make $ replicate 3 [textEdit]
    textEdit = (SizeRange (pure 2) (pure Nothing), const img)
    (img, eventMap) = TextEdit.make font "<empty>" 2 model

updateModel :: Draw.Font -> Event -> TextEdit.Model -> TextEdit.Model
updateModel font event model =
  fromMaybe model .
  lookup event .
  snd $
  widget font model
