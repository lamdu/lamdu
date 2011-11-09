{-# OPTIONS -Wall #-}
import Prelude hiding (lookup)

import qualified GLFWWrap

import Control.Arrow (first, second)
import Data.IORef
import Data.List.Utils (enumerate2d, nth)
import Data.Maybe
import Data.Vector.Vector2(Vector2(..))
import SizeRange (Size)
import EventMap
import Graphics.DrawingCombinators((%%))
import Graphics.DrawingCombinators.Utils (Image)
import Graphics.UI.GLFW
import KeyHandlers(modifiersEventHandlerWrap)
import Typematic(typematicKeyHandlerWrap)
import Widget(Widget(..))
import qualified FocusDelegator
import qualified Graphics.DrawingCombinators as Draw
import qualified GridEdit
import qualified System.Info
import qualified TextEdit
import qualified Widget

type Model = ([[(FocusDelegator.Model, TextEdit.Model)]],
              GridEdit.Model)

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

mainLoop :: (Size -> Event -> IO ()) -> (Size -> IO Image) -> IO a
mainLoop eventHandler makeImage = GLFWWrap.withGLFW $ do
  GLFWWrap.openWindow defaultDisplayOptions

  let
    windowSize = do
      (x, y) <- getWindowDimensions
      return $ Vector2 (fromIntegral x) (fromIntegral y)

    eventHandlerWithSize event = (`eventHandler` event) =<< windowSize
  modifiersHandler <- modifiersEventHandlerWrap eventHandlerWithSize

  let
    keyHandler key isPress = modifiersHandler $ GLFWWrap.KeyEvent key isPress
    typematicTime x = 0.5 + fromIntegral x * 0.05

  typematicKeyHandler <- typematicKeyHandlerWrap typematicTime keyHandler

  let
    handleEvent (GLFWWrap.KeyEvent key isPress) = typematicKeyHandler key isPress
    handleEvent GLFWWrap.WindowClose = error "Quit" -- TODO: Make close event
    handleEvent x = modifiersHandler x

  GLFWWrap.eventLoop $ \events -> do
    winSize@(Vector2 winSizeX winSizeY) <- windowSize
    mapM_ handleEvent events
    Draw.clearRender .
      (Draw.scale (20/winSizeX) (-20/winSizeY) %%) =<<
      makeImage winSize

main :: IO ()
main = do
  font <- Draw.openFont (defaultFont System.Info.os)
  modelVar <- newIORef (replicate 3 . replicate 3 $ (False, TextEdit.Model 4 "Text"),
                        Vector2 0 0)
  let
    draw size = do
      model <- readIORef modelVar
      return $ Widget.image (widget font model) True size

    eventHandler size = modifyIORef modelVar . updateModel font size

  mainLoop eventHandler draw

widget :: Draw.Font -> Model -> Widget Model
widget font origModel@(rowModels, gModel) =
  GridEdit.make ((,) rowModels) gModel children
  where
    children = (map . map . uncurry) makeTextEdit . enumerate2d $ rowModels

    makeTextEdit index (fdModel, teModel) =
      FocusDelegator.make (liftRowModel index . first . const) fdModel .
      fmap (liftRowModel index . second . const) .
      TextEdit.make font "<empty>" 1 $
      teModel

    liftRowModel (rowIndex, colIndex) editCell =
      (first . nth rowIndex . nth colIndex) editCell origModel

updateModel :: Draw.Font -> Size -> Event -> Model -> Model
updateModel font size event model =
  fromMaybe model $
  lookup event =<< Widget.eventMap (widget font model) True size
