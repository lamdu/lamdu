{-# OPTIONS -Wall #-}
import Prelude hiding (lookup)

import qualified GLFWWrap

import Control.Arrow (first, second)
import Data.IORef
import Data.List.Utils (enumerate2d, nth)
import Data.Maybe
import Data.Vector.Vector2(Vector2(..))
import EventMap
import Graphics.DrawingCombinators((%%))
import Graphics.UI.GLFW
import SizeRange (Size)
import Widget(Widget(..))
import qualified FocusDelegator
import qualified Graphics.DrawingCombinators as Draw
import qualified GridEdit
import qualified System.Info
import qualified TextEdit
import qualified Widget
import Typematic(typematicKeyHandlerWrap)
import KeyHandlers(modifiersEventHandlerWrap)

type Model = ([[(FocusDelegator.Model, TextEdit.Model)]],
              GridEdit.Model)

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

main :: IO ()
main = GLFWWrap.withGLFW $ do
  font <- Draw.openFont (defaultFont System.Info.os)
  GLFWWrap.openWindow defaultDisplayOptions

  modelVar <- newIORef (replicate 3 . replicate 3 $ (False, TextEdit.Model 4 "Text"),
                        Vector2 0 0)
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
    Draw.clearRender .
      (Draw.scale (20/800) (-20/600) %%) .
      ($ fullSize) . ($ True) . Widget.image . widget font =<<
      readIORef modelVar

fullSize :: Size
fullSize = Vector2 800 600

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

updateModel :: Draw.Font -> Event -> Model -> Model
updateModel font event model =
  fromMaybe model $
  lookup event =<< Widget.eventMap (widget font model) True fullSize
