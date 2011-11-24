{-# OPTIONS -Wall #-}
import Prelude hiding (lookup)

import Control.Arrow (first, second)
import Data.IORef
import Data.List.Utils (enumerate2d, nth)
import Data.Maybe
import Data.Vector.Vector2(Vector2(..))
import Graphics.UI.GLFWWidgets.EventMap
import Graphics.UI.GLFWWidgets.MainLoop (mainLoop)
import Graphics.UI.GLFWWidgets.SizeRange (Size)
import Graphics.UI.GLFWWidgets.Widget(Widget(..))
import qualified Graphics.DrawingCombinators as Draw -- TODO: Only needed for fonts...
import qualified Graphics.UI.GLFWWidgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.GLFWWidgets.GridEdit as GridEdit
import qualified Graphics.UI.GLFWWidgets.TextEdit as TextEdit
import qualified Graphics.UI.GLFWWidgets.Widget as Widget
import qualified System.Info

type Model = ([[(FocusDelegator.Model, TextEdit.Model)]],
              GridEdit.Model)

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

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
      TextEdit.make (TextEdit.Theme font "<empty>") $
      teModel

    liftRowModel (rowIndex, colIndex) editCell =
      (first . nth rowIndex . nth colIndex) editCell origModel

updateModel :: Draw.Font -> Size -> Event -> Model -> Model
updateModel font size event model =
  fromMaybe model $
  lookup event =<< Widget.eventMap (widget font model) True size
