{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections #-}
import Prelude hiding (lookup)

import Data.IORef
import Data.Maybe
import Data.Vector.Vector2(Vector2(..))
import Graphics.UI.GLFWWidgets.EventMap
import Graphics.UI.GLFWWidgets.Widgetable (Theme(..), toWidget)
import Graphics.UI.GLFWWidgets.MainLoop (mainLoop)
import Graphics.UI.GLFWWidgets.SizeRange (Size)
import Graphics.UI.GLFWWidgets.Widget(Widget(..))
import qualified Graphics.DrawingCombinators as Draw -- TODO: Only needed for fonts...
import qualified Graphics.UI.GLFWWidgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.GLFWWidgets.GridView as GridView
import qualified Graphics.UI.GLFWWidgets.GridEdit as GridEdit
import qualified Graphics.UI.GLFWWidgets.TextEdit as TextEdit
import qualified Graphics.UI.GLFWWidgets.TextView as TextView
import qualified Graphics.UI.GLFWWidgets.Widget as Widget
import qualified System.Info

type Model = (GridEdit.Cursor,
              [[(FocusDelegator.Cursor, TextEdit.Model)]])

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

main :: IO ()
main = do
  font <- Draw.openFont (defaultFont System.Info.os)
  modelVar <- newIORef (Vector2 0 0, replicate 3 . replicate 3 $ (False, TextEdit.Model 4 "Text"))
  let
    draw size = do
      model <- readIORef modelVar
      return $ Widget.image (widget font model) True size

    eventHandler size = modifyIORef modelVar . updateModel font size

  mainLoop eventHandler draw

theme :: Draw.Font -> Theme
theme font = Theme $ TextEdit.Theme font "<empty>"

widget :: Draw.Font -> Model -> Widget Model
widget font model =
  GridView.makeFromWidgets
  [[ titleWidget ],
   [ modelWidget ]]
  where
    titleWidget = TextView.makeWidget font ["The not-yet glorious structural code editor"]
    modelWidget = toWidget (theme font) model

updateModel :: Draw.Font -> Size -> Event -> Model -> Model
updateModel font size event model =
  fromMaybe model $
  lookup event =<< Widget.eventMap (widget font model) True size
