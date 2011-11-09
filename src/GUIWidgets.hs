{-# OPTIONS -Wall #-}
import Prelude hiding (lookup)

import Control.Arrow (first, second)
import Data.IORef
import Data.List.Utils (enumerate2d, nth)
import Data.Maybe
import Data.Vector.Vector2(Vector2(..))
import SizeRange (Size)
import EventMap
import MainLoop (mainLoop)
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
