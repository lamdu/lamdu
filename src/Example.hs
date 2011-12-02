{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, TypeOperators, TupleSections #-}
import Control.Applicative ((<*>))
import Data.IORef (newIORef, modifyIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Record.Label((:->), lens)
import Data.Vector.Vector2(Vector2(..))
import Graphics.UI.GLFWWidgets.MainLoop (mainLoop)
import Graphics.UI.GLFWWidgets.Widget(Widget(..))
import Graphics.UI.GLFWWidgets.Widgetable (Widgetable(..), Theme(..))
import qualified Data.Record.Label as L
import qualified Graphics.DrawingCombinators as Draw -- TODO: Only needed for fonts...
import qualified Graphics.UI.GLFWWidgets.EventMap as E
import qualified Graphics.UI.GLFWWidgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.GLFWWidgets.GridEdit as GridEdit
import qualified Graphics.UI.GLFWWidgets.GridView as GridView
import qualified Graphics.UI.GLFWWidgets.Spacer as Spacer
import qualified Graphics.UI.GLFWWidgets.TextEdit as TextEdit
import qualified Graphics.UI.GLFWWidgets.TextView as TextView
import qualified Graphics.UI.GLFWWidgets.Widget as Widget
import qualified System.Info

type StringEdit = TextEdit.Model

data ExpressionWithGUI =
    Lambda { _lambdaParam :: StringEdit,
             _lambdaBody :: ExpressionWithGUI,
             _lambdaGridData :: GridEdit.Cursor }
  | Apply { _applyFunc :: ExpressionWithGUI,
            _applyArg :: ExpressionWithGUI,
            _applyGridData :: GridEdit.Cursor }
  | GetValue { _valueId :: StringEdit,
               _valueDelegating :: FocusDelegator.Cursor }
  | LiteralInt { _litValue :: StringEdit {- TODO: IntegerEdit -} }

$(L.mkLabels [''ExpressionWithGUI])

mkApply :: ExpressionWithGUI -> ExpressionWithGUI -> ExpressionWithGUI
mkApply func arg = Apply func arg (Vector2 1 0)

mkGetValue :: String -> ExpressionWithGUI
mkGetValue text = GetValue (TextEdit.Model (length text) text) False

standardSpacer :: Widget k
standardSpacer = Spacer.makeWidget (Vector2 1 1)

addArgKey :: (E.ModState, E.Key)
addArgKey = (E.noMods, E.charKey 'a')

set :: f -> (f :-> a) -> a -> f
set = flip (flip . L.setL)

makeTextView :: Theme -> [String] -> Widget k
makeTextView t textLines = TextView.makeWidget (themeFont t) (themeFontSize t) textLines

instance Widgetable ExpressionWithGUI where
  toWidget t getValue@(GetValue se delegating) =
    Widget.atMaybeEventMap (flip mappend $ Just addArg) .
    FocusDelegator.make (modify valueDelegating) delegating .
    fmap (modify valueId) $
    toWidget t se
    where
      addArg =
        E.fromEventType (uncurry E.KeyEventType addArgKey) $
        Apply getValue (GetValue (TextEdit.Model 0 "") True) (Vector2 3 0)
      modify = set getValue

  toWidget t apply@(Apply func arg cursor) =
    GridEdit.make (modify applyGridData) cursor
    [[ makeTextView t ["("],
       funcWidget, standardSpacer, argWidget,
       makeTextView t [")"] ]]
    where
      funcWidget = fmap (modify applyFunc) $ toWidget t func
      argWidget = fmap (modify applyArg) $ toWidget t arg
      modify = set apply

type Model = ExpressionWithGUI

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"
defaultBasePtSize :: Int
defaultBasePtSize = 30

main :: IO ()
main = do
  font <- Draw.openFont (defaultFont System.Info.os)
  modelVar <-
    newIORef $
    mkApply (mkGetValue "launchMissiles") (mkGetValue "Mars")
  let
    mkWidget model = widget font defaultBasePtSize model
    draw size = do
      model <- readIORef modelVar
      return $ Widget.image (mkWidget model) True size

    updateModel size event model w =
      fromMaybe model $
      E.lookup event =<< Widget.eventMap w True size

    eventHandler size event =
      modifyIORef modelVar $ updateModel size event <*> mkWidget

  mainLoop eventHandler draw

theme :: Draw.Font -> Int -> Theme
theme font ptSize = Theme font ptSize "<empty>"

widget :: Draw.Font -> Int -> Model -> Widget Model
widget font basePtSize model =
  GridView.makeFromWidgets
  [[ titleWidget ],
   [ modelWidget ]]
  where
    titleWidget = Widget.atImage (Draw.tint $ Draw.Color 1 0 1 1) $
                  TextView.makeWidget font (basePtSize * 2) ["The not-yet glorious structural code editor"]
    modelWidget = toWidget (theme font basePtSize) model
