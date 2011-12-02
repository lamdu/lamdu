{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, TypeOperators, TupleSections #-}
import Data.IORef
import Data.Maybe
import Data.Monoid (Monoid(..))
import Data.Record.Label((:->), lens)
import qualified Data.Record.Label as L
import Data.Vector.Vector2(Vector2(..))
import Graphics.UI.GLFWWidgets.Widgetable (Widgetable(..), Theme(..))
import Graphics.UI.GLFWWidgets.MainLoop (mainLoop)
import Graphics.UI.GLFWWidgets.SizeRange (Size)
import Graphics.UI.GLFWWidgets.Widget(Widget(..))
import qualified Graphics.DrawingCombinators as Draw -- TODO: Only needed for fonts...
import qualified Graphics.UI.GLFWWidgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.GLFWWidgets.GridView as GridView
import qualified Graphics.UI.GLFWWidgets.GridEdit as GridEdit
import qualified Graphics.UI.GLFWWidgets.TextEdit as TextEdit
import qualified Graphics.UI.GLFWWidgets.TextView as TextView
import qualified Graphics.UI.GLFWWidgets.Spacer as Spacer
import qualified Graphics.UI.GLFWWidgets.Widget as Widget
import qualified System.Info
import qualified Graphics.UI.GLFWWidgets.EventMap as E

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

makeTextView :: Theme -> Int -> [String] -> Widget k
makeTextView t ptSize textLines = TextView.makeWidget (TextEdit.themeFont (textEditTheme t)) ptSize textLines

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
    [[ makeTextView t 40 ["("],
       funcWidget, standardSpacer, argWidget,
       makeTextView t 40 [")"] ]]
    where
      funcWidget = fmap (modify applyFunc) $ toWidget t func
      argWidget = fmap (modify applyArg) $ toWidget t arg
      modify = set apply

type Model = ExpressionWithGUI

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

main :: IO ()
main = do
  font <- Draw.openFont (defaultFont System.Info.os)
  modelVar <-
    newIORef $
    mkApply (mkGetValue "launchMissiles") (mkGetValue "Mars")
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
    titleWidget = TextView.makeWidget font 60 ["The not-yet glorious structural code editor"]
    modelWidget = toWidget (theme font) model

updateModel :: Draw.Font -> Size -> E.Event -> Model -> Model
updateModel font size event model =
  fromMaybe model $
  E.lookup event =<< Widget.eventMap (widget font model) True size
