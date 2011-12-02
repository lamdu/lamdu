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
import qualified Graphics.UI.GLFWWidgets.Box as Box
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
             _lambdaCursor :: Box.Cursor }
  | Apply { _applyFunc :: ExpressionWithGUI,
            _applyArg :: ExpressionWithGUI,
            _applyCursor :: Box.Cursor }
  | GetValue { _valueId :: StringEdit,
               _valueDelegating :: FocusDelegator.Cursor }
  | LiteralInt { _litValue :: StringEdit {- TODO: IntegerEdit -} }

$(L.mkLabels [''ExpressionWithGUI])

mkStringEdit :: String -> StringEdit
mkStringEdit text = TextEdit.Model (length text) text

mkApply :: ExpressionWithGUI -> ExpressionWithGUI -> ExpressionWithGUI
mkApply func arg = Apply func arg 1

mkGetValue :: String -> ExpressionWithGUI
mkGetValue text = GetValue (mkStringEdit text) False

mkLambda :: String -> ExpressionWithGUI -> ExpressionWithGUI
mkLambda param body = Lambda (mkStringEdit param) body 1

standardSpacer :: Widget k
standardSpacer = Spacer.makeWidget (Vector2 30 30)

addArgKey :: (E.ModState, E.Key)
addArgKey = (E.noMods, E.charKey 'a')

set :: f -> (f :-> a) -> a -> f
set record label val = L.setL label val record

makeTextView :: Theme -> [String] -> Widget k
makeTextView theme textLines = TextView.makeWidget (themeFont theme) (themeFontSize theme) textLines

instance Widgetable ExpressionWithGUI where
  toWidget theme node@(GetValue se delegating) =
    Widget.atMaybeEventMap (flip mappend $ Just addArg) .
    FocusDelegator.make (modify valueDelegating) delegating .
    fmap (modify valueId) $
    toWidget theme se
    where
      addArg =
        E.fromEventType (uncurry E.KeyEventType addArgKey) $
        Apply node (GetValue (TextEdit.Model 0 "") True) 3
      modify = set node

  toWidget theme node@(Apply func arg cursor) =
    Box.make Box.horizontal (modify applyCursor) cursor
    [ makeTextView theme ["("],
      funcWidget, standardSpacer, argWidget,
      makeTextView theme [")"] ]
    where
      funcWidget = fmap (modify applyFunc) $ toWidget theme func
      argWidget = fmap (modify applyArg) $ toWidget theme arg
      modify = set node

  toWidget theme node@(Lambda param body cursor) =
    Box.make Box.vertical (modify lambdaCursor) cursor [
      GridView.makeFromWidgets [[
        makeTextView theme ["(λ"],
        paramWidget, standardSpacer,
        makeTextView theme ["->"]
      ]],
      GridView.makeFromWidgets [[
        standardSpacer,
        bodyWidget,
        makeTextView theme [")"]
      ]]
    ]
    where
      paramWidget = fmap (modify lambdaParam) $ toWidget theme param
      bodyWidget = fmap (modify lambdaBody) $ toWidget theme body
      modify = set node

type Model = ExpressionWithGUI

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"
defaultBasePtSize :: Int
defaultBasePtSize = 30

initialProgram :: ExpressionWithGUI
initialProgram = mkLambda "x" $ mkApply (mkGetValue "launchMissiles") (mkGetValue "x")

main :: IO ()
main = do
  font <- Draw.openFont (defaultFont System.Info.os)
  modelVar <- newIORef initialProgram
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

mkTheme :: Draw.Font -> Int -> Theme
mkTheme font ptSize = Theme font ptSize "<empty>"

widget :: Draw.Font -> Int -> Model -> Widget Model
widget font basePtSize model =
  GridView.makeFromWidgets
  [[ titleWidget ],
   [ modelWidget ]]
  where
    titleWidget = Widget.atImage (Draw.tint $ Draw.Color 1 0 1 1) $
                  TextView.makeWidget font (basePtSize * 2) ["The not-yet glorious structural code editor"]
    modelWidget = toWidget (mkTheme font basePtSize) model
