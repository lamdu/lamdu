{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, TypeOperators, TupleSections #-}
import Control.Applicative ((<*>))
import Data.IORef (newIORef, modifyIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Record.Label ((:->), lens)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.GLFWWidgets.MainLoop (mainLoop)
import Graphics.UI.GLFWWidgets.Widget (Widget(..))
import Graphics.UI.GLFWWidgets.Widgetable (Widgetable(..), Theme(..))
import Graphics.DrawingCombinators.Utils (backgroundColor)
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
             _lambdaCursor :: Box.Cursor,
             _lambdaDelegating :: FocusDelegator.Cursor }
  | Apply { _applyFunc :: ExpressionWithGUI,
            _applyArg :: ExpressionWithGUI,
            _applyCursor :: Box.Cursor,
            _applyDelegating :: FocusDelegator.Cursor }
  | GetValue { _valueId :: StringEdit,
               _valueDelegating :: FocusDelegator.Cursor }
--  | LiteralInt { _litValue :: StringEdit {- TODO: IntegerEdit -} }

$(L.mkLabels [''ExpressionWithGUI])

mkStringEdit :: String -> StringEdit
mkStringEdit text = TextEdit.Model (length text) text

mkApply :: ExpressionWithGUI -> ExpressionWithGUI -> ExpressionWithGUI
mkApply func arg = Apply func arg 0 True

mkGetValue :: String -> ExpressionWithGUI
mkGetValue text = GetValue (mkStringEdit text) False

mkLambda :: String -> ExpressionWithGUI -> ExpressionWithGUI
mkLambda param body = Lambda (mkStringEdit param) body 0 True

standardSpacer :: Widget k
standardSpacer = Spacer.makeWidget (Vector2 30 30)

addArgKey :: (E.ModState, E.Key)
addArgKey = (E.noMods, E.charKey 'a')

set :: f -> (f :-> a) -> a -> f
set record label val = L.setL label val record

makeTextView :: Theme -> [String] -> Widget k
makeTextView theme textLines = TextView.makeWidget (themeFont theme) (themeFontSize theme) textLines

empty :: Widget a
empty = Spacer.makeWidget (Vector2 0 0)

parens :: Theme -> (Widget a, Widget a)
parens theme = (makeTextView theme ["("], makeTextView theme [")"])

noParens :: (Widget a, Widget a)
noParens = (empty, empty)

type Scope = [String]

makeWidget :: Scope -> Theme -> ExpressionWithGUI -> Widget ExpressionWithGUI
makeWidget scope theme node@(GetValue se delegating) =
  (if inScope
    then id
    else Widget.atImageWithSize (backgroundColor (Draw.Color 1 0 0 0.5))) .
  Widget.atMaybeEventMap (flip mappend $ Just addArg) .
  FocusDelegator.make (modify valueDelegating) delegating .
  fmap (modify valueId) $
  toWidget theme se
  where
    inScope = TextEdit.modelText se `elem` scope
    addArg =
      E.fromEventType (uncurry E.KeyEventType addArgKey) $
      Apply node (GetValue (mkStringEdit "") True) 3 True
    modify = set node

makeWidget scope theme node@(Apply func arg cursor delegating) =
  FocusDelegator.make (modify applyDelegating) delegating $
  Box.make Box.horizontal (modify applyCursor) cursor
  [ funcWidget, standardSpacer, before, argWidget, after ]
  where
    (before, after) =
      case arg of
        Apply{} -> parens theme
        _       -> noParens
    funcWidget = fmap (modify applyFunc) $ makeWidget scope theme func
    argWidget = fmap (modify applyArg) $ makeWidget scope theme arg
    modify = set node

makeWidget scope theme node@(Lambda param body cursor delegating) =
  FocusDelegator.make (modify lambdaDelegating) delegating $
  Box.make Box.vertical (modify lambdaCursor) cursor [
    GridView.makeFromWidgets [[
      makeTextView theme ["λ"],
      paramWidget, standardSpacer,
      makeTextView theme ["→"]
    ]],
    GridView.makeFromWidgets [[
      standardSpacer,
      bodyWidget
    ]]
  ]
  where
    paramWidget = fmap (modify lambdaParam) $ toWidget theme param
    bodyWidget = fmap (modify lambdaBody) $ makeWidget (TextEdit.modelText param : scope) theme body
    modify = set node

type Model = ExpressionWithGUI

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"
defaultBasePtSize :: Int
defaultBasePtSize = 30

initialProgram :: ExpressionWithGUI
initialProgram = mkLambda "x" $ mkLambda "y" $ mkApply (mkGetValue "launchMissiles") (mkGetValue "x")

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
    modelWidget = makeWidget ["launchMissiles"] (mkTheme font basePtSize) model
