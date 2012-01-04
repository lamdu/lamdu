{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, TypeOperators, TupleSections #-}
import Control.Applicative ((<*>))
import Data.IORef (newIORef, modifyIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Record.Label ((:->), lens)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.DrawingCombinators.Utils (backgroundColor)
import Graphics.UI.Bottle.MainLoop (mainLoop)
import Graphics.UI.Bottle.Widget (Widget(..))
import Graphics.UI.Bottle.Theme (Theme(..))
import qualified Data.Record.Label as L
import qualified Data.Record.Label.Tuple as LabelTuple
import qualified Graphics.DrawingCombinators as Draw -- TODO: Only needed for fonts...
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.Bottle.Widgets.Spacer as Spacer
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified System.Info

type StringEdit = TextEdit.Model

type DelegatedStringEdit = (FocusDelegator.Cursor, StringEdit)

data ExpressionWithGUI =
    Lambda { _lambdaParam :: StringEdit,
             _lambdaBody :: ExpressionWithGUI,
             _lambdaCursor :: Box.Cursor,
             _lambdaDelegating :: FocusDelegator.Cursor }
  | Apply { _applyFunc :: ExpressionWithGUI,
            _applyArg :: ExpressionWithGUI,
            _applyCursor :: Box.Cursor,
            _applyDelegating :: FocusDelegator.Cursor }
  | GetValue { _valueId :: DelegatedStringEdit }
--  | LiteralInt { _litValue :: StringEdit {- TODO: IntegerEdit -} }

$(L.mkLabels [''ExpressionWithGUI])

mkStringEdit :: String -> StringEdit
mkStringEdit text = TextEdit.Model (length text) text

mkApply :: ExpressionWithGUI -> ExpressionWithGUI -> ExpressionWithGUI
mkApply func arg = Apply func arg 0 True

mkGetValue :: String -> ExpressionWithGUI
mkGetValue text = GetValue (False, mkStringEdit text)

mkLambda :: String -> ExpressionWithGUI -> ExpressionWithGUI
mkLambda param body = Lambda (mkStringEdit param) body 0 True

standardSpacer :: Widget k
standardSpacer = Spacer.makeWidget (Vector2 30 30)

addArgKey :: (E.ModState, E.Key)
addArgKey = (E.noMods, E.charKey 'a')

set :: f -> (f :-> a) -> a -> f
set record label val = L.setL label val record

makeTextView :: Theme -> [String] -> Widget k
makeTextView theme = TextView.makeWidget (TextView.Style (themeFont theme) (themeFontSize theme))

empty :: Widget a
empty = Spacer.makeWidget (Vector2 0 0)

parens :: Theme -> (Widget a, Widget a)
parens theme = (makeTextView theme ["("], makeTextView theme [")"])

noParens :: (Widget a, Widget a)
noParens = (empty, empty)

type Scope = [String]

makeStringEditWidget :: Theme -> StringEdit -> Widget StringEdit
makeStringEditWidget theme =
  TextEdit.make
    (TextView.Style (themeFont theme)
                    (themeFontSize theme))
    (themeEmptyString theme)

makeStringEditWidgetWithLabel :: Theme -> (model :-> StringEdit) -> model -> Widget model
makeStringEditWidgetWithLabel theme label model =
  TextEdit.makeWithLabel
    (TextView.Style (themeFont theme) (themeFontSize theme))
    (themeEmptyString theme)
    label model

makeDelegatedStringEditWidget :: Theme -> DelegatedStringEdit -> Widget DelegatedStringEdit
makeDelegatedStringEditWidget theme delegatedStringEdit =
  FocusDelegator.makeWithLabel LabelTuple.first delegatedStringEdit $
  makeStringEditWidgetWithLabel theme LabelTuple.second delegatedStringEdit

makeWidget :: Scope -> Theme -> ExpressionWithGUI -> Widget ExpressionWithGUI
makeWidget scope theme node =
  Widget.removeExtraSize .
  Widget.atMaybeEventMap (flip mappend $ Just addArg) $
  makeWidgetFor scope theme node
  where
    addArg =
      E.fromEventType (uncurry E.KeyEventType addArgKey) $
      Apply node (GetValue (True, mkStringEdit "")) 3 True

makeFocusDelegator ::
  (FocusDelegator.Cursor -> k) -> FocusDelegator.Cursor ->
  Widget k -> Widget k
makeFocusDelegator = FocusDelegator.makeWithKeys (shiftK E.KeyRight) (shiftK E.KeyLeft)
  where
    shiftK = E.KeyEventType E.shift

makeWidgetFor :: Scope -> Theme -> ExpressionWithGUI -> Widget ExpressionWithGUI
makeWidgetFor scope theme node@(GetValue se) =
  (if inScope
    then id
    else Widget.removeExtraSize .
         Widget.atImageWithSize (backgroundColor (Draw.Color 1 0 0 0.5))) .
  fmap (modify valueId) $
  makeDelegatedStringEditWidget theme se
  where
    inScope = TextEdit.modelText (snd se) `elem` scope
    modify = set node

makeWidgetFor scope theme node@(Apply func arg cursor delegating) =
  makeFocusDelegator (modify applyDelegating) delegating $
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

makeWidgetFor scope theme node@(Lambda param body cursor delegating) =
  makeFocusDelegator (modify lambdaDelegating) delegating $
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
    paramWidget = fmap (modify lambdaParam) $ makeStringEditWidget theme param
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
    mkWidget = widget font defaultBasePtSize
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
                  TextView.makeWidget (TextView.Style font (basePtSize * 2)) ["The not-yet glorious structural code editor"]
    modelWidget = makeWidget ["launchMissiles"] (mkTheme font basePtSize) model
