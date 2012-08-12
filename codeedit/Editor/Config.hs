{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

module Editor.Config where

import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

mk = E.ModKey

noMods = mk E.noMods
ctrl = mk E.ctrl . E.charKey
alt = mk E.alt . E.charKey
shift = mk E.shift . E.charKey
ctrlAlt = mk (E.noMods {E.modCtrl = True, E.modAlt = True}) . E.charKey
-- altShift = mk E.noMods { E.modAlt = True, E.modShift = True } . E.charKey
k = noMods . E.charKey

quitKeys          = [ctrl 'q']
undoKeys          = [ctrl 'z']
redoKeys          = [ctrl 'y']
makeBranchKeys    = [ctrl 's']

jumpToBranchesKeys = [mk E.ctrl E.KeyF10]

overlayDocKeys    = [noMods E.KeyF1, alt 'h']

addNextParamKeys  = [E.ModKey E.noMods E.KeySpace]

delBranchKeys     = [alt 'o']

closePaneKeys     = [alt 'w']
movePaneDownKeys  = [mk E.alt E.KeyDown]
movePaneUpKeys    = [mk E.alt E.KeyUp]

replaceKeys       = alt 'r' : delKeys

pickResultKeys    = [noMods E.KeyEnter]
jumpToDefinitionKeys  = [noMods E.KeyEnter]
delKeys           = [noMods E.KeyBackspace, noMods E.KeyDel, mk E.alt E.KeyDel]
giveAsArgumentKeys = [k ']', shift '0']
callWithArgumentKeys = [k '[', shift '9']
addNextArgumentKeys = [E.ModKey E.noMods E.KeySpace]
debugModeKeys = [ctrlAlt 'd']

newDefinitionKeys = [alt 'n']

definitionColor = Draw.Color 0.8 0.5 1 1
parameterColor = Draw.Color 0.2 0.8 0.9 1

literalIntColor = Draw.Color 0 1 0 1

previousCursorKeys = [mk E.alt E.KeyLeft]

holeResultCount = 8
holeResultScaleFactor = 0.7

focusedHoleBackgroundColor = Draw.Color 1 0 0 0.361
unfocusedHoleBackgroundColor = Draw.Color 1 0 0 1

unfocusedReadOnlyHoleBackgroundColor = Draw.Color 0.7 0.3 0.3 1

inferredHoleColor = Draw.Color 0.2 1 0.2 1

parenHighlightColor = Draw.Color 0.3 0 1 0.25

lambdaWrapKeys = [k '\\']
addWhereItemKeys = [k 'w']

lambdaColor = Draw.Color 1 0.2 0.2 1
lambdaTextSize = 30

rightArrowColor = Draw.Color 1 0.2 0.2 1
rightArrowTextSize = 30

whereColor = Draw.Color 0.8 0.6 0.1 1
whereTextSize = 16
whereScaleFactor = 0.85

foldKeys = [k '-']
unfoldKeys = foldKeys

typeScaleFactor = 0.6
squareParensScaleFactor = 0.96

foreignModuleColor = Draw.Color 1 0.3 0.35 1
foreignVarColor = Draw.Color 1 0.65 0.25 1

cutKeys = [ctrl 'x', k 'x']
pasteKeys = [ctrl 'v', k 'v']

inactiveTintColor = Draw.Color 1 1 1 0.8
activeDefBGColor = Draw.Color 0 0 0.2 1

inferredTypeTint = Draw.Color 0.7 0.7 0.7 1
inferredTypeErrorBGColor = Draw.Color 0.5 0.05 0.05 1

helpStyle font = TextView.Style {
  TextView.styleColor = Draw.Color 1 1 1 1,
  TextView.styleFont = font,
  TextView.styleFontSize = 10
  }

baseStyle font = TextEdit.Style
  { TextEdit.sTextViewStyle =
    TextView.Style
      { TextView.styleColor = Draw.Color 1 1 1 1
      , TextView.styleFont = font
      , TextView.styleFontSize = 25
      }
  , TextEdit.sCursorColor = TextEdit.defaultCursorColor
  , TextEdit.sCursorWidth = TextEdit.defaultCursorWidth
  , TextEdit.sTextCursorId = WidgetIds.textCursorId
  , TextEdit.sBackgroundCursorId = WidgetIds.backgroundCursorId
  , TextEdit.sEmptyUnfocusedString = ""
  , TextEdit.sEmptyFocusedString = ""
  }

selectedBranchColor = Draw.Color 0 0.5 0 1

jumpLHStoRHSKeys = [k '`']
jumpRHStoLHSKeys = [k '`']

shrinkBaseFontKeys = [ctrl '-']
enlargeBaseFontKeys = [ctrl '=']

enlargeFactor = 1.1
shrinkFactor = 1.1

defTypeLabelTextSize = 16
defTypeLabelColor = Draw.Color 0.6 0.7 1 1

defTypeBoxSizeFactor = 0.6

acceptInferredTypeKeys = [noMods E.KeySpace, noMods E.KeyEnter]
