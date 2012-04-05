{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

module Editor.Config where

import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

mk = E.KeyEventType

noMods = mk E.noMods
ctrl = mk E.ctrl . E.charKey
alt = mk E.alt . E.charKey
shift = mk E.shift . E.charKey
ctrlAlt = mk (E.noMods {E.modCtrl = True, E.modAlt = True}) . E.charKey
-- altShift = mk E.noMods { E.modAlt = True, E.modShift = True } . E.charKey
k = noMods . E.charKey

-- pasteKeys         = [ctrl 'v']
-- cutKeys           = [ctrl 'x']

-- actionKeys        = [noMods E.KeyEnter]

quitKeys          = [ctrl 'q']
undoKeys          = [ctrl 'z']
redoKeys          = [ctrl 'y']
makeBranchKeys    = [ctrl 's']

-- moveToParentKeys  = [mk E.alt E.KeyLeft]

overlayDocKeys    = [noMods E.KeyF1, alt 'h']

addNextParamKeys  = [E.KeyEventType E.noMods E.KeySpace]

delBranchKeys     = [alt 'o']

closePaneKeys     = [alt 'w']

replaceKeys       = [alt 'r']

pickResultKeys    = [noMods E.KeyEnter]
jumpToDefinitionKeys  = [noMods E.KeyEnter]
delKeys           = [noMods E.KeyBackspace, noMods E.KeyDel, mk E.alt E.KeyDel]
giveAsArgumentKeys = [k ']', shift '0']
callWithArgumentKeys = [k '[', shift '9']
addNextArgumentKeys = [E.KeyEventType E.noMods E.KeySpace]
debugModeKeys = [ctrlAlt 'd']

exprFocusDelegatorKeys = FocusDelegator.Keys {
  FocusDelegator.startDelegatingKey = mk E.shift E.KeyRight,
  FocusDelegator.stopDelegatingKey = mk E.shift E.KeyLeft
  }

newDefinitionKeys = [alt 'n']

builtinColor = Draw.Color 1 0.6 0.2 1
definitionColor = Draw.Color 0.8 0.5 1 1
parameterColor = Draw.Color 0.2 0.8 0.9 1

literalIntColor = Draw.Color 0 1 0 1

previousCursorKeys = [mk E.alt E.KeyLeft]

focusedHoleBackgroundColor = Draw.Color 1 0 0 0.361
unfocusedHoleBackgroundColor = Draw.Color 1 0 0 1

parenHighlightColor = Draw.Color 0.3 0 1 0.25

unnamedStr = "<noname>"

jumpToLhsKeys = [k '`']
jumpToRhsKeys = [k '=', noMods E.KeyPadEqual]

lambdaWrapKeys = [k '\\']

lambdaColor = Draw.Color 1 0.2 0.2 1
lambdaTextSize = 30

rightArrowColor = Draw.Color 1 0.2 0.2 1
rightArrowTextSize = 30

whereColor = Draw.Color 0.8 0.6 0.1 1
whereTextSize = 16
whereScaleFactor = 0.85

foldKeys = [k '-']
unfoldKeys = foldKeys
