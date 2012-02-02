{-# OPTIONS -fno-warn-missing-signatures #-}

module Editor.Config(
    overlayDocKeys,
    quitKeys, undoKeys,

    -- cutKeys, pasteKeys,

    makeBranchKeys, delBranchKeys,

    -- actionKeys,

    delKeys, callWithArgumentKeys, giveAsArgumentKeys, addNextArgumentKeys,
    pickResultKeys, jumpToDefinitionKeys,

    -- moveToParentKeys,

    addParamKeys, newDefinitionKeys,

    closePaneKeys,

    exprFocusDelegatorKeys,

    builtinColor, definitionColor, parameterColor)
where

import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

group = E.KeyEventType
ctrl = group E.ctrl . E.charKey
alt = group E.alt . E.charKey
-- altShift = group E.noMods { E.modAlt = True, E.modShift = True } . E.charKey
simple = group E.noMods

-- pasteKeys         = [ctrl 'v']
-- cutKeys           = [ctrl 'x']

-- actionKeys        = [simple E.KeyEnter]

quitKeys          = [ctrl 'q']
undoKeys          = [ctrl 'z']
makeBranchKeys    = [ctrl 's']

-- moveToParentKeys  = [group E.alt E.KeyLeft]

overlayDocKeys    = [simple E.KeyF1, alt 'h']

addParamKeys      = [alt 'p']

delBranchKeys     = [alt 'o']

closePaneKeys       = [ctrl 'w']

pickResultKeys    = [simple E.KeyEnter]
jumpToDefinitionKeys  = [simple E.KeyEnter]
delKeys           = [simple E.KeyBackspace, simple E.KeyDel]
giveAsArgumentKeys = [simple (E.charKey 'a')]
callWithArgumentKeys = [simple (E.charKey 'c')]
addNextArgumentKeys = [E.SpaceKeyEventType E.noMods]

exprFocusDelegatorKeys = FocusDelegator.Keys {
  FocusDelegator.startDelegatingKey = group E.shift E.KeyRight,
  FocusDelegator.stopDelegatingKey = group E.shift E.KeyLeft
  }

newDefinitionKeys = [alt 'n']

builtinColor = Draw.Color 1 0.6 0.2 1
definitionColor = Draw.Color 1 1 1 1
parameterColor = Draw.Color 0.2 0.8 0.9 1
