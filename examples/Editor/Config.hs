{-# OPTIONS -fno-warn-missing-signatures #-}

module Editor.Config(
    overlayDocKeys,
    quitKeys, undoKeys,

    -- cutKeys, pasteKeys,

    makeBranchKeys, delBranchKeys,

    -- actionKeys,

    -- moveToParentKeys,

    addParamKeys, delParamKeys,

    exprFocusDelegatorKeys)
where

import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

group = E.KeyEventType
ctrl = group E.ctrl . E.charKey
alt = group E.alt . E.charKey
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
delParamKeys      = [alt 'o']

delBranchKeys     = [alt 'o']

exprFocusDelegatorKeys = FocusDelegator.Keys {
  FocusDelegator.startDelegatingKey = group E.shift E.KeyRight,
  FocusDelegator.stopDelegatingKey = group E.shift E.KeyLeft
  }