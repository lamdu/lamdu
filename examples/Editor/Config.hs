{-# OPTIONS -fno-warn-missing-signatures #-}

module Editor.Config(
    maxDepth,
    overlayDocKeys,
    quitKeys, undoKeys, goUpKeys, appendChildKeys, delChildKeys,
    setFocalPointKeys, cutKeys, pasteKeys,
    makeBranchKeys, delBranchKeys,
    actionKeys, collapseKeys, expandKeys, moveToParentKeys)
where

import qualified Graphics.UI.Bottle.EventMap as E

maxDepth :: Int
maxDepth = 10

group = E.KeyEventType
ascii = group E.noMods . E.charKey
ctrl = group E.ctrl . E.charKey
alt = group E.alt . E.charKey
simple = group E.noMods

quitKeys          = [ctrl 'q']
undoKeys          = [ctrl 'z']
goUpKeys          = [ctrl 'r']
appendChildKeys   = [ctrl 'n']
delChildKeys      = [ctrl 'o']
setFocalPointKeys = [ctrl 'g']
pasteKeys         = [ctrl 'v']
cutKeys           = [ctrl 'x']
makeBranchKeys    = [ctrl 's']
delBranchKeys     = [ctrl 'o']
actionKeys        = [simple E.KeyEnter]
collapseKeys      = [ascii '[']
expandKeys        = [ascii ']']
moveToParentKeys  = [group E.alt E.KeyLeft]

overlayDocKeys = [simple E.KeyF1, alt 'h']