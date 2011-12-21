{-# OPTIONS -fno-warn-missing-signatures #-}

module Editor.Config(
    maxDepth,
    quitKeys, undoKeys, goUpKeys, appendChildKeys, delChildKeys,
    setFocalPointKeys, cutKeys, pasteKeys,
    makeBranchKeys, delBranchKeys,
    actionKeys, collapseKeys, expandKeys, moveToParentKeys)
where

import qualified Graphics.UI.Bottle.EventMap as E

maxDepth :: Int
maxDepth = 10

group mods k = E.KeyEventType mods k
ascii k = group E.noMods (E.charKey k)
ctrl k = group E.ctrl (E.charKey k)

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
actionKeys        = [group E.noMods E.KeyEnter]
collapseKeys      = [ascii '[']
expandKeys        = [ascii ']']
moveToParentKeys  = [group E.alt E.KeyLeft]
