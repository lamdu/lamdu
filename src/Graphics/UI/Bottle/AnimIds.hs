{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.UI.Bottle.AnimIds(backgroundCursorId, textCursorId) where

import Graphics.UI.Bottle.Animation(AnimId)

backgroundCursorId :: AnimId
backgroundCursorId = ["background cursor"]

textCursorId :: AnimId
textCursorId = ["text cursor"]
