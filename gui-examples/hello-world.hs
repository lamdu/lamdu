{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Main as Main
import           Graphics.UI.Bottle.MetaKey (MetaKey(..), noMods)
import           Graphics.UI.Bottle.Widget (Widget, Size, EventResult, keysEventMap, strongerEvents, respondToCursor)
import qualified Graphics.UI.Bottle.Widgets.EventMapDoc as EventMapDoc
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

import           Prelude.Compat

main :: IO ()
main =
    do
        win <- GLFWUtils.createWindow "Hello World" Nothing (Vector2 800 100)
        addHelp <- EventMapDoc.makeToggledHelpAdder EventMapDoc.HelpNotShown
        Main.mainLoopWidget win (hello addHelp) Main.defaultOptions
    & GLFWUtils.withGLFW

hello ::
    Functor m =>
    (EventMapDoc.Config -> Size -> Widget (m EventResult) -> IO (Widget (m EventResult))) ->
    Size -> IO (Widget (m EventResult))
hello addHelp size =
    do
        font <- Draw.openFont (min 100 (realToFrac (size ^. _2))) "fonts/DejaVuSans.ttf"
        let textStyle =
                TextView.Style
                { TextView._styleColor = Draw.Color 1 1 1 1
                , TextView._styleFont = font
                , TextView._styleUnderline = Nothing
                }
        TextView.makeWidget textStyle "Hello World!" ["hello"]
            & respondToCursor
            & strongerEvents quitEventMap
            & addHelp (EventMapDoc.defaultConfig font) size

quitEventMap :: Functor f => EventMap (f EventResult)
quitEventMap =
    keysEventMap
    [MetaKey noMods GLFW.Key'Q] (EventMap.Doc ["Quit"])
    (error "Quit")
