{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Main as Main
import           Graphics.UI.Bottle.Widget (Widget, Size)
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

import           Prelude.Compat

main :: IO ()
main =
    do
        win <- GLFWUtils.createWindow "GUI Demo" Nothing (Vector2 800 100)
        Main.mainLoopWidget win hello Main.defaultOptions
    & GLFWUtils.withGLFW

hello :: Size -> IO (Widget a)
hello size =
    do
        font <- Draw.openFont (min 200 (realToFrac (size ^. _2))) "fonts/DejaVuSans.ttf"
        let textStyle =
                TextView.Style
                { TextView._styleColor = Draw.Color 1 1 1 1
                , TextView._styleFont = font
                , TextView._styleUnderline = Nothing
                }
        TextView.makeWidget textStyle "Hello World!" ["hello"] & return
