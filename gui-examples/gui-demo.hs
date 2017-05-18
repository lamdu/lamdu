{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import           Control.Lens.Operators
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Main as Main
import qualified Graphics.UI.Bottle.Main.Animation as MainAnim
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

import           Prelude.Compat

main :: IO ()
main =
    do
        win <- GLFWUtils.createWindow "GUI Demo" Nothing (Vector2 800 150)
        font <- Draw.openFont 120 "fonts/DejaVuSans.ttf"
        let textStyle =
                TextView.Style
                { TextView._styleColor = Draw.Color 1 1 1 1
                , TextView._styleFont = font
                , TextView._styleUnderline = Nothing
                }
        let hello = TextView.makeWidget textStyle "Hello World!" ["hello"]
        Main.mainLoopWidget win (return False) (const (return hello)) (return mainLoopConfig)
    & GLFWUtils.withGLFW
    where
        mainLoopConfig =
            Main.Config
            { Main.cAnim =
                MainAnim.AnimConfig
                { MainAnim.acTimePeriod = 0.11
                , MainAnim.acRemainingRatioInPeriod = 0.2
                }
            , Main.cCursor =
                Widget.CursorConfig
                { Widget.cursorColor = Draw.Color 0.5 0.5 1 0.5
                }
            }
