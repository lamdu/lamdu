{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import           Control.Lens.Operators ((&), (^.))
import           Data.MRUMemo (memoIO)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           GUI.Momentu.EventMap (strongerEvents)
import qualified GUI.Momentu.Main as Main
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified GUI.Momentu.Zoom as Zoom
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

import           Prelude.Compat

fontPath :: FilePath
fontPath = "fonts/DejaVuSans.ttf"

main :: IO ()
main =
    do
        win <- GLFWUtils.createWindow "Hello World" Nothing (Vector2 800 400)
        cachedOpenFont <- memoIO (`Draw.openFont` fontPath)
        Main.defaultOptions fontPath
            >>= Main.mainLoopWidget win (hello cachedOpenFont)
    & GLFWUtils.withGLFW

hello ::
    Functor m =>
    (Float -> IO Draw.Font) -> Main.Env -> IO (Widget (m Widget.EventResult))
hello getFont env =
    do
        sizeFactor <- Zoom.getSizeFactor (env ^. Main.eZoom)
        font <- getFont (sizeFactor * 20)
        TextView.make (TextView.whiteText font) "Hello World!" ["hello"]
            & Widget.fromView
            & Widget.setFocused
            & strongerEvents Main.quitEventMap
            & return
