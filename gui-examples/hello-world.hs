{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import           Control.Lens.Operators ((&), (^.))
import           Data.MRUMemo (memoIO)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.EventMap (strongerEvents)
import qualified Graphics.UI.Bottle.Main as Main
import           Graphics.UI.Bottle.Widget (Widget, Size, EventResult, setFocused)
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.Bottle.Zoom as Zoom
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
    (Float -> IO Draw.Font) -> Main.Env -> IO (Widget (m EventResult))
hello getFont env =
    do
        sizeFactor <- Zoom.getSizeFactor (env ^. Main.eZoom)
        font <- getFont (sizeFactor * 20)
        TextView.makeWidget (TextView.whiteText font) "Hello World!" ["hello"]
            & setFocused
            & strongerEvents Main.quitEventMap
            & return
