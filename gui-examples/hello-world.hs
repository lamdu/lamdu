{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import           Data.MRUMemo (memoIO)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.EventMap (strongerEvents)
import qualified Graphics.UI.Bottle.Main as Main
import           Graphics.UI.Bottle.Widget (Widget, Size, EventResult, respondToCursor)
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.Bottle.Zoom as Zoom
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

import           Prelude.Compat

fontPath :: FilePath
fontPath = "fonts/DejaVuSans.ttf"

main :: IO ()
main =
    GLFWUtils.withGLFW $ do
        win <- GLFWUtils.createWindow "Hello World" Nothing (Vector2 800 400)
        cachedOpenFont <-
            memoIO $ \size ->
            Draw.openFont (min 100 size) fontPath
        Main.defaultOptions fontPath
            >>= Main.mainLoopWidget win (hello cachedOpenFont)

hello ::
    Functor m =>
    (Float -> IO Draw.Font) -> Zoom.Zoom -> Size -> IO (Widget (m EventResult))
hello getFont zoom _size =
    do
        sizeFactor <- Zoom.getSizeFactor zoom
        font <- getFont (sizeFactor * 20)
        return $
            strongerEvents Main.quitEventMap $
            respondToCursor $
            TextView.makeWidget (TextView.whiteText font) "Hello World!" ["hello"]
