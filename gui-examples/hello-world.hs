{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Control.Lens.Operators ((&), (^.))
import           Data.MRUMemo (memoIO)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView

import           Prelude.Compat

fontPath :: FilePath
fontPath = "fonts/DejaVuSans.ttf"

main :: IO ()
main =
    do
        win <- M.createWindow "Hello World" Nothing (M.Vector2 800 400)
        cachedOpenFont <- memoIO (`M.openFont` fontPath)
        M.defaultOptions fontPath
            >>= M.mainLoopWidget win (hello cachedOpenFont)
    & M.withGLFW

hello ::
    Functor m =>
    (Float -> IO M.Font) -> M.MainLoopEnv -> IO (M.Widget (m M.EventResult))
hello getFont env =
    do
        sizeFactor <- M.getZoomFactor (env ^. M.eZoom)
        font <- getFont (sizeFactor * 20)
        TextView.make (TextView.whiteText font) "Hello World!" ["hello"]
            ^. M.tValue
            & Widget.fromView
            & Widget.setFocused
            & M.weakerEvents M.quitEventMap
            & pure
