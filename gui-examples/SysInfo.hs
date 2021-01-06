{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

import           Lamdu.Prelude

main :: IO ()
main =
    do
        putStrLn ""
        monitor <- GLFWUtils.getPrimaryMonitor
        GLFW.getMonitorPhysicalSize monitor
            <&> (\(x,y) -> "Monitor physical size: " ++ show x ++ "mm x " ++ show y ++ "mm")
            >>= putStrLn
        putStrLn ""
        GLFW.getVideoMode monitor
            >>= maybe (fail "Can't get video mode of monitor") pure
            <&> show
            <&> ("Video mode: " ++)
            >>= putStrLn
        putStrLn ""
    & GLFWUtils.withGLFW
