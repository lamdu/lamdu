{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}
module GUI.Momentu.Zoom
    ( Zoom, make, eventMap, getZoomFactor
    , Config(..), defaultConfig
    ) where

import           Data.Aeson.TH (deriveJSON)
import           Data.Aeson.Types (defaultOptions)
import           Data.IORef
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.MetaKey (MetaKey)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as State
import qualified GUI.Momentu.Widget as Widget
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

import           Lamdu.Prelude

data Config = Config
    { shrinkKeys :: [MetaKey]
    , enlargeKeys :: [MetaKey]
    , enlargeFactor :: Double
    , shrinkFactor :: Double
    } deriving (Eq, Show)
deriveJSON defaultOptions ''Config

defaultConfig :: Config
defaultConfig =
    Config
    { shrinkKeys = [MetaKey.cmd MetaKey.Key'Minus]
    , enlargeKeys = [MetaKey.cmd MetaKey.Key'Equal]
    , shrinkFactor = 1.1
    , enlargeFactor = 1.1
    }

newtype Zoom = Zoom
    { _scaleFactorRef :: IORef Widget.R
    }

eventMap :: Zoom -> Config -> EventMap (IO State.Update)
eventMap (Zoom ref) config =
    mconcat
    [ E.keysEventMap (enlargeKeys config)
        (E.Doc ["View", "Zoom", "Enlarge"]) $
        modifyIORef ref (* realToFrac (enlargeFactor config))
    , E.keysEventMap (shrinkKeys config)
        (E.Doc ["View", "Zoom", "Shrink"]) $
        modifyIORef ref (/ realToFrac (shrinkFactor config))
    ]

getZoomFactor :: Fractional a => Zoom -> IO a
getZoomFactor (Zoom ref) = readIORef ref <&> realToFrac

make :: GLFW.Window -> IO Zoom
make win =
    do
        displayScale <- GLFWUtils.getDisplayScale win
        newIORef (displayScale ^. _2) <&> Zoom
