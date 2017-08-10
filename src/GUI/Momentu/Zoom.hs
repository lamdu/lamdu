{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, RecordWildCards, OverloadedStrings #-}
module GUI.Momentu.Zoom
    ( Zoom, make, eventMap, getSizeFactor
    , Config(..), defaultConfig
    ) where

import qualified Data.Aeson.Types as Aeson
import           Data.IORef
import           GHC.Generics (Generic)
import qualified GUI.Momentu.EventMap as EventMap
import           GUI.Momentu.MetaKey (MetaKey)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.Widget as Widget
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFWUtils

import           Lamdu.Prelude

data Config = Config
    { shrinkKeys :: [MetaKey]
    , enlargeKeys :: [MetaKey]
    , enlargeFactor :: Double
    , shrinkFactor :: Double
    } deriving (Eq, Generic, Show)
instance Aeson.ToJSON Config where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions
instance Aeson.FromJSON Config

defaultConfig :: Config
defaultConfig =
    Config
    { shrinkKeys = [MetaKey.cmd GLFW.Key'Minus]
    , enlargeKeys = [MetaKey.cmd GLFW.Key'Equal]
    , shrinkFactor = 1.1
    , enlargeFactor = 1.1
    }

newtype Zoom = Zoom
    { _scaleFactorRef :: IORef Widget.R
    }

eventMap :: Zoom -> Config -> Widget.EventMap (IO Widget.EventResult)
eventMap (Zoom ref) Config{..} =
    mconcat
    [ Widget.keysEventMap enlargeKeys
        (EventMap.Doc ["View", "Zoom", "Enlarge"]) $
        modifyIORef ref (* realToFrac enlargeFactor)
    , Widget.keysEventMap shrinkKeys
        (EventMap.Doc ["View", "Zoom", "Shrink"]) $
        modifyIORef ref (/ realToFrac shrinkFactor)
    ]

getSizeFactor :: Fractional a => Zoom -> IO a
getSizeFactor (Zoom ref) = readIORef ref <&> realToFrac

make :: GLFW.Window -> IO Zoom
make win =
    do
        displayScale <- GLFWUtils.getDisplayScale win
        newIORef (displayScale ^. _2) <&> Zoom
