{-# LANGUAGE NoImplicitPrelude, DeriveGeneric, RecordWildCards, OverloadedStrings #-}
module Graphics.UI.Bottle.Zoom
    ( Zoom, make, eventMap, getSizeFactor
    , Config(..), defaultConfig
    ) where

import qualified Data.Aeson.Types as Aeson
import           Data.IORef
import           GHC.Generics (Generic)
import qualified Graphics.UI.Bottle.EventMap as EventMap
import           Graphics.UI.Bottle.MetaKey (MetaKey)
import qualified Graphics.UI.Bottle.MetaKey as MetaKey
import qualified Graphics.UI.Bottle.Widget as Widget
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

getSizeFactor :: Zoom -> IO Widget.R
getSizeFactor (Zoom ref) = readIORef ref

make :: GLFW.Window -> IO Zoom
make win =
    do
        displayScale <- GLFWUtils.getDisplayScale win
        newIORef (displayScale ^. _2) <&> Zoom
