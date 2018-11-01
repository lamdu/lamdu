{-# LANGUAGE TemplateHaskell #-}
module GUI.Momentu.Zoom
    ( Zoom, make, eventMap, getZoomFactor
    , Config(..), defaultConfig
    , makeUnscaled
    ) where

import qualified Control.Lens as Lens
import           Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.List.Lens (prefixed)
import           Data.IORef
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.MetaKey (MetaKey)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.State as State
import qualified GUI.Momentu.Widget as Widget
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLFW.Utils as GLFW.Utils

import           Lamdu.Prelude

data Config = Config
    { _shrinkKeys :: [MetaKey]
    , _enlargeKeys :: [MetaKey]
    , _enlargeFactor :: Double
    , _shrinkFactor :: Double
    } deriving (Eq, Show)
deriveJSON Aeson.defaultOptions
    {Aeson.fieldLabelModifier = (^?! prefixed "_")}
    ''Config

Lens.makeLenses ''Config

defaultConfig :: Config
defaultConfig =
    Config
    { _shrinkKeys = [MetaKey.cmd MetaKey.Key'Minus]
    , _enlargeKeys = [MetaKey.cmd MetaKey.Key'Equal]
    , _shrinkFactor = 1.1
    , _enlargeFactor = 1.1
    }

newtype Zoom = Zoom
    { _scaleFactorRef :: IORef Widget.R
    }

eventMap :: Zoom -> Config -> EventMap (IO State.Update)
eventMap (Zoom ref) config =
    mconcat
    [ E.keysEventMap (config ^. enlargeKeys)
        (E.Doc ["View", "Zoom", "Enlarge"]) $
        modifyIORef ref (* config ^. enlargeFactor)
    , E.keysEventMap (config ^. shrinkKeys)
        (E.Doc ["View", "Zoom", "Shrink"]) $
        modifyIORef ref (/ config ^. shrinkFactor)
    ]

getZoomFactor :: Fractional a => Zoom -> IO a
getZoomFactor (Zoom ref) = readIORef ref <&> realToFrac

make :: GLFW.Window -> IO Zoom
make win =
    do
        displayScale <- GLFW.Utils.getDisplayScale win
        winSize <- GLFW.Utils.windowSize win
        let winSizeFactor = (winSize ^. _2) / 1080 & max 1
        newIORef (displayScale ^. _2 * winSizeFactor) <&> Zoom

-- | Useful mainly for tests
makeUnscaled :: Widget.R -> IO Zoom
makeUnscaled = fmap Zoom . newIORef
