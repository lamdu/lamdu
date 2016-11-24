{-# LANGUAGE NoImplicitPrelude, RecordWildCards, OverloadedStrings #-}
module Lamdu.GUI.Zoom
    ( Zoom, make, eventMap, getSizeFactor
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
import           Data.IORef
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config

newtype Zoom = Zoom
    { _scaleFactorRef :: IORef Widget.R
    }

eventMap :: Zoom -> Config.Zoom -> Widget.EventMap (IO Widget.EventResult)
eventMap (Zoom ref) Config.Zoom{..} =
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

make :: IO Zoom
make = newIORef 1 <&> Zoom
