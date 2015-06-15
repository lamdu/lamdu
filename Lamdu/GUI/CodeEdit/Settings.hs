{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.CodeEdit.Settings
    ( Settings(..), sInfoMode, InfoMode(..), defaultInfoMode
    , nextInfoMode
    , mkEventMap
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.IORef
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config

data InfoMode = Evaluation | Types | None
    deriving (Eq, Ord, Show, Enum, Bounded)

defaultInfoMode :: InfoMode
defaultInfoMode = Evaluation

newtype Settings = Settings
    { _sInfoMode :: InfoMode
    }
Lens.makeLenses ''Settings

cyclicSucc :: (Eq a, Enum a, Bounded a) => a -> a
cyclicSucc x
    | x == maxBound = minBound
    | otherwise = succ x

nextInfoMode :: InfoMode -> InfoMode
nextInfoMode = cyclicSucc

mkEventMap :: Config -> IORef Settings -> IO (Widget.EventHandlers IO)
mkEventMap config settingsRef =
    do
        settings <- readIORef settingsRef
        let curInfoMode = settings ^. sInfoMode
            next = nextInfoMode curInfoMode
            nextDoc = EventMap.Doc ["View", "Subtext", "Show " ++ show next]
        return .
            Widget.keysEventMap (Config.nextInfoModeKeys config) nextDoc .
            modifyIORef settingsRef $ sInfoMode .~ next
