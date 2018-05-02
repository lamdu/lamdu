{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Lamdu.Debug
    ( Tasks(..), inference
    , Monitors
    , HasMonitors(..)
    , Ekg.Evaluator(..)
    , makeMonitors
    ) where

import qualified Control.Lens as Lens
import           System.Remote.Monitoring.Shim (Ekg)
import qualified System.Remote.Monitoring.Shim as Ekg

import           Lamdu.Prelude

newtype Tasks a = Tasks
    { _inference :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Tasks

type Monitors = Tasks Ekg.Evaluator

class HasMonitors env where
    monitors :: Lens' env Monitors

instance HasMonitors (Tasks Ekg.Evaluator) where monitors = id

idE :: Ekg.Evaluator
idE = Ekg.Evaluator id

taskNames :: Tasks Text
taskNames =
    Tasks
    { _inference = "Inference"
    }

makeMonitors :: Maybe Ekg -> IO Monitors
makeMonitors Nothing = Tasks idE & pure
makeMonitors (Just ekg) =
    traverse (`Ekg.timedEvaluator` ekg) taskNames
