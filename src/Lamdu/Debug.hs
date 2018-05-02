{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Lamdu.Debug
    ( Tasks(..), inference
    , Monitors
    , HasMonitors(..)
    , Evaluator(..)
    , makeCounters
    , makeMonitors
    ) where

import qualified Control.Lens as Lens
import qualified System.Metrics as Metrics
import           System.Metrics.Counter (Counter)
import qualified System.Metrics.Counter as Counter
import qualified System.Remote.Monitoring.Shim as Ekg
import           System.TimeIt.Pure (Evaluator(..))
import qualified System.TimeIt.Pure as TimeIt

import           Lamdu.Prelude

newtype Tasks a = Tasks
    { _inference :: a
    } deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''Tasks

type Monitors = Tasks Evaluator

class HasMonitors env where
    monitors :: Lens' env Monitors

instance HasMonitors (Tasks Evaluator) where monitors = id

idE :: Evaluator
idE = Evaluator id

taskNames :: Tasks Text
taskNames =
    Tasks
    { _inference = "Inference"
    }

makeCounters :: Ekg.Server -> IO (Tasks Counter)
makeCounters ekg =
    traverse (`Metrics.createCounter` (Ekg.serverMetricStore ekg)) taskNames

makeMonitors :: Maybe (Tasks Counter) -> IO Monitors
makeMonitors Nothing = Tasks idE & pure
makeMonitors (Just tasks) =
    traverse f tasks
    where
        f ctr = TimeIt.timedEvaluator (Counter.add ctr . round . (*1e6))
