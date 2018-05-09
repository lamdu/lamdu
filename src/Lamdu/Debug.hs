{-# LANGUAGE FlexibleInstances #-}
module Lamdu.Debug
    ( module Lamdu.Debug.Tasks
    , Monitors, Counters
    , HasMonitors(..)
    , Evaluator(..)
    , makeCounters
    , makeMonitors
    , noopMonitors
    ) where

import qualified Data.Text as Text
import           Lamdu.Debug.Tasks
import qualified System.Metrics as Metrics
import           System.Metrics.Counter (Counter)
import qualified System.Metrics.Counter as Counter
import qualified System.Remote.Monitoring.Shim as Ekg
import           System.TimeIt.Pure (Evaluator(..))
import qualified System.TimeIt.Pure as TimeIt

import           Lamdu.Prelude

type Monitors = Tasks Evaluator
type Counters = Tasks Counter

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
    traverse (`Metrics.createCounter` Ekg.serverMetricStore ekg) taskNames

counterEvaluator :: Counter -> IO Evaluator
counterEvaluator ctr = TimeIt.timedEvaluator (Counter.add ctr . round . (*1e6))

makeBreakpoint :: Text -> Bool -> Evaluator
makeBreakpoint _ False = idE
makeBreakpoint msg True =
    Evaluator (\_ -> error ("Breakpoint encountered: " ++ Text.unpack msg))

compose :: Evaluator -> Evaluator -> Evaluator
compose (Evaluator f) (Evaluator g) = Evaluator (f . g)

noopMonitors :: Monitors
noopMonitors = pure idE

makeMonitors :: Tasks Bool -> Maybe Counters -> IO Monitors
makeMonitors breakpoints mCounters =
    do
        countersEval <-
            case mCounters of
            Nothing -> pure (pure idE)
            Just counters -> traverse counterEvaluator counters
        compose
            <$> (makeBreakpoint <$> taskNames <*> breakpoints)
            <*> countersEval
            & pure
