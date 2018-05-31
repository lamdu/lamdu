{-# LANGUAGE FlexibleInstances, RankNTypes, TemplateHaskell #-}
module Lamdu.Debug
    ( module Lamdu.Debug.Tasks
    , Monitors, Counters
    , HasMonitors(..)
    , Monitor(..), mPure, mAction
    , Evaluator(..), EvaluatorM(..)
    , makeCounters
    , makeMonitors
    , noopMonitors
    ) where

import qualified Control.Lens as Lens
import qualified Data.Text as Text
import           Lamdu.Debug.Tasks
import           System.CPUTime (getCPUTime)
import           System.IO.Unsafe (unsafePerformIO)
import qualified System.Metrics as Metrics
import           System.Metrics.Counter (Counter)
import qualified System.Metrics.Counter as Counter
import qualified System.Remote.Monitoring.Shim as Ekg
import           System.TimeIt.Pure (Evaluator(..))
import qualified System.TimeIt.Pure as TimeIt

import           Lamdu.Prelude

newtype EvaluatorM = EvaluatorM { evaluateM :: forall m a. (Monad m, HasCallStack) => m a -> m a }

data Monitor = Monitor
    { _mPure :: Evaluator
    , _mAction :: EvaluatorM
    }
Lens.makeLenses ''Monitor

type Monitors = Tasks Monitor
type Counters = Tasks Counter

class HasMonitors env where
    monitors :: Lens' env Monitors

instance HasMonitors (Tasks Monitor) where monitors = id

idE :: Evaluator
idE = Evaluator id

taskNames :: Tasks Text
taskNames =
    Tasks
    { _inference = "Inference"
    }

unsafeGetCPUTime :: Monad m => m Integer
unsafeGetCPUTime = pure getCPUTime >>= pure . unsafePerformIO

unsafeTimeIt :: Monad m => m a -> m (Double, a)
unsafeTimeIt action =
    do
        before <- unsafeGetCPUTime
        result <- action
        after <- unsafeGetCPUTime
        pure (1e-12 * fromIntegral (after - before), result)

actionEvaluator :: (Double -> IO ()) -> IO EvaluatorM
actionEvaluator report =
    pure EvaluatorM
    { evaluateM =
        \x ->
        do
            (time, res) <- unsafeTimeIt x
            unsafePerformIO (report time) `seq` pure res
    }

makeCounters :: Ekg.Server -> IO (Tasks Counter)
makeCounters ekg =
    traverse (`Metrics.createCounter` Ekg.serverMetricStore ekg) taskNames

counterMonitor :: Counter -> IO Monitor
counterMonitor ctr =
    Monitor
    <$> TimeIt.timedEvaluator report
    <*> actionEvaluator report
    where
        report = Counter.add ctr . round . (*1e6)

makeBreakpoint :: Text -> Bool -> Evaluator
makeBreakpoint _ False = idE
makeBreakpoint msg True =
    Evaluator (\_ -> error ("Breakpoint encountered: " ++ Text.unpack msg))

composeEvaluator :: Evaluator -> Evaluator -> Evaluator
composeEvaluator (Evaluator f) (Evaluator g) = Evaluator (f . g)

composeEvaluatorM :: Evaluator -> EvaluatorM -> EvaluatorM
composeEvaluatorM (Evaluator f) (EvaluatorM g) = EvaluatorM (fmap f . g)

composeMonitor :: Evaluator -> Monitor -> Monitor
composeMonitor e (Monitor p a) =
    Monitor
    { _mPure = composeEvaluator e p
    , _mAction = composeEvaluatorM e a
    }

noopMonitors :: Monitors
noopMonitors =
    pure Monitor
    { _mPure = idE
    , _mAction = EvaluatorM id
    }

makeMonitors :: Tasks Bool -> Maybe Counters -> IO Monitors
makeMonitors breakpoints mCounters =
    do
        countersEval <-
            case mCounters of
            Nothing -> pure noopMonitors
            Just counters -> traverse counterMonitor counters
        composeMonitor
            <$> (makeBreakpoint <$> taskNames <*> breakpoints)
            <*> countersEval
            & pure
