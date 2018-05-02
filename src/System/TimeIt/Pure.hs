{-# LANGUAGE RankNTypes #-}
-- | Measure pure computation times

module System.TimeIt.Pure where

import Control.Exception (evaluate)
import System.TimeIt
import System.IO.Unsafe

import Lamdu.Prelude

newtype TimedEvaluator = TimedEvaluator { reportEvalTime :: forall a. a -> a }

timedEvaluator :: (Double -> IO ()) -> IO TimedEvaluator
timedEvaluator report =
    pure TimedEvaluator
    { reportEvalTime = \x ->
        do
            (time, res) <- evaluate x & timeItT
            report time
            pure res
        & unsafePerformIO
    }
