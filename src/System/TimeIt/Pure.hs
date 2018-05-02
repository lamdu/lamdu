{-# LANGUAGE RankNTypes #-}
-- | Measure pure computation times

module System.TimeIt.Pure
    ( Evaluator(..), timedEvaluator
    ) where

import qualified Control.Exception as E
import           System.IO.Unsafe
import           System.TimeIt

import           Lamdu.Prelude

newtype Evaluator = Evaluator { evaluate :: forall a. HasCallStack => a -> a }

timedEvaluator :: (Double -> IO ()) -> IO Evaluator
timedEvaluator report =
    pure Evaluator
    { evaluate = \x ->
        do
            (time, res) <- E.evaluate x & timeItT
            report time
            pure res
        & unsafePerformIO
    }
