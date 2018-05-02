-- | Shim for Ekg's System.Remote.Monitoring that supports
-- compile-flag to seemlessly disable ekg support
{-# LANGUAGE CPP, EmptyCase #-}
module System.Remote.Monitoring.Shim
    ( Ekg, start, registerGauge
    , TimedEvaluator(..), timedEvaluator
    ) where

import           Data.Int (Int64)
import           Data.Word (Word16)
import           System.TimeIt.Pure (TimedEvaluator(..))

import           Lamdu.Prelude

#ifdef WITH_EKG

import qualified System.Metrics as Metrics
import qualified System.Metrics.Counter as Counter
import qualified System.Remote.Monitoring as Ekg
import qualified System.TimeIt.Pure as TimeIt

newtype Ekg = Ekg { server :: Ekg.Server }

store :: Ekg -> Metrics.Store
store = Ekg.serverMetricStore . server

registerGauge :: Text -> IO Int64 -> Ekg -> IO ()
registerGauge label getVal = Metrics.registerGauge label getVal . store

start :: Word16 -> IO Ekg
start port = Ekg.forkServer "localhost" (fromIntegral port) <&> Ekg

timedEvaluator :: Text -> Ekg -> IO TimedEvaluator
timedEvaluator label ekg =
    do
        ctr <- Metrics.createCounter label (store ekg)
        TimeIt.timedEvaluator (Counter.add ctr . round . (*1e6))

#else
-- EKG stub:

data Ekg

registerGauge :: Text -> IO Int64 -> Ekg -> IO ()
registerGauge _ _ = \case

start :: Word16 -> IO Ekg
start = fail "Lamdu is compiled without ekg support. Rebuild it with the ekg cabal flag"

timedEvaluator :: Text -> Ekg -> IO TimedEvaluator
timedEvaluator _ = \case

#endif
