-- | Ekg support for Lamdu
{-# LANGUAGE CPP, EmptyCase #-}
module Lamdu.Ekg (Ekg, start, registerGauge) where

import           Data.Int (Int64)
import           Data.Word (Word16)

import           Lamdu.Prelude

#ifdef WITH_EKG
import qualified System.Remote.Monitoring as Ekg
import qualified System.Metrics as Metrics

newtype Ekg = Ekg { server :: Ekg.Server }

registerGauge :: Text -> IO Int64 -> Ekg -> IO ()
registerGauge label getVal =
    Metrics.registerGauge label getVal . Ekg.serverMetricStore . server

start :: Word16 -> IO Ekg
start port = Ekg.forkServer "localhost" (fromIntegral port) <&> Ekg
#else
data Ekg

registerGauge :: Text -> IO Int64 -> Ekg -> IO ()
registerGauge _ _ = \case

start :: Word16 -> IO Ekg
start = fail "Lamdu is compiled without ekg support. Rebuild it with the ekg cabal flag"
#endif
