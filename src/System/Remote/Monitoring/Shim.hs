-- | Shim for Ekg's System.Remote.Monitoring that supports
-- compile-flag to seemlessly disable ekg support
{-# LANGUAGE CPP, EmptyDataDecls #-}
module System.Remote.Monitoring.Shim
    ( Server, serverMetricStore
    , start
    ) where

import           Data.Word (Word16)

import           Lamdu.Prelude

#ifdef WITH_EKG

import           System.Remote.Monitoring (Server, serverMetricStore)
import qualified System.Remote.Monitoring as Ekg

start :: Word16 -> IO Server
start port = Ekg.forkServer "localhost" (fromIntegral port)

#else
-- EKG stub:

import           System.Metrics (Store)

data Server

start :: Word16 -> IO Server
start _ = fail "Lamdu is compiled without ekg support. Rebuild it with the ekg cabal flag"

serverMetricStore :: Server -> Store
serverMetricStore = error "absurd"

#endif
