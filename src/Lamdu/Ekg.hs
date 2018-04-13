-- | Ekg support for Lamdu
{-# LANGUAGE CPP #-}
module Lamdu.Ekg (start) where

import           Data.Word (Word16)

import           Lamdu.Prelude

#ifdef WITH_EKG
import qualified System.Remote.Monitoring as Ekg

start :: Word16 -> IO ()
start port = Ekg.forkServer "localhost" (fromIntegral port) & void
#else
start :: Word16 -> IO ()
start = fail "Lamdu is compiled without ekg support. Rebuild it with the ekg cabal flag"
#endif
