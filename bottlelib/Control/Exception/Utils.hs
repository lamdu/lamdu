module Control.Exception.Utils
    ( loopWhileException
    ) where

import           Control.Exception (Exception, try)
import           Data.Proxy (Proxy, asProxyTypeOf)

-- | Run an action and retry it when it throws a specific exception type.
-- Finishes when either the action completes successfully
-- or when throws a different exception type
loopWhileException :: Exception e => Proxy e -> IO a -> IO a
loopWhileException proxy act =
    loop
    where
        loop = try act >>= resume
        resume (Left e) = const loop $ e `asProxyTypeOf` proxy
        resume (Right res) = return res
