module Debug.Trace.Warned where

import qualified Debug.Trace as Trace

import           Prelude

{-# WARNING traceId "Leaving traces in the code" #-}
traceId :: Show a => String -> a -> a
traceId prefix x = Trace.trace (prefix ++ show x) x

{-# WARNING traceIdVia "Leaving traces in the code" #-}
traceIdVia :: Show b => (a -> b) -> String -> a -> a
traceIdVia f prefix x = Trace.trace (prefix ++ show (f x)) x

{-# WARNING trace "Leaving traces in the code" #-}
trace :: String -> a -> a
trace = Trace.trace

{-# WARNING traceShowM "Leaving traces in the code" #-}
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = Trace.traceShowM

{-# WARNING traceM "Leaving traces in the code" #-}
traceM :: Applicative f => String -> f ()
traceM = Trace.traceM
