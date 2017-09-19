{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Prelude
    ( module X
    , traceId, trace, traceShowM
    , todo
    ) where

-- .@~ is missing in Control.Lens.Operators in lens-4.15.3
import           Control.Lens as X (Lens, Lens', (.@~))
import           Control.Lens.Operators as X
import           Control.Lens.Tuple as X
import           Control.Monad as X (forever, guard, unless, void, when, join)
import           Control.Monad.Reader as X (MonadReader)
import           Control.Monad.Trans.Class as X (lift)
import           Data.ByteString as X (ByteString)
import           Data.Foldable as X (traverse_)
import           Data.Maybe as X (fromMaybe)
import           Data.Monoid as X ((<>))
import           Data.Map as X (Map)
import           Data.Set as X (Set)
import           Data.Text as X (Text)
import qualified Debug.Trace as Trace

import           Prelude.Compat as X

{-# WARNING traceId "Leaving traces in the code" #-}
traceId :: Show a => String -> a -> a
traceId prefix x = Trace.trace (prefix ++ show x) x

{-# WARNING trace "Leaving traces in the code" #-}
trace :: String -> a -> a
trace = Trace.trace

{-# WARNING traceShowM "Leaving traces in the code" #-}
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = Trace.traceShowM

{-# WARNING todo "Leaving todos in the code" #-}
todo :: String -> a
todo = error . ("TODO: " ++)
