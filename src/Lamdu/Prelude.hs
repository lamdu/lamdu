module Lamdu.Prelude
    ( module X
    , traceId, traceIdVia, trace, traceShowM, traceM
    , todo, warn
    ) where

-- .@~ is missing in Control.Lens.Operators in lens-4.15.3
import           Control.Lens as X (Lens, Lens', (.@~))
import           Control.Lens.Operators as X
import           Control.Lens.Tuple as X
import           Control.Monad as X (forever, guard, unless, void, when, join)
import           Control.Monad.Fail as X (MonadFail)
import           Control.Monad.Reader as X (MonadReader)
import           Control.Monad.Trans.Class as X (lift)
import           Data.ByteString as X (ByteString)
import           Data.Has as X (Has(..))
import           Data.Foldable as X (traverse_)
import           Data.Functor.Identity as X (Identity(..))
import           Data.Maybe as X (fromMaybe)
import           Data.Map as X (Map)
import           Data.Semigroup as X (Semigroup(..))
import           Data.Set as X (Set)
import           Data.Text as X (Text)
import qualified Debug.Trace as Trace
import           Generic.Data as X (Generically(..), Generically1(..))
import           GHC.Generics as X (Generic, Generic1)
import           GHC.Stack.Types as X (HasCallStack)

import           Prelude.Compat as X hiding (return)

{-# WARNING traceId "Leaving traces in the code" #-}
traceId :: Show a => String -> a -> a
traceId prefix x = Trace.trace (prefix ++ show x) x

{-# WARNING traceIdVia "Leaving traces in the code" #-}
traceIdVia :: Show b => (a -> b) -> String -> a -> a
traceIdVia f prefix x = Trace.trace (prefix ++ show (f x)) x

{-# WARNING trace "Leaving traces in the code" #-}
trace :: String -> a -> a
trace = Trace.trace

{-# WARNING warn "Leaving warn in the code" #-}
warn :: String -> a -> a
warn _ = id

{-# WARNING traceShowM "Leaving traces in the code" #-}
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = Trace.traceShowM

{-# WARNING traceM "Leaving traces in the code" #-}
traceM :: Applicative f => String -> f ()
traceM = Trace.traceM

{-# WARNING todo "Leaving todos in the code" #-}
todo :: String -> a
todo = error . ("TODO: " ++)
