{-# LANGUAGE ExplicitNamespaces #-}
module Lamdu.Prelude
    ( module X
    , todo, warn
    ) where

import           Control.Lens as X (Lens, Lens')
import           Control.Lens.Operators as X
import           Control.Lens.Tuple as X
import           Control.Monad as X (forever, guard, unless, void, when, join)
import           Control.Monad.Reader as X (MonadReader, local)
import           Control.Monad.Trans.Class as X (lift)
import           Data.Binary as X (Binary)
import           Data.ByteString as X (ByteString)
import           Data.Foldable as X (traverse_)
import           Data.Functor.Const as X (Const(..))
import           Data.Functor.Identity as X (Identity(..))
import           Data.Has as X (Has(..))
import           Data.Map as X (Map)
import           Data.Maybe as X (fromMaybe)
import           Data.Proxy as X (Proxy(..))
import           Data.Semigroup as X (Semigroup(..))
import           Data.Set as X (Set)
import           Data.Text as X (Text)
import           Debug.Trace.Warned as X
import           GHC.Generics as X (Generic, Generic1)
import           GHC.Stack.Types as X (HasCallStack)
import           Generic.Data as X (Generically(..), Generically1(..))
import           Hyper as X (type (#), type (:#), Ann(..), hVal, hAnn, annotation, Pure(..), _Pure, Annotated)

import           Prelude.Compat as X hiding (return)

{-# WARNING warn "Leaving warn in the code" #-}
warn :: String -> a -> a
warn _ = id

{-# WARNING todo "Leaving todos in the code" #-}
todo :: String -> a
todo = error . ("TODO: " ++)
