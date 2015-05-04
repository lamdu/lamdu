{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.Decycle
  ( DecycleT, runDecycleT, visit
  ) where

import Control.Applicative (Applicative)
import Control.Lens.Operators
import Control.Monad (liftM)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Set (Set)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Set as Set

newtype DecycleT k m a =
  DecycleT (ReaderT (Set k) m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

runDecycleT :: DecycleT k m a -> m a
runDecycleT (DecycleT (ReaderT action)) = action Set.empty

visit :: (Ord k, Monad m) => k -> DecycleT k m a -> DecycleT k m (Maybe a)
visit key (DecycleT action) =
  DecycleT $ do
    visited <- Reader.ask
    if visited ^. Lens.contains key
      then return Nothing
      else Reader.local (Lens.contains key .~ True) . liftM Just $ action
