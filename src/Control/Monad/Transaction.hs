-- | A MonadTransaction mtl-style class for Transactions
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}

module Control.Monad.Transaction
    ( MonadTransaction(..)
    , module X
    , getP, setP, readIRef, writeIRef
    ) where

import           Control.Monad.Once (OnceT)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.State (StateT)
import           Data.Binary (Binary)
import qualified Data.Property as Property
import           Revision.Deltum.IRef (IRef)
import qualified Revision.Deltum.Transaction as Transaction
import           Revision.Deltum.Transaction as X (Transaction)

import           Prelude

type T = Transaction

class (Monad n, Monad m) => MonadTransaction n m | m -> n where
    transaction :: T n a -> m a

instance Monad m => MonadTransaction m (T m) where
    transaction = id

instance MonadTransaction n m => MonadTransaction n (MaybeT    m) where transaction = lift . transaction
instance MonadTransaction n m => MonadTransaction n (StateT  s m) where transaction = lift . transaction
instance MonadTransaction n m => MonadTransaction n (ReaderT r m) where transaction = lift . transaction
instance MonadTransaction n m => MonadTransaction n (ExceptT e m) where transaction = lift . transaction
instance MonadTransaction n m => MonadTransaction n (OnceT m)     where transaction = lift . transaction

getP :: MonadTransaction n m => Property.MkProperty' (T n) a -> m a
getP = transaction . Property.getP

setP :: MonadTransaction n m => Property.MkProperty' (T n) a -> a -> m ()
setP prop = transaction . Property.setP prop

readIRef :: (Binary a, MonadTransaction n m) => IRef n a -> m a
readIRef = transaction . Transaction.readIRef

writeIRef :: (Binary a, MonadTransaction n m) => IRef n a -> a -> m ()
writeIRef iref = transaction . Transaction.writeIRef iref
