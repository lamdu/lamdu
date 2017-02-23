{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, UndecidableInstances #-}
-- | A StateT-based WriterT

module Control.Monad.Trans.FastWriter
    ( WriterT, runWriterT, writerT, execWriterT, evalWriterT
    , Writer , runWriter , writer , execWriter , evalWriter
    , mapWriter, mapWriterT
    , listen, censor, tell
    ) where

import           Control.Applicative (Alternative)
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad.State (MonadState(..))
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.State.Strict (StateT(..))
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Functor.Identity (Identity(..))
import           Data.Monoid ((<>))

newtype WriterT w m a = WriterT { unWriterT :: StateT w m a }
    deriving (Functor, Applicative, Monad, MonadTrans, Alternative)

instance MonadState s m => MonadState s (WriterT w m) where
    state = lift . state

type Writer w = WriterT w Identity

runWriterT :: Monoid w => WriterT w m a -> m (a, w)
runWriterT act =
    unWriterT act
    & (`runStateT` mempty)

runWriter :: Monoid w => Writer w a -> (a, w)
runWriter act =
    unWriterT act
    & (`runStateT` mempty)
    & runIdentity

execWriterT :: (Monoid w, Functor m) => WriterT w m a -> m w
execWriterT act = runWriterT act <&> snd

evalWriterT :: (Monoid w, Functor m) => WriterT w m a -> m a
evalWriterT act = runWriterT act <&> fst

execWriter :: Monoid w => Writer w a -> w
execWriter act = runWriter act & snd

evalWriter :: Monoid w => Writer w a -> a
evalWriter act = runWriter act & fst

writerT :: (Functor m, Monoid w) => m (a, w) -> WriterT w m a
writerT act = WriterT $ StateT $ \s -> act <&> (_2 %~ (s <>))

writer :: Monoid w => (a, w) -> Writer w a
writer pair = writerT $ Identity pair

listen :: (Monad m, Monoid w) => WriterT w m a -> WriterT w m (a, w)
listen (WriterT act) =
    WriterT $
    do
        prev <- State.get
        State.put mempty
        res <- act
        localWrites <- State.get
        State.put $! prev <> localWrites
        return (res, localWrites)

censor :: Monad m => (w -> w) -> WriterT w m a -> WriterT w m a
censor f (WriterT act) = WriterT $ act <* (id %= f)

mapWriter :: (Monoid w, Monoid w') => ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f act = runWriterT act <&> f & writerT

mapWriterT :: (Monoid w, Monoid w', Functor n) => (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f act = runWriterT act & f & writerT

tell :: (Monad m, Monoid w) => w -> WriterT w m ()
tell w = WriterT $ State.modify' (<> w)
