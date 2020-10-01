{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, UndecidableInstances #-}
-- | A StateT-based WriterT

module Control.Monad.Trans.FastWriter
    ( WriterT, runWriterT, writerT, execWriterT, evalWriterT
    , Writer, runWriter, execWriter , evalWriter
    , module Control.Monad.Writer
    , mapWriter, mapWriterT
    ) where

import Control.Applicative (Alternative)
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State.Strict (StateT(..))
import Control.Monad.Writer (MonadWriter(..))
import Data.Functor.Identity (Identity(..))

import Prelude

newtype WriterT w m a = WriterT { unWriterT :: StateT w m a }
    deriving newtype (Functor, Applicative, Monad, MonadTrans, Alternative)

instance MonadState s m => MonadState s (WriterT w m) where state = lift . state
instance (Monad m, Monoid w) => MonadWriter w (WriterT w m) where
    tell w = WriterT $ StateT $ \w0 -> let w' = w0 <> w in w' `seq` pure ((), w')
    listen = mapWriter $ \(x, w) -> ((x, w), w)
    pass = mapWriter $ \((x, censor), w) -> (x, censor w)
instance MonadIO m => MonadIO (WriterT w m) where liftIO = lift . liftIO

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
writerT act = WriterT $ StateT $ \w -> act <&> (_2 %~ (w <>))

mapWriter :: (Functor m, Monoid w, Monoid w') => ((a, w) -> (b, w')) -> WriterT w m a -> WriterT w' m b
mapWriter f = mapWriterT (<&> f)

mapWriterT :: (Monoid w, Monoid w', Functor n) => (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f act = runWriterT act & f & writerT

