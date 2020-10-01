{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}
-- | A StateT-based RWST

module Control.Monad.Trans.FastRWS
    ( RWST, runRWST, rwst, execRWST
    , RWS , runRWS , rws , execRWS
    , mapRWS, mapRWST
    , asks
    ) where

import           Control.Applicative (Alternative(..))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (MonadPlus(..))
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Reader as MonadReader
import qualified Control.Monad.State as MonadState
import           Control.Monad.Trans.Class (MonadTrans(..))
import qualified Control.Monad.Writer as MonadWriter
import           Data.Functor.Identity (Identity(..))

import           Prelude

newtype RWST r w s m a = RWST { unRWST :: r -> w -> s -> m (a, s, w) }
    deriving stock Functor

instance Monad m => Applicative (RWST r w s m) where
    pure x = RWST $ \_ w s -> pure (x, s, w)
    RWST fact <*> RWST xact =
        RWST $ \r w0 s0 ->
        do
            (f, s1, w1) <- fact r w0 s0
            xact r w1 s1 <&> _1 %~ f

instance Monad m => Monad (RWST r w s m) where
    RWST act >>= f =
        RWST $ \r w0 s0 ->
        do
            (a, s1, w1) <- act r w0 s0
            unRWST (f a) r w1 s1

instance MonadIO m => MonadIO (RWST r w s m) where
    liftIO = lift . liftIO

instance MonadPlus m => Alternative (RWST r w s m) where
    empty = RWST $ \_ _ _ -> empty
    RWST x <|> RWST y = RWST $ \r w s -> x r w s <|> y r w s

instance Monad m => MonadState.MonadState s (RWST r w s m) where
    state f = RWST $ \_ w s0 -> let (res, s1) = f s0 in pure (res, s1, w)

instance Monad m => MonadReader.MonadReader r (RWST r w s m) where
    ask = asks id
    local f (RWST act) = RWST $ \r w s -> act (f r) w s

instance (Monoid w, Monad m) => MonadWriter.MonadWriter w (RWST r w s m) where
    tell = tell
    listen = mapRWS $ \(x, s, w) -> ((x, w), s, w)
    pass = mapRWS $ \((x, censor), s, w) -> (x, s, censor w)

instance MonadTrans (RWST r w s) where
    lift act = RWST $ \_ w s -> act <&> (, s, w)

type RWS r w s = RWST r w s Identity

runRWST :: Monoid w => RWST r w s m a -> r -> s -> m (a, s, w)
runRWST act r = unRWST act r mempty

runRWS :: Monoid w => RWS r w s a -> r -> s -> (a, s, w)
runRWS act r s = runRWST act r s & runIdentity

execRWST :: (Monoid w, Functor m) => RWST r w s m a -> r -> s -> m (s, w)
execRWST act r s0 = runRWST act r s0 <&> \(_, s1, w) -> (s1, w)

execRWS :: Monoid w => RWS r w s a -> r -> s -> (s, w)
execRWS act r s = execRWST act r s & runIdentity

rwst :: (Functor m, Monoid w) => (r -> s -> m (a, s, w)) -> RWST r w s m a
rwst act = RWST $ \r w s -> act r s <&> (_3 %~ (w <>))

rws :: Monoid w => (r -> s -> (a, s, w)) -> RWS r w s a
rws act = rwst $ \r s -> Identity $ act r s

mapRWS ::
    (Monoid w, Monoid w', Functor m) =>
    ((a, s, w) -> (b, s, w')) -> RWST r w s m a -> RWST r w' s m b
mapRWS f = mapRWST (<&> f)

mapRWST ::
    (Monoid w, Monoid w', Functor n) =>
    (m (a, s, w) -> n (b, s, w')) -> RWST r w s m a -> RWST r w' s n b
mapRWST f act = runRWST act <&> Lens.mapped %~ f & rwst

tell :: (Applicative m, Monoid w) => w -> RWST r w s m ()
tell w = RWST $ \_r wPrev s -> pure ((), s, wPrev <> w)

asks :: Applicative m => (r -> a) -> RWST r w s m a
asks f = RWST $ \r w s -> pure (f r, s, w)
