{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TemplateHaskell #-}

module Control.Monad.Once
    ( MonadOnce(..)
    , OnceT(..), _OnceT
    , OnceState
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Dynamic (Dynamic, Typeable, toDyn, fromDynamic)
import           Data.IORef
import qualified Data.Sequence as Sequence

import           Lamdu.Prelude

class MonadOnce m where
    once :: Typeable a => m a -> m (m a)

instance MonadOnce IO where
    once a =
        newIORef Nothing
        <&>
        \r ->
        readIORef r
        >>=
        \case
        Just x -> pure x
        Nothing ->
            do
                x <- a
                x <$ writeIORef r (Just x)

type OnceState = Sequence.Seq Dynamic

newtype OnceT m a = OnceT (StateT OnceState m a)
    deriving newtype (Functor, Applicative, Monad, MonadTrans)

instance (Semigroup a, Monad m) => Semigroup (OnceT m a) where
    a <> b = (<>) <$> a <*> b

Lens.makePrisms ''OnceT

instance Monad m => MonadOnce (OnceT m) where
    once (a :: OnceT m a) =
        id <<%= (|> toDyn (Nothing :: Maybe a)) <&> Sequence.length & OnceT
        <&>
        \r ->
        OnceT (Lens.use id) <&> (^?! Lens.ix r) <&> fromDynamic
        >>=
        \case
        Just (Just x) -> pure x
        Just Nothing ->
            do
                x <- a
                x <$ OnceT (Lens.ix r .= toDyn (Just x))
        Nothing -> error "Once used incorrectly!"
