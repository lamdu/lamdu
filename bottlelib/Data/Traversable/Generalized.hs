{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module Data.Traversable.Generalized
    ( GTraversable(..)
    ) where

import Control.Lens (Settable, mapped, _1, _2, _Just, _Wrapped)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT)
import Data.Distributive (Distributive(..))
import Data.Functor.Identity (Identity(..))
import Data.Set (Set)
import Data.Set.Lens (setmapped)
import GHC.Exts (Constraint)

class GTraversable t where
    type Constraints t (f :: * -> *) :: Constraint
    type Constraints t f = ()
    type ValConstraints t a :: Constraint
    type ValConstraints t a = ()
    gTraverse ::
        (Functor f, Constraints t f, ValConstraints t a, ValConstraints t b) =>
        (a -> f b) -> t a -> f (t b)

instance GTraversable Identity where
    gTraverse = _Wrapped

instance GTraversable [] where
    type Constraints [] f = Applicative f
    gTraverse = traverse

instance GTraversable Set where
    type Constraints Set f = Settable f
    type ValConstraints Set a = Ord a
    gTraverse = setmapped

instance GTraversable Maybe where
    type Constraints Maybe f = Applicative f
    gTraverse = traverse

instance GTraversable (Either e) where
    type Constraints (Either e) f = Applicative f
    gTraverse = traverse

instance GTraversable ((,) w) where
    gTraverse = _2

instance GTraversable ((->) r) where
    type Constraints ((->) r) f = Distributive f
    gTraverse = collect

instance GTraversable m => GTraversable (MaybeT m) where
    type Constraints (MaybeT m) f = (Constraints m f, Applicative f)
    type ValConstraints (MaybeT m) a = (ValConstraints m (Maybe a))
    gTraverse = _Wrapped . gTraverse . _Just

instance GTraversable m => GTraversable (WriterT w m) where
    type Constraints (WriterT w m) f = Constraints m f
    type ValConstraints (WriterT w m) a = ValConstraints m (a, w)
    gTraverse = _Wrapped . gTraverse . _1

instance GTraversable m => GTraversable (ReaderT r m) where
    type Constraints (ReaderT r m) f = (Constraints m f, Settable f)
    type ValConstraints (ReaderT r m) a = ValConstraints m a
    gTraverse = _Wrapped . mapped . gTraverse

instance GTraversable m => GTraversable (StateT s m) where
    type Constraints (StateT s m) f = (Constraints m f, Settable f)
    type ValConstraints (StateT s m) a = ValConstraints m (a, s)
    gTraverse = _Wrapped . mapped . gTraverse . _1
