{-# LANGUAGE TypeFamilies, UndecidableInstances, ConstraintKinds #-}

module Data.Traversable.Generalized
    ( GTraversable(..), Constraints
    ) where

import Control.Lens (Settable, mapped, _1, _2, _Just, _Wrapped)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT)
import Data.Distributive (Distributive(..))
import Data.Functor.Identity (Identity(..))
import GHC.Exts (Constraint)

type Constraints t f = (GTraversable t, Functor f, GTConstraints t f)

class Functor t => GTraversable t where
    type GTConstraints t (f :: * -> *) :: Constraint
    type GTConstraints t f = ()
    gTraverse :: Constraints t f => (a -> f b) -> t a -> f (t b)

instance GTraversable Identity where
    gTraverse = _Wrapped

instance GTraversable [] where
    type GTConstraints [] f = Applicative f
    gTraverse = traverse

instance GTraversable Maybe where
    type GTConstraints Maybe f = Applicative f
    gTraverse = traverse

instance GTraversable (Either e) where
    type GTConstraints (Either e) f = Applicative f
    gTraverse = traverse

instance GTraversable ((,) w) where
    gTraverse = _2

instance GTraversable ((->) r) where
    type GTConstraints ((->) r) f = Distributive f
    gTraverse = collect

instance GTraversable m => GTraversable (MaybeT m) where
    type GTConstraints (MaybeT m) f = (GTConstraints m f, Applicative f)
    gTraverse = _Wrapped . gTraverse . _Just

instance GTraversable m => GTraversable (WriterT w m) where
    type GTConstraints (WriterT w m) f = GTConstraints m f
    gTraverse = _Wrapped . gTraverse . _1

instance GTraversable m => GTraversable (ReaderT r m) where
    type GTConstraints (ReaderT r m) f = (GTConstraints m f, Settable f)
    gTraverse = _Wrapped . mapped . gTraverse

instance GTraversable m => GTraversable (StateT s m) where
    type GTConstraints (StateT s m) f = (GTConstraints m f, Settable f)
    gTraverse = _Wrapped . mapped . gTraverse . _1
