{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -fno-warn-orphans #-}
module Control.Monad.Reader.Instances () where

import Control.Monad.Reader
import Data.Monoid (Ap(..))

import Lamdu.Prelude

deriving via Ap (ReaderT env m) a instance (Applicative m, Semigroup a) => Semigroup (ReaderT env m a)
deriving via Ap (ReaderT env m) a instance (Applicative m, Monoid a) => Monoid (ReaderT env m a)
