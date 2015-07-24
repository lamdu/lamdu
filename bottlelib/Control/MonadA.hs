{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module Control.MonadA(MonadA) where

import Prelude.Compat

class (Monad m, Applicative m) => MonadA m
instance (Monad m, Applicative m) => MonadA m
