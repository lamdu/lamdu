{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Control.MonadA(MonadA) where

import Control.Applicative (Applicative)

class (Monad m, Applicative m) => MonadA m
instance (Monad m, Applicative m) => MonadA m
