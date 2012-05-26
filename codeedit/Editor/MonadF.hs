{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Editor.MonadF(MonadF) where

import Control.Applicative (Applicative)

class (Monad m, Applicative m) => MonadF m
instance (Monad m, Applicative m) => MonadF m
