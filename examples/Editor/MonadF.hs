{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Editor.MonadF(MonadF) where

class (Monad m, Functor m) => MonadF m
instance (Monad m, Functor m) => MonadF m
