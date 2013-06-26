{-# LANGUAGE DeriveFunctor, RankNTypes #-}
module Lamdu.Sugar.AddNames.CPS
  ( CPS(..)
  ) where

import Control.Applicative (Applicative(..))

data CPS m a = CPS { runCPS :: forall r. m r -> m (a, r) }
  deriving (Functor)

instance Functor m => Applicative (CPS m) where
  pure x = CPS $ fmap ((,) x)
  CPS cpsf <*> CPS cpsx =
    CPS (fmap foo . cpsf . cpsx)
    where
      foo (f, (x, r)) = (f x, r)
