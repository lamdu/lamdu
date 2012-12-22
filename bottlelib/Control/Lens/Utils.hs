{-# LANGUAGE Rank2Types #-}
module Control.Lens.Utils
  ( SimpleContext
  , lensContext
  , contextSetter, contextVal
  , argument, result
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens, (^.))
import qualified Control.Lens as Lens

type SimpleContext a t = Lens.Context a a t

lensContext :: s -> Lens s t a b -> Lens.Context a b t
lensContext x lens = Lens.Context (flip (Lens.set lens) x) $ x ^. lens

contextSetter :: Lens (Lens.Context a b0 t0) (Lens.Context a b1 t1) (b0 -> t0) (b1 -> t1)
contextSetter f (Lens.Context set val) = (`Lens.Context` val) <$> f set

contextVal :: Lens (Lens.Context a0 b t) (Lens.Context a1 b t) a0 a1
contextVal f (Lens.Context set val) = Lens.Context set <$> f val

argument :: Lens.Setter (a0 -> b) (a1 -> b) a1 a0
argument = Lens.sets (flip (.))

result :: Lens.Setter (a -> b0) (a -> b1) b0 b1
result = Lens.mapped
