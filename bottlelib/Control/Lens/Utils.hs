{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Control.Lens.Utils
  ( Context'
  , contextSetter, contextVal
  , argument, result
  , _fromJust, allElementsOf
  ) where

import Control.Applicative (Applicative, (<$>))
import Control.Lens (Lens, LensLike, IndexedLensLike)
import Control.Lens.Internal.Indexed (Indexing(..))
import Data.Maybe (fromMaybe)
import qualified Control.Lens as Lens

type Context' a t = Lens.Context a a t

contextSetter :: Lens (Lens.Context a b0 t0) (Lens.Context a b1 t1) (b0 -> t0) (b1 -> t1)
contextSetter f (Lens.Context set val) = (`Lens.Context` val) <$> f set

contextVal :: Lens (Lens.Context a0 b t) (Lens.Context a1 b t) a0 a1
contextVal f (Lens.Context set val) = Lens.Context set <$> f val

argument :: Lens.Setter (a0 -> b) (a1 -> b) a1 a0
argument = Lens.sets (flip (.))

result :: Lens.Setter (a -> b0) (a -> b1) b0 b1
result = Lens.mapped

-- TODO: Into lens...
allElementsOf ::
  Applicative f =>
  LensLike (Indexing f) s t a b -> IndexedLensLike Int f s t a b
allElementsOf l iafb s = snd $ runIndexing (l (\a -> Indexing (\i -> i `seq` (i + 1, Lens.indexed iafb i a))) s) 0

_fromJust :: String -> Lens.Iso (Maybe a) (Maybe b) a b
_fromJust msg = Lens.iso (fromMaybe (error msg)) Just
{-# INLINE _fromJust #-}
