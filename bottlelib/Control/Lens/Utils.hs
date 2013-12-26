{-# LANGUAGE RankNTypes #-}
module Control.Lens.Utils
  ( Context'
  , contextSetter, contextVal
  , _fromJust
  , addListContexts
  , addTuple2Contexts
  , getPrism
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens)
import Data.Maybe (fromMaybe)
import qualified Control.Lens as Lens

_fromJust :: String -> Lens.Iso (Maybe a) (Maybe b) a b
_fromJust msg = Lens.iso (fromMaybe (error msg)) Just
{-# INLINE _fromJust #-}

type Context' a t = Lens.Context a a t

contextSetter :: Lens (Lens.Context a b0 t0) (Lens.Context a b1 t1) (b0 -> t0) (b1 -> t1)
contextSetter f (Lens.Context set val) = (`Lens.Context` val) <$> f set

contextVal :: Lens (Lens.Context a0 b t) (Lens.Context a1 b t) a0 a1
contextVal f (Lens.Context set val) = Lens.Context set <$> f val

addListContexts :: (a -> b) -> Lens.Context [a] [b] t -> [Lens.Context a b t]
addListContexts _   (Lens.Context _         []) = []
addListContexts tob (Lens.Context fromBList (a:as)) =
  Lens.Context (fromBList . (:bs)) a :
  addListContexts tob (Lens.Context (fromBList . (b:)) as)
  where
    b = tob a
    bs = map tob as

addTuple2Contexts :: (a -> b) -> Lens.Context (a, a) (b, b) t -> (Lens.Context a b t, Lens.Context a b t)
addTuple2Contexts tob (Lens.Context fromBTuple (a0, a1)) =
  (Lens.Context chg0 a0, Lens.Context chg1 a1)
  where
    chg0 b0 = fromBTuple (b0, tob a1)
    chg1 b1 = fromBTuple (tob a0, b1)

getPrism :: Lens.Prism s t a b -> s -> Either a t
getPrism p = p Left
