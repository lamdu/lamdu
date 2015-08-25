{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}
module Control.Lens.Utils
    ( Context'
    , contextSetter, contextVal
    , _fromJust
    , addListContexts
    , addTuple2Contexts
    , getPrism
    ) where

import           Prelude.Compat

import           Control.Lens (Lens)
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Data.Maybe (fromMaybe)

_fromJust :: String -> Lens.Iso (Maybe a) (Maybe b) a b
_fromJust msg = Lens.iso (fromMaybe (error msg)) Just
{-# INLINE _fromJust #-}

type Context' a t = Lens.Context a a t

contextSetter :: Lens (Lens.Context a b0 t0) (Lens.Context a b1 t1) (b0 -> t0) (b1 -> t1)
contextSetter f (Lens.Context set val) = f set <&> (`Lens.Context` val)

contextVal :: Lens (Lens.Context a0 b t) (Lens.Context a1 b t) a0 a1
contextVal f (Lens.Context set val) = f val <&> Lens.Context set

addListContexts :: (a -> b) -> Lens.Context [a] [b] t -> [Lens.Context a b t]
addListContexts _   (Lens.Context _         []) = []
addListContexts tob (Lens.Context fromBList (a:as)) =
    Lens.Context (fromBList . (:bs)) a :
    addListContexts tob (Lens.Context (fromBList . (b:)) as)
    where
        b = tob a
        bs = map tob as

-- TODO: can use composition with _2 instead ?
addTuple2Contexts :: Lens.Context (z, a) (z, b) t -> (z, Lens.Context a b t)
addTuple2Contexts (Lens.Context fromBTuple (z, a)) =
    (z, Lens.Context chg a)
    where
        chg b = fromBTuple (z, b)

getPrism :: Lens.Prism s t a b -> s -> Either a t
getPrism p = p Left
