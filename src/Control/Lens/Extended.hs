module Control.Lens.Extended
    ( module Control.Lens
    , AnItemLens
    , (~~>)
    , filteredByIndex
    ) where

import           Control.Lens
import qualified Data.Monoid as Monoid

import           Prelude

filteredByIndex ::
    (Applicative f, Indexable j p) =>
    ((j -> Const (Monoid.First j) j) -> i -> Const (Monoid.First j) i) ->
    p a (f a) -> Indexed i a (f a)
filteredByIndex fold f =
    Indexed $
    \idx val ->
    case idx ^? fold of
    Nothing -> pure val
    Just proof -> indexed f proof val

-- Generalization of Data.Map.singleton
{-# ANN (~~>) ("HLint: ignore Use ~~>"::String) #-}
(~~>) :: (At a, Monoid a) => Index a -> IxValue a -> a
k ~~> v = mempty & at k ?~ v

type AnItemLens t a = ALens' (t a) a
