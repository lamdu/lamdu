{-# LANGUAGE RankNTypes #-}
module Control.Lens.Extended
    ( module Control.Lens
    , singletonAt
    , filteredByIndex
    , OneOf
    ) where

import Control.Lens

import Prelude

filteredByIndex ::
    (Applicative f, Indexable j p) =>
    Fold i j -> p a (f a) -> Indexed i a (f a)
filteredByIndex fold f =
    Indexed $
    \idx val ->
    case idx ^? fold of
    Nothing -> pure val
    Just proof -> indexed f proof val

-- Generalization of Data.Map.singleton
singletonAt :: (At a, Monoid a) => Index a -> IxValue a -> a
singletonAt k v = mempty & at k ?~ v

type OneOf f = forall a. Lens' (f a) a
