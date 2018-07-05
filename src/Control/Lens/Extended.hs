{-# LANGUAGE RankNTypes, TupleSections #-}
module Control.Lens.Extended
    ( module Control.Lens
    , singletonAt, tagged
    , filteredBy, filteredByIndex
    ) where

import           Control.Lens

import           Prelude

{-# INLINE tagged #-}
tagged :: Prism' tag () -> Prism' (a, tag) a
tagged p =
    prism (, (p # ()))
    ( \(a, tag1) ->
      case matching p tag1 of
      Left tag2 -> Left (a, tag2)
      Right () -> Right a
    )

filteredBy :: Fold s i -> IndexedTraversal' i s s
filteredBy fold f val =
    case val ^? fold of
    Nothing -> pure val
    Just proof -> indexed f proof val

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
