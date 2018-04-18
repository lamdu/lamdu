{-# LANGUAGE RankNTypes #-}
module Control.Lens.Extended
    ( module Control.Lens
    , singletonAt, tagged
    ) where

import           Control.Lens

import           Prelude

{-# INLINE tagged #-}
tagged :: Prism' tag () -> Prism' (a, tag) a
tagged p =
    prism (flip (,) (p # ()))
    ( \(a, tag1) ->
      case matching p tag1 of
      Left tag2 -> Left (a, tag2)
      Right () -> Right a
    )

-- Generalization of Data.Map.singleton
singletonAt :: (At a, Monoid a) => Index a -> IxValue a -> a
singletonAt k v = mempty & at k ?~ v
