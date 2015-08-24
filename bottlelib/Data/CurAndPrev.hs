{-# LANGUAGE NoImplicitPrelude, RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Data.CurAndPrev
    ( CurAndPrev(..), current, prev
    ) where

import           Prelude.Compat

import           Control.Lens (Lens')
import           Control.Lens.Operators

data CurAndPrev a = CurAndPrev
    { _current :: a
    , _prev :: a
    } deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

instance Monoid a => Monoid (CurAndPrev a) where
    mempty = CurAndPrev mempty mempty
    mappend (CurAndPrev c0 p0) (CurAndPrev c1 p1) =
        CurAndPrev (mappend c0 c1) (mappend p0 p1)

instance Applicative CurAndPrev where
    pure x = CurAndPrev x x
    CurAndPrev f0 f1 <*> CurAndPrev a0 a1 = CurAndPrev (f0 a0) (f1 a1)

current :: Lens' (CurAndPrev a) a
current f CurAndPrev{..} = f _current <&> \_current -> CurAndPrev{..}

prev :: Lens' (CurAndPrev a) a
prev f CurAndPrev{..} = f _prev <&> \_prev -> CurAndPrev{..}
