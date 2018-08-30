{-# LANGUAGE TemplateHaskell #-}

module Data.CurAndPrev
    ( CurAndPrev(..), current, prev
    , fallbackToPrev
    , CurPrevTag(..), curPrevTag
    ) where

import           Control.Applicative (Alternative(..))
import qualified Control.Lens as Lens
import           Control.Lens.Operators

import           Lamdu.Prelude

data CurAndPrev a = CurAndPrev
    { _current :: a
    , _prev :: a
    } deriving (Functor, Foldable, Traversable, Show, Eq, Ord, Generic)
Lens.makeLenses ''CurAndPrev

instance Semigroup a => Semigroup (CurAndPrev a) where
    CurAndPrev c0 p0 <> CurAndPrev c1 p1 = CurAndPrev (c0 <> c1) (p0 <> p1)
instance Monoid a => Monoid (CurAndPrev a) where
    mempty = CurAndPrev mempty mempty
    mappend = (<>)

instance Applicative CurAndPrev where
    pure x = CurAndPrev x x
    CurAndPrev f0 f1 <*> CurAndPrev a0 a1 = CurAndPrev (f0 a0) (f1 a1)

fallbackToPrev :: Alternative f => CurAndPrev (f a) -> f a
fallbackToPrev cp = cp ^. current <|> cp ^. prev

data CurPrevTag = Current | Prev deriving (Eq, Ord, Show, Enum)

curPrevTag :: CurAndPrev CurPrevTag
curPrevTag =
    CurAndPrev
    { _current = Current
    , _prev = Prev
    }
