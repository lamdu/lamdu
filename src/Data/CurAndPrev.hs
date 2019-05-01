{-# LANGUAGE TemplateHaskell, DerivingVia #-}

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
    } deriving stock (Functor, Foldable, Traversable, Show, Eq, Ord, Generic, Generic1)
    deriving (Semigroup, Monoid) via Generically (CurAndPrev a)
    deriving Applicative via Generically1 CurAndPrev
Lens.makeLenses ''CurAndPrev

fallbackToPrev :: Alternative f => CurAndPrev (f a) -> f a
fallbackToPrev cp = cp ^. current <|> cp ^. prev

data CurPrevTag = Current | Prev deriving (Eq, Ord, Show, Enum)

curPrevTag :: CurAndPrev CurPrevTag
curPrevTag =
    CurAndPrev
    { _current = Current
    , _prev = Prev
    }
