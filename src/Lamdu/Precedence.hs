{-# LANGUAGE NoImplicitPrelude, DeriveFunctor #-}
module Lamdu.Precedence
    ( Precedence(..)
    , HasPrecedence(..)
    ) where

import Lamdu.Prelude

data Precedence a = Precedence
    { before :: a
    , after  :: a
    } deriving (Show, Functor)
instance Applicative Precedence where
    pure = join Precedence
    Precedence af bf <*> Precedence ax bx = Precedence (af ax) (bf bx)

class HasPrecedence a where
    precedence :: a -> Int
