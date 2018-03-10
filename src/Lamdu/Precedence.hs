{-# LANGUAGE NoImplicitPrelude, DeriveFunctor #-}
module Lamdu.Precedence
    ( Precedence(..)
    , HasPrecedence(..)
    , Prec, minNamePrec, maxNamePrec
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map

import           Lamdu.Prelude

data Precedence a = Precedence
    { before :: a
    , after  :: a
    } deriving (Show, Functor)
instance Applicative Precedence where
    pure = join Precedence
    Precedence af bf <*> Precedence ax bx = Precedence (af ax) (bf bx)

type Prec = Int

-- Lower precedences are reserved for grammars
minNamePrec :: Prec
minNamePrec = 2

-- Higher precedences are reserved for grammars
maxNamePrec :: Prec
maxNamePrec = 12

class HasPrecedence a where
    -- | Returns a precedence between minNamePrec..maxNamePrec
    precedence :: a -> Prec

instance HasPrecedence Char where
    precedence c = precedenceMap ^. Lens.at c & fromMaybe maxNamePrec

-- | Returns a precedence between minNamePrec..(maxNamePrec-1)
-- Inspired by Table 2 in https://www.haskell.org/onlinereport/decls.html
precedenceMap :: Map Char Prec
precedenceMap =
    [ ('$', 2)
    , (';', 3)
    , ('|', 4)
    , ('&', 5)
    ] ++
    [ (c  , 6) | c <- "=><≠≥≤" ] ++
    [ ('.', 7) ] ++
    [ (c  , 8) | c <- "+-" ] ++
    [ (c  , 9) | c <- "*/%" ] ++
    [ ('^', 10)
    , ('!', 11) ]
    & Map.fromList
