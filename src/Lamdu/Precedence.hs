{-# LANGUAGE TemplateHaskell, DerivingVia #-}
module Lamdu.Precedence
    ( Precedence(..), before, after
    , HasPrecedence(..)
    , Prec, minNamePrec, maxNamePrec
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map

import           Lamdu.Prelude

data Precedence a = Precedence
    { _before :: a
    , _after  :: a
    } deriving stock (Generic, Generic1, Show, Functor)
    deriving Applicative via Generically1 Precedence

Lens.makeLenses ''Precedence

type Prec = Int

-- For the rest of the precedence integer values, see Lamdu.Sugar.Parens

-- Lower precedences are reserved for grammars
minNamePrec :: Prec
minNamePrec = 1

-- Higher precedences are reserved for grammars
maxNamePrec :: Prec
maxNamePrec = 13

class HasPrecedence a where
    -- | Returns a precedence between minNamePrec..maxNamePrec
    precedence :: a -> Prec

instance HasPrecedence Char where
    precedence c = precedenceMap ^. Lens.at c & fromMaybe maxNamePrec

-- | Returns a precedence between minNamePrec..(maxNamePrec-1)
-- Inspired by Table 2 in https://www.haskell.org/onlinereport/decls.html
precedenceMap :: Map Char Prec
precedenceMap =
    [ (':', 1)
    , ('$', 2)
    , (';', 3)
    , ('|', 4)
    , ('&', 5)
    ] ++
    [ (c  , 6) | c <- "=><≠≥≤" ] ++
    [ (c  , 8) | c <- "+-" ] ++
    [ (c  , 9) | c <- "*/%" ] ++
    [ ('^', 10)
    , ('!', 11)
    , ('.', 12) ]
    & Map.fromList
