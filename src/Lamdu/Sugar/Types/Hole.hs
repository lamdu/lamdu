{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveTraversable #-}

module Lamdu.Sugar.Types.Hole
    ( HoleOption(..), hoVal, hoSugaredBaseExpr, hoResults
    , Literal(..), _LiteralNum, _LiteralBytes, _LiteralText
    , OptionLiteral
    , Hole(..), holeOptions, holeOptionLiteral
    , HoleResultScore(..), hrsNumFragments, hrsScore
    , HoleResult(..)
        , holeResultConverted
        , holeResultPick
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.ListT (ListT)
import           Data.Functor.Identity (Identity(..))
import           Lamdu.Calc.Val.Annotated (Val)

import           Lamdu.Prelude

data HoleResultScore = HoleResultScore
    { _hrsNumFragments :: !Int
    , _hrsScore :: ![Int]
    } deriving (Eq, Ord)

data HoleResult m resultExpr = HoleResult
    { _holeResultConverted :: resultExpr
    , _holeResultPick :: m ()
    } deriving (Functor, Foldable, Traversable)

data HoleOption m resultExpr = HoleOption
    { _hoVal :: Val ()
    , _hoSugaredBaseExpr :: m resultExpr
    , -- A group in the hole results based on this option
      _hoResults :: ListT m (HoleResultScore, m (HoleResult m resultExpr))
    } deriving Functor

data Literal f
    = LiteralNum (f Double)
    | LiteralBytes (f ByteString)
    | LiteralText (f Text)

type OptionLiteral m resultExpr = Literal Identity -> m (HoleResultScore, m (HoleResult m resultExpr))

data Hole m resultExpr = Hole
    { _holeOptions :: m [HoleOption m resultExpr]
    , _holeOptionLiteral :: OptionLiteral m resultExpr
    }

Lens.makeLenses ''Hole
Lens.makeLenses ''HoleOption
Lens.makeLenses ''HoleResult
Lens.makeLenses ''HoleResultScore
Lens.makePrisms ''Literal
