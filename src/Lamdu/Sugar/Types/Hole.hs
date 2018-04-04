{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Sugar.Types.Hole
    ( HoleOption(..), hoVal, hoSugaredBaseExpr, hoResults
    , HoleOption'
    , Literal(..), _LiteralNum, _LiteralBytes, _LiteralText
    , OptionLiteral
    , Hole(..), holeOptions, holeOptionLiteral, holeMDelete
    , HoleResultScore(..), hrsNumFragments, hrsScore
    , HoleResult(..)
        , holeResultConverted
        , holeResultPick
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.ListT (ListT)
import           Data.Functor.Identity (Identity(..))
import           Lamdu.Calc.Val.Annotated (Val)
import           Lamdu.Sugar.Internal.EntityId (EntityId)

import           Lamdu.Prelude

data HoleResultScore = HoleResultScore
    { _hrsNumFragments :: !Int
    , _hrsScore :: ![Int]
    } deriving (Eq, Ord)

data HoleResult o resultExpr = HoleResult
    { _holeResultConverted :: resultExpr
    , _holeResultPick :: o ()
    } deriving (Functor, Foldable, Traversable)

data HoleOption i o resultExpr = HoleOption
    { _hoVal :: Val ()
    , _hoSugaredBaseExpr :: i resultExpr
    , -- A group in the hole results based on this option
      _hoResults :: ListT i (HoleResultScore, i (HoleResult o resultExpr))
    } deriving Functor

type HoleOption' m = HoleOption m m

data Literal f
    = LiteralNum (f Double)
    | LiteralBytes (f ByteString)
    | LiteralText (f Text)

type OptionLiteral i o resultExpr =
    Literal Identity -> i (HoleResultScore, i (HoleResult o resultExpr))

data Hole i o resultExpr = Hole
    { _holeOptions :: i [HoleOption i o resultExpr]
      -- TODO: Lifter from i to o?
    , _holeOptionLiteral :: OptionLiteral i o resultExpr
    , -- Changes the structure around the hole to remove the hole.
      -- For example (f _) becomes (f) or (2 + _) becomes 2
      _holeMDelete :: Maybe (o EntityId)
    } deriving Functor

Lens.makeLenses ''Hole
Lens.makeLenses ''HoleOption
Lens.makeLenses ''HoleResult
Lens.makeLenses ''HoleResultScore
Lens.makePrisms ''Literal
