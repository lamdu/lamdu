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

data HoleResult am resultExpr = HoleResult
    { _holeResultConverted :: resultExpr
    , _holeResultPick :: am ()
    } deriving (Functor, Foldable, Traversable)

data HoleOption im am resultExpr = HoleOption
    { _hoVal :: Val ()
    , _hoSugaredBaseExpr :: im resultExpr
    , -- A group in the hole results based on this option
      _hoResults :: ListT im (HoleResultScore, im (HoleResult am resultExpr))
    } deriving Functor

type HoleOption' m = HoleOption m m

data Literal f
    = LiteralNum (f Double)
    | LiteralBytes (f ByteString)
    | LiteralText (f Text)

type OptionLiteral im am resultExpr =
    Literal Identity -> im (HoleResultScore, im (HoleResult am resultExpr))

data Hole im am resultExpr = Hole
    { _holeOptions :: im [HoleOption im am resultExpr]
      -- TODO: Lifter from im to am?
    , _holeOptionLiteral :: OptionLiteral im am resultExpr
    , -- Changes the structure around the hole to remove the hole.
      -- For example (f _) becomes (f) or (2 + _) becomes 2
      _holeMDelete :: Maybe (am EntityId)
    } deriving Functor

Lens.makeLenses ''Hole
Lens.makeLenses ''HoleOption
Lens.makeLenses ''HoleResult
Lens.makeLenses ''HoleResultScore
Lens.makePrisms ''Literal
