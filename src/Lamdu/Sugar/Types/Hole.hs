{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Sugar.Types.Hole
    ( HoleOption(..), hoVal, hoSugaredBaseExpr, hoResults
    , OptionLiteral
    , Hole(..), holeOptions, holeOptionLiteral, holeMDelete
    , HoleResult(..)
        , holeResultConverted
        , holeResultPick
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.ListT (ListT)
import           Data.Functor.Identity (Identity(..))
import           Lamdu.Calc.Val.Annotated (Val)
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Parts (Literal, HoleResultScore)

import           Lamdu.Prelude

data HoleResult o resultExpr = HoleResult
    { _holeResultConverted :: resultExpr
    , _holeResultPick :: o ()
    } deriving (Functor, Foldable, Traversable, Generic)

data HoleOption i o resultExpr = HoleOption
    { _hoVal :: Val ()
    , _hoSugaredBaseExpr :: i resultExpr
    , -- A group in the hole results based on this option
      _hoResults :: ListT i (HoleResultScore, i (HoleResult o resultExpr))
    } deriving (Functor, Generic)

type OptionLiteral i o resultExpr =
    Literal Identity -> i (HoleResultScore, i (HoleResult o resultExpr))

data Hole i o resultExpr = Hole
    { _holeOptions :: i [HoleOption i o resultExpr]
        -- outer "i" here is used to read index of globals
        -- inner "i" is used to type-check/sugar every val in the option
      -- TODO: Lifter from i to o?
    , _holeOptionLiteral :: OptionLiteral i o resultExpr
    , -- Changes the structure around the hole to remove the hole.
      -- For example (f _) becomes (f) or (2 + _) becomes 2
      _holeMDelete :: Maybe (o EntityId)
    } deriving (Functor, Generic)

Lens.makeLenses ''Hole
Lens.makeLenses ''HoleOption
Lens.makeLenses ''HoleResult
