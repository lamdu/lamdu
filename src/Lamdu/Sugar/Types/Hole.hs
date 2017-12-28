{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveTraversable #-}

module Lamdu.Sugar.Types.Hole
    ( Unwrap(..), _UnwrapAction, _UnwrapTypeMismatch
    , HoleArg(..), haExpr, haUnwrap
    , HoleOption(..), hoVal, hoSugaredBaseExpr, hoResults
    , LeafHoleActions(..), holeOptionLiteral
    , Literal(..), _LiteralNum, _LiteralBytes, _LiteralText
    , HoleKind(..), _LeafHole, _WrapperHole
    , Hole(..), holeOptions, holeKind
    , HoleResultScore(..), hrsNumHoleWrappers, hrsScore
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
    { _hrsNumHoleWrappers :: !Int
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

newtype LeafHoleActions m resultExpr = LeafHoleActions
    { _holeOptionLiteral :: Literal Identity -> m (HoleResultScore, m (HoleResult m resultExpr))
    } deriving Functor

data Unwrap m
    = UnwrapAction (m EntityId)
    | UnwrapTypeMismatch

data HoleArg m expr = HoleArg
    { _haExpr :: expr
    , _haUnwrap :: Unwrap m
    } deriving (Functor, Foldable, Traversable)

data HoleKind m resultExpr expr
    = LeafHole (LeafHoleActions m resultExpr)
    | WrapperHole (HoleArg m expr)
    deriving (Functor, Foldable, Traversable)

data Hole m resultExpr expr = Hole
    { _holeOptions :: m [HoleOption m resultExpr]
    , _holeKind :: HoleKind m resultExpr expr
    } deriving (Functor, Foldable, Traversable)

Lens.makeLenses ''Hole
Lens.makeLenses ''HoleArg
Lens.makeLenses ''HoleOption
Lens.makeLenses ''HoleResult
Lens.makeLenses ''HoleResultScore
Lens.makeLenses ''LeafHoleActions
Lens.makePrisms ''HoleKind
Lens.makePrisms ''Literal
Lens.makePrisms ''Unwrap
