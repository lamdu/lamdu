{-# LANGUAGE TemplateHaskell #-}

-- | Expression body types that have a single child expression types
-- (Unlike lambdas and let-items).

module Lamdu.Sugar.Types.Simple
    ( CompositeItem(..), ciDelete, ciTag, ciExpr
    , CaseArg(..), caVal, caToLambdaCase
    , CaseKind(..), _LambdaCase, _CaseWithArg
    , Nominal(..), nTId, nVal
    , GetField(..), gfRecord, gfTag
    , V.Apply(..), V.applyFunc, V.applyArg
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Term as V
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Tag
import           Lamdu.Sugar.Types.Type

import           Lamdu.Prelude

data CompositeItem name i o expr = CompositeItem
    { _ciDelete :: o EntityId
    , _ciTag :: TagRef name i o
    , _ciExpr :: expr
    } deriving (Functor, Foldable, Traversable, Generic)

data CaseArg o expr = CaseArg
    { _caVal :: expr
    , _caToLambdaCase :: o EntityId
    } deriving (Functor, Foldable, Traversable, Generic)

data CaseKind o expr
    = LambdaCase
    | CaseWithArg (CaseArg o expr)
    deriving (Functor, Foldable, Traversable, Generic)

data GetField name i o expr = GetField
    { _gfRecord :: expr
    , _gfTag :: TagRef name i o
    } deriving (Functor, Foldable, Traversable, Generic)

data Nominal name expr = Nominal
    { _nTId :: TId name
    , _nVal :: expr
    } deriving (Functor, Foldable, Traversable, Generic)

Lens.makeLenses ''CaseArg
Lens.makeLenses ''CompositeItem
Lens.makeLenses ''GetField
Lens.makeLenses ''Nominal
Lens.makePrisms ''CaseKind
