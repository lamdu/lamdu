{-# LANGUAGE TemplateHaskell #-}

-- | Expression body types that have a single child expression types
-- (Unlike lambdas and let-items).

module Lamdu.Sugar.Types.Simple
    ( -- record:
      CompositeItem(..), ciDelete, ciTag, ciExpr
    , Composite(..), cItems, cAddItem, cTail
    -- case
    , CaseArg(..), caVal, caToLambdaCase
    , CaseKind(..), _LambdaCase, _CaseWithArg
    , Case(..), cKind, cBody
    , ElseIfContent(..), eiScopes, eiEntityId, eiContent, eiCondAddLet, eiNodeActions
    , Else(..), _SimpleElse, _ElseIf
    , IfThen(..), itIf, itThen, itDelete
    , IfElse(..), iIfThen, iElse
    , Nominal(..), nTId, nVal
    --
    , GetField(..), gfRecord, gfTag
    , V.Apply(..), V.applyFunc, V.applyArg
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Val as V
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Eval (ChildScopes)
import           Lamdu.Sugar.Types.Parts
import           Lamdu.Sugar.Types.Tag
import           Lamdu.Sugar.Types.Type

import           Lamdu.Prelude

{- Composites start -}
data CompositeItem name i o expr = CompositeItem
    { _ciDelete :: o EntityId
    , _ciTag :: Tag name i o
    , _ciExpr :: expr
    } deriving (Functor, Foldable, Traversable, Generic)

data Composite name i o expr = Composite
    { _cItems :: [CompositeItem name i o expr]
    , _cTail :: CompositeTail o expr
    , _cAddItem :: TagSelection name i o EntityId
    } deriving (Functor, Foldable, Traversable, Generic)

data CaseArg o expr = CaseArg
    { _caVal :: expr
    , _caToLambdaCase :: o EntityId
    } deriving (Functor, Foldable, Traversable, Generic)

data CaseKind o expr
    = LambdaCase
    | CaseWithArg (CaseArg o expr)
    deriving (Functor, Foldable, Traversable, Generic)

data Case name i o expr = Case
    { _cKind :: CaseKind o expr
    , _cBody :: Composite name i o expr
    } deriving (Functor, Foldable, Traversable, Generic)
{- Composites end -}

-- An "if/elif <cond>: <then>" clause in an IfElse expression
data IfThen o expr = IfThen
    { _itIf :: expr
    , _itThen :: expr
    , _itDelete :: o EntityId
    } deriving (Functor, Foldable, Traversable, Generic)

-- An "elif <cond>: <then>" clause in an IfElse expression and the subtree under it
data ElseIfContent name i o expr = ElseIfContent
    { _eiScopes :: ChildScopes
    , _eiEntityId :: EntityId
    , _eiContent :: IfElse name i o expr
    , _eiCondAddLet :: o EntityId
    , _eiNodeActions :: NodeActions name i o
    } deriving (Functor, Foldable, Traversable, Generic)

data Else name i o expr = SimpleElse expr | ElseIf (ElseIfContent name i o expr)
    deriving (Functor, Foldable, Traversable, Generic)

data IfElse name i o expr = IfElse
    { _iIfThen :: IfThen o expr
    , _iElse :: Else name i o expr
    } deriving (Functor, Foldable, Traversable, Generic)

data GetField name i o expr = GetField
    { _gfRecord :: expr
    , _gfTag :: Tag name i o
    } deriving (Functor, Foldable, Traversable, Generic)

data Nominal name expr = Nominal
    { _nTId :: TId name
    , _nVal :: expr
    } deriving (Functor, Foldable, Traversable, Generic)

Lens.makeLenses ''Case
Lens.makeLenses ''CaseArg
Lens.makeLenses ''Composite
Lens.makeLenses ''CompositeItem
Lens.makeLenses ''ElseIfContent
Lens.makeLenses ''GetField
Lens.makeLenses ''IfElse
Lens.makeLenses ''IfThen
Lens.makeLenses ''Nominal
Lens.makePrisms ''CaseKind
Lens.makePrisms ''Else
