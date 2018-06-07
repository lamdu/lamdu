{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module Lamdu.Sugar.Types.Binder
    ( Let(..)
        , lEntityId, lValue, lName, lUsages
        , lActions, lAnnotation, lBodyScope, lBody
    -- Binders
    , Meta.SpecialArgs(..), Meta._Verbose, Meta._Object, Meta._Infix
    , Meta.DefinitionState(..)
    , BinderParamScopeId(..), bParamScopeId
    , BinderBody(..), bbAddOuterLet, bbContent
    , BinderContent(..), _BinderLet, _BinderExpr
    , Binder(..)
        , bChosenScopeProp, bParams, bBody
        , bLamId, bActions, bBodyScopes
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property)
import           Lamdu.Data.Anchors (BinderParamScopeId(..), bParamScopeId)
import qualified Lamdu.Data.Meta as Meta
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Eval
import           Lamdu.Sugar.Types.Parts
import           Lamdu.Sugar.Types.Tag

import           Lamdu.Prelude

data Let name i o expr = Let
    { _lValue :: Binder name i o expr -- "let foo = [[bar]] in x"
    , _lEntityId :: EntityId
    , _lUsages :: [EntityId]
    , _lAnnotation :: Annotation name
    , _lName :: Tag name i o -- let [[foo]] = bar in x
    , _lActions :: LetActions name i o
    , _lBodyScope :: ChildScopes
    , _lBody :: BinderBody name i o expr -- "let foo = bar in [[x]]"
    } deriving (Functor, Foldable, Traversable, Generic)

data BinderContent name i o expr
    = BinderLet (Let name i o expr)
    | BinderExpr expr
    deriving (Functor, Foldable, Traversable, Generic)

-- An expression with 0 or more let items,
-- Appear in a:
-- * Function: "\x -> [[THIS]]"
-- * ToNom: "«X [[THIS]]"
-- * Definition or let item value: "x = [[THIS]]"
-- * Let-item/redex: "let x = y in [[THIS]]"
data BinderBody name i o expr = BinderBody
    { _bbAddOuterLet :: o EntityId
    , _bbContent :: BinderContent name i o expr
    } deriving (Functor, Foldable, Traversable, Generic)

data Binder name i o expr = Binder
    { _bChosenScopeProp :: i (Property o (Maybe BinderParamScopeId))
    , _bParams :: BinderParams name i o
    , _bLamId :: Maybe EntityId
    , _bBody :: BinderBody name i o expr
    , _bActions :: BinderActions name i o
    , -- The scope inside a lambda (if exists)
      _bBodyScopes :: BinderBodyScope
    } deriving (Functor, Foldable, Traversable, Generic)

Lens.makeLenses ''Binder
Lens.makeLenses ''BinderBody
Lens.makeLenses ''Let
Lens.makePrisms ''BinderContent
