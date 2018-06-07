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
    , Function(..)
        , fChosenScopeProp, fParams, fBody
        , fAddFirstParam, fBodyScopes
    , Assignment(..), aBody, aNodeActions
    , AssignFunction(..), afFunction, afLamId
    , AssignPlain(..), apAddFirstParam, apBody
    , AssignmentBody(..), _BodyFunction, _BodyPlain
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
    { _lValue :: Assignment name i o expr -- "let foo = [[bar]] in x"
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

data Function name i o expr = Function
    { _fChosenScopeProp :: i (Property o (Maybe BinderParamScopeId))
    , _fParams :: BinderParams name i o
    , _fBody :: BinderBody name i o expr
    , _fAddFirstParam :: AddFirstParam name i o
    , -- The scope inside a lambda
      _fBodyScopes :: BinderBodyScope
    } deriving (Functor, Foldable, Traversable, Generic)

data AssignFunction name i o expr = AssignFunction
    { _afLamId :: EntityId
    , _afFunction :: Function name i o expr
    } deriving (Functor, Foldable, Traversable, Generic)

data AssignPlain name i o expr = AssignPlain
    { _apAddFirstParam :: AddFirstParam name i o
    , _apBody :: BinderBody name i o expr
    } deriving (Functor, Foldable, Traversable, Generic)

data AssignmentBody name i o expr
    = BodyFunction (AssignFunction name i o expr)
    | BodyPlain (AssignPlain name i o expr)
    deriving (Functor, Foldable, Traversable, Generic)

data Assignment name i o expr = Assignment
    { _aBody :: AssignmentBody name i o expr
    , _aNodeActions :: NodeActions name i o
    } deriving (Functor, Foldable, Traversable, Generic)

Lens.makeLenses ''AssignFunction
Lens.makeLenses ''Assignment
Lens.makeLenses ''AssignPlain
Lens.makeLenses ''BinderBody
Lens.makeLenses ''Function
Lens.makeLenses ''Let
Lens.makePrisms ''AssignmentBody
Lens.makePrisms ''BinderContent
