-- TODO:
-- The makeHTraversableAndBases calls below generate redundant constraints.
-- hypertypes's TH needs to support generating instances for a whole group,
-- so that it knows which other instances it will create.
{-# OPTIONS -Wno-redundant-constraints #-}

{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses, UndecidableInstances, DataKinds, GADTs, ConstraintKinds, FlexibleInstances #-}
module Lamdu.Sugar.Types.Expression
    ( Expr, Body
    , Term(..)
        , _BodyLam, _BodyLabeledApply, _BodySimpleApply
        , _BodyGetVar, _BodyGetField, _BodyInject, _BodyHole
        , _BodyLiteral, _BodyCase, _BodyRecord, _BodyFragment
        , _BodyFromNom, _BodyToNom, _BodyIfElse
    , AnnotatedArg(..), aaTag, aaExpr
    , LabeledApply(..), aFunc, aSpecialArgs, aAnnotatedArgs, aPunnedArgs
    , App(..), appFunc, appArg
    , Fragment(..), fExpr, fHeal, fTypeMismatch, fOptions
    , Lambda(..), lamFunc, lamMode, lamApplyLimit
    , InjectContent(..), _InjectVal, _InjectNullary
    , Inject(..), iTag, iContent
    , GetField(..), gfRecord, gfTag
    , Nominal(..), nTId, nVal
    -- Binders
    , Let(..), lValue, lName, lUsages, lDelete, lBody, lVarInfo
    , Meta.SpecialArgs(..), Meta._Verbose, Meta._Operator
    , Meta.DefinitionState(..)
    , BinderParamScopeId(..), bParamScopeId
    , Binder(..), _BinderLet, _BinderTerm
    , Function(..)
        , fChosenScopeProp, fParams, fBody
        , fAddFirstParam, fBodyScopes
    , AssignPlain(..), apAddFirstParam, apBody
    , Assignment(..), _BodyFunction, _BodyPlain
    -- Holes
    , HoleOption(..), hoEntityId, hoSugaredBaseExpr, hoResults
    , Hole(..), holeOptions, holeMDelete
    , HoleResult(..), holeResultConverted, holeResultPick
    -- If/else
    , IfElse(..), iIf, iThen, iElse
    , Else(..), _SimpleElse, _ElseIf
    -- Record & Cases
    , Composite(..), cItems, cPunnedItems, cAddItem, cTail
    , CompositeItem(..), ciDelete, ciTag, ciExpr
    , CompositeTail(..), _OpenComposite, _ClosedComposite
    , Case(..), cKind, cBody
    , CaseArg(..), caVal, caToLambdaCase
    , CaseKind(..), _LambdaCase, _CaseWithArg
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.ListT (ListT)
import           Data.Property (Property)
import           Hyper
import           Hyper.Type.AST.App (App(..), appFunc, appArg)
import           Lamdu.Data.Anchors (BinderParamScopeId(..), bParamScopeId)
import qualified Lamdu.Data.Meta as Meta
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Eval
import           Lamdu.Sugar.Types.GetVar (GetVar, BinderVarRef, BinderMode)
import           Lamdu.Sugar.Types.Parts
import           Lamdu.Sugar.Types.Tag
import           Lamdu.Sugar.Types.Type

import           Lamdu.Prelude

type Expr e v name i o a = Annotated (Payload v name i o, a) # e v name i o
type Body e v name i o a = e v name i o # Annotated (Payload v name i o, a)

data AnnotatedArg v name i o k = AnnotatedArg
    { _aaTag :: Tag name
    , _aaExpr :: k :# Term v name i o
    } deriving Generic

-- TODO: func + specialArgs into a single sum type so that field order
-- matches gui order, no need for special traversal code
data LabeledApply v name i o k = LabeledApply
    { _aFunc :: k :# Lens.Const (BinderVarRef name o)
    , _aSpecialArgs :: Meta.SpecialArgs (k :# Term v name i o)
    , _aAnnotatedArgs :: [AnnotatedArg v name i o k]
    , _aPunnedArgs :: [k :# Lens.Const (GetVar name o)]
    } deriving Generic

data InjectContent v name i o k
    = InjectNullary (k :# Lens.Const (NullaryVal name i o))
    | InjectVal (k :# Term v name i o)
    deriving Generic

data Inject v name i o f = Inject
    { _iTag :: TagRef name i o
    , _iContent :: InjectContent v name i o f
    } deriving Generic

data GetField v name i o k = GetField
    { _gfRecord :: k :# Term v name i o
    , _gfTag :: TagRef name i o
    } deriving Generic

data Lambda v name i o f = Lambda
    { _lamMode :: BinderMode
    , _lamApplyLimit :: FuncApplyLimit
    , _lamFunc :: Function v name i o f
    } deriving Generic

-- | An expression marked for transformation.
-- Holds an expression to be transformed but acts like a hole.
data Fragment v name i o k = Fragment
    { _fExpr :: k :# Term v name i o
    , _fHeal :: o EntityId
    , _fTypeMismatch :: Maybe (Annotated EntityId # Type name)
    , _fOptions :: i [HoleOption v name i o]
    } deriving Generic

data HoleResult v name i o = HoleResult
    { _holeResultConverted :: Expr Binder v name i o ()
    , _holeResultPick :: o ()
    } deriving Generic

data HoleOption v name i o = HoleOption
    { _hoEntityId :: EntityId
    , _hoSugaredBaseExpr :: i (Expr Binder v name i o ())
    , -- A group in the hole results based on this option
        -- TODO: HoleResult need not have actual eval results
      _hoResults :: ListT i (HoleResultScore, i (HoleResult v name i o))
    } deriving Generic

data Hole v name i o = Hole
    { _holeOptions :: i [HoleOption v name i o]
        -- outer "i" here is used to read index of globals
        -- inner "i" is used to type-check/sugar every val in the option
      -- TODO: Lifter from i to o?
    , -- Changes the structure around the hole to remove the hole.
      -- For example (f _) becomes (f) or (2 + _) becomes 2
      _holeMDelete :: Maybe (o EntityId)
    } deriving Generic

data Else v name i o f
    = SimpleElse (Term v name i o f)
    | ElseIf (IfElse v name i o f)
    deriving Generic

data IfElse v name i o k = IfElse
    { _iIf :: k :# Term v name i o
    , _iThen :: k :# Term v name i o
    , _iElse :: k :# Else v name i o
    } deriving Generic

data CompositeItem v name i o k = CompositeItem
    { _ciDelete :: o EntityId
    , _ciTag :: TagRef name i o
    , _ciExpr :: k :# Term v name i o
    } deriving Generic

data CompositeTail v name i o k
    = OpenComposite (OpenCompositeActions o) (k :# Term v name i o)
    | ClosedComposite (ClosedCompositeActions o)
    deriving Generic

data Composite v name i o k = Composite
    { _cItems :: [CompositeItem v name i o k]
    , -- Punned items are like Haskell's NamedFieldPuns
      _cPunnedItems :: [k :# Lens.Const (GetVar name o)]
    , _cTail :: CompositeTail v name i o k
    , _cAddItem :: TagReplace name i o EntityId
    } deriving Generic

data CaseArg v name i o k = CaseArg
    { _caVal :: k :# Term v name i o
    , _caToLambdaCase :: o EntityId
    } deriving Generic

data CaseKind v name i o k
    = LambdaCase
    | CaseWithArg (CaseArg v name i o k)
    deriving Generic

data Case v name i o k = Case
    { _cKind :: CaseKind v name i o k
    , _cBody :: Composite v name i o k
    } deriving Generic

data Nominal v name i o k = Nominal
    { _nTId :: TId name
    , _nVal :: k :# Binder v name i o
    } deriving Generic

data Term v name i o k
    = BodyLam (Lambda v name i o k)
    | BodySimpleApply (App (Term v name i o) k)
    | BodyLabeledApply (LabeledApply v name i o k)
    | BodyHole (Hole v name i o)
    | BodyLiteral (Literal (Property o))
    | BodyRecord (Composite v name i o k)
    | BodyGetField (GetField v name i o k)
    | BodyCase (Case v name i o k)
    | BodyIfElse (IfElse v name i o k)
    | BodyInject (Inject v name i o k)
    | BodyGetVar (GetVar name o)
    | BodyToNom (Nominal v name i o k)
    | BodyFromNom (TId name)
    | BodyFragment (Fragment v name i o k)
    | BodyPlaceHolder -- Used for hole results, shown as "★"
    deriving Generic

data Let v name i o k = Let
    { _lValue :: k :# Assignment v name i o -- "let foo = [[bar]] in x"
    , _lVarInfo :: VarInfo
    , _lUsages :: [EntityId]
    , _lName :: TagRef name i o -- let [[foo]] = bar in x
    , _lDelete :: o ()
    , _lBody :: k :# Binder v name i o -- "let foo = bar in [[x]]"
    } deriving Generic

-- An expression with 0 or more let items,
-- Appear in a:
-- * Function: "\x -> [[THIS]]"
-- * ToNom: "«X [[THIS]]"
-- * Definition or let item value: "x = [[THIS]]"
-- * Let-item/redex: "let x = y in [[THIS]]"
data Binder v name i o f
    = BinderLet (Let v name i o f)
    | BinderTerm (Term v name i o f)
    deriving Generic

data Function v name i o k = Function
    { _fChosenScopeProp :: i (Property o (Maybe BinderParamScopeId))
    , _fParams :: BinderParams v name i o
    , _fBody :: k :# Binder v name i o
    , _fAddFirstParam :: AddFirstParam name i o
    , -- The scope inside a lambda
      _fBodyScopes :: ParamScopes
    } deriving Generic

data AssignPlain v name i o f = AssignPlain
    { _apAddFirstParam :: AddFirstParam name i o
    , _apBody :: Binder v name i o f
    } deriving Generic

data Assignment v name i o f
    = BodyFunction (Function v name i o f)
    | BodyPlain (AssignPlain v name i o f)
    deriving Generic

Lens.makeLenses ''AnnotatedArg
Lens.makeLenses ''AssignPlain
Lens.makeLenses ''Case
Lens.makeLenses ''CaseArg
Lens.makePrisms ''CaseKind
Lens.makeLenses ''Composite
Lens.makeLenses ''CompositeItem
Lens.makePrisms ''CompositeTail
Lens.makeLenses ''Fragment
Lens.makeLenses ''Function
Lens.makeLenses ''GetField
Lens.makeLenses ''Hole
Lens.makeLenses ''HoleOption
Lens.makeLenses ''HoleResult
Lens.makeLenses ''IfElse
Lens.makeLenses ''Inject
Lens.makeLenses ''LabeledApply
Lens.makeLenses ''Lambda
Lens.makeLenses ''Let
Lens.makeLenses ''Nominal
Lens.makePrisms ''Assignment
Lens.makePrisms ''Binder
Lens.makePrisms ''Else
Lens.makePrisms ''InjectContent
Lens.makePrisms ''Term

traverse makeHTraversableAndBases
    [ ''AnnotatedArg, ''Assignment, ''AssignPlain, ''Binder, ''Case, ''CaseArg
    , ''CaseKind, ''Composite, ''CompositeItem, ''CompositeTail, ''Else
    , ''Fragment, ''Function, ''GetField, ''IfElse, ''Inject, ''InjectContent
    , ''LabeledApply, ''Lambda, ''Let, ''Nominal, ''Term
    ] <&> concat

-- TODO: Replace boilerplate below with TH

instance RNodes (Assignment v name i o)
instance RNodes (Binder v name i o)
instance RNodes (Else v name i o)
instance RNodes (Function v name i o)
instance RNodes (Term v name i o)

type Dep v c name i o =
    ( (c (Assignment v name i o) :: Constraint)
    , c (Binder v name i o)
    , c (Const (BinderVarRef name o))
    , c (Const (NullaryVal name i o))
    , c (Const (GetVar name o))
    , c (Else v name i o)
    , c (Term v name i o)
    )

instance Dep v c name i o => Recursively c (Assignment v name i o)
instance Dep v c name i o => Recursively c (Binder v name i o)
instance Dep v c name i o => Recursively c (Else v name i o)
instance Dep v c name i o => Recursively c (Term v name i o)

instance (Dep v c name i o, c (Function v name i o)) =>
         Recursively c (Function v name i o)

instance RTraversable (Assignment v name i o)
instance RTraversable (Binder v name i o)
instance RTraversable (Else v name i o)
instance RTraversable (Term v name i o)
