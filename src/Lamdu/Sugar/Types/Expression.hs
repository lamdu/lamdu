{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses, UndecidableInstances, DataKinds, GADTs, ConstraintKinds, FlexibleInstances #-}
module Lamdu.Sugar.Types.Expression
    ( Expr, Body
    , Term(..)
        , _BodyLam, _BodyLabeledApply, _BodySimpleApply
        , _BodyRecord, _BodyFragment, _BodyLeaf, _BodyNullaryInject
        , _BodyToNom, _BodyIfElse, _BodyPostfixApply, _BodyPostfixFunc
    , Leaf(..), _LeafLiteral, _LeafHole, _LeafGetVar, _LeafInject, _LeafPlaceHolder
    , AnnotatedArg(..), aaTag, aaExpr
    , OperatorArgs(..), oaLhs, oaRhs, oaSwapArguments
    , LabeledApply(..), aFunc, aMOpArgs, aAnnotatedArgs, aPunnedArgs
    , PostfixApply(..), pArg, pFunc
    , PostfixFunc(..), _PfCase, _PfFromNom, _PfGetField
    , App(..), appFunc, appArg
    , Fragment(..), fExpr, fHeal, fTypeMismatch, fOptions
    , Lambda(..), lamFunc, lamMode, lamApplyLimit
    , Nominal(..), nTId, nVal
    -- Binders
    , Let(..), lValue, lName, lUsages, lDelete, lBody
    , Meta.DefinitionState(..)
    , BinderParamScopeId(..), bParamScopeId
    , Binder(..), _BinderLet, _BinderTerm
    , Function(..)
        , fChosenScopeProp, fParams, fBody
        , fAddFirstParam, fBodyScopes
    , AssignPlain(..), apAddFirstParam, apBody
    , Assignment(..), _BodyFunction, _BodyPlain
    -- Holes
    , HoleOption(..), hoEntityId, hoSearchTerms, hoResults
    , Hole(..), holeOptions
    , HoleResult(..), holeResultConverted, holeResultPick
    -- If/else
    , IfElse(..), iIf, iThen, iElse
    , Else(..), _SimpleElse, _ElseIf
    -- Record & Cases
    , Composite(..), cItems, cPunnedItems, cAddItem, cTail
    , CompositeItem(..), ciDelete, ciTag, ciExpr
    , CompositeTail(..), _OpenComposite, _ClosedComposite
    , PunnedVar(..), pvVar, pvTagEntityId

    , MorphWitness(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.ListT (ListT)
import           Data.Property (Property)
import           Data.Kind (Type)
import           Hyper
import           Hyper.Type.AST.App (App(..), appFunc, appArg)
import           Lamdu.Data.Anchors (BinderParamScopeId(..), bParamScopeId)
import qualified Lamdu.Data.Meta as Meta
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Eval (ParamScopes)
import           Lamdu.Sugar.Types.GetVar (GetVar, BinderVarRef, BinderMode)
import           Lamdu.Sugar.Types.Parts
import           Lamdu.Sugar.Types.Tag
import           Lamdu.Sugar.Types.Type (TId)
import qualified Lamdu.Sugar.Types.Type as T

import           Lamdu.Prelude

type Expr e v name (i :: Type -> Type) (o :: Type -> Type) a = Annotated (Payload v o, a) # e v name i o
type Body e v name (i :: Type -> Type) (o :: Type -> Type) a = e v name i o # Annotated (Payload v o, a)

data AnnotatedArg v name i o k = AnnotatedArg
    { _aaTag :: Tag name
    , _aaExpr :: k :# Term v name i o
    } deriving Generic

data OperatorArgs v name i o k = OperatorArgs
    { _oaLhs :: k :# Term v name i o
    , _oaRhs :: k :# Term v name i o
    , _oaSwapArguments :: o ()
    } deriving Generic

-- TODO: func + specialArgs into a single sum type so that field order
-- matches gui order, no need for special traversal code
data LabeledApply v name i o k = LabeledApply
    { _aFunc :: k :# Const (BinderVarRef name o)
    , _aMOpArgs :: Maybe (OperatorArgs v name i o k)
    , _aAnnotatedArgs :: [AnnotatedArg v name i o k]
    , _aPunnedArgs :: [PunnedVar name o k]
    } deriving Generic

data PostfixApply v name i o k = PostfixApply
    { _pArg :: k :# Term v name i o
    , _pFunc :: k :# PostfixFunc v name i o
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
    , _fTypeMismatch :: Maybe (Annotated EntityId # T.Type name)
    , _fOptions :: Hole name i o
    } deriving Generic

data HoleResult name i o = HoleResult
    { _holeResultConverted :: Expr Binder (Annotation () name) name i o ()
    , _holeResultPick :: o ()
    } deriving Generic

data HoleOption name i o = HoleOption
    { _hoEntityId :: EntityId
    , _hoSearchTerms :: i [HoleTerm name]
    , -- A group in the hole results based on this option
        -- TODO: HoleResult need not have actual eval results
      _hoResults :: ListT i (HoleResultScore, i (HoleResult name i o))
    } deriving Generic

newtype Hole name i o = Hole
    { _holeOptions :: i (OptionFilter -> [HoleOption name i o])
        -- outer "i" here is used to read index of globals
        -- inner "i" is used to type-check/sugar every val in the option
      -- TODO: Lifter from i to o?
    } deriving stock Generic

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
    = OpenComposite (k :# Term v name i o)
    | ClosedComposite (ClosedCompositeActions o)
    deriving Generic

data Composite v name i o k = Composite
    { _cItems :: [CompositeItem v name i o k]
    , -- Punned items are like Haskell's NamedFieldPuns
      _cPunnedItems :: [PunnedVar name o k]
    , _cTail :: CompositeTail v name i o k
    , _cAddItem :: TagChoice name i o EntityId
    } deriving Generic

data Nominal v name i o k = Nominal
    { _nTId :: TId name
    , _nVal :: k :# Binder v name i o
    } deriving Generic

data PostfixFunc v name i o k
    = PfCase (Composite v name i o k)
    | PfFromNom (TId name)
    | PfGetField (TagRef name i o)
    deriving Generic

data Leaf name i o
    = LeafLiteral (Literal (Property o))
    | LeafHole (Hole name i o)
    | LeafGetVar (GetVar name o)
    | LeafInject (TagRef name i o)
    | LeafPlaceHolder -- Used for hole results, shown as "★"
    deriving Generic

data Term v name i o k
    = BodyLam (Lambda v name i o k)
    | BodySimpleApply (App (Term v name i o) k)
    | BodyPostfixApply (PostfixApply v name i o k)
    | BodyLabeledApply (LabeledApply v name i o k)
    | BodyRecord (Composite v name i o k)
    | BodyIfElse (IfElse v name i o k)
    | BodyToNom (Nominal v name i o k)
    | BodyPostfixFunc (PostfixFunc v name i o k)
    | BodyNullaryInject (NullaryInject name i o k)
    | BodyFragment (Fragment v name i o k)
    | BodyLeaf (Leaf name i o)
    deriving Generic

data Let v name i o k = Let
    { _lValue :: k :# Assignment v name i o -- "let foo = [[bar]] in x"
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

traverse Lens.makeLenses
    [ ''AnnotatedArg, ''AssignPlain
    , ''Composite, ''CompositeItem, ''Fragment
    , ''Function, ''Hole, ''HoleOption, ''HoleResult
    , ''IfElse, ''LabeledApply, ''Lambda, ''Let
    , ''Nominal, ''OperatorArgs, ''PostfixApply
    ] <&> concat
traverse Lens.makePrisms
    [''Assignment, ''Binder, ''CompositeTail, ''Else, ''Leaf, ''PostfixFunc, ''Term] <&> concat

traverse makeHTraversableAndBases
    [ ''AnnotatedArg, ''Assignment, ''AssignPlain, ''Binder
    , ''Composite, ''CompositeItem, ''CompositeTail, ''Else
    , ''Fragment, ''Function, ''IfElse
    , ''LabeledApply, ''Lambda, ''Let, ''Nominal
    , ''OperatorArgs, ''PostfixApply, ''PostfixFunc, ''Term
    ] <&> concat

traverse makeHMorph
    [ ''Composite, ''IfElse, ''LabeledApply, ''Let, ''OperatorArgs, ''PostfixApply, ''PostfixFunc
    ] <&> concat

-- TODO: Replace boilerplate below with TH

instance RNodes (Assignment v name i o)
instance RNodes (Binder v name i o)
instance RNodes (Else v name i o)
instance RNodes (Function v name i o)
instance RNodes (PostfixFunc v name i o)
instance RNodes (Term v name i o)

type Dep v (c :: HyperType -> Constraint) name i o =
    ( c (Assignment v name i o)
    , c (Binder v name i o)
    , c (Const (BinderVarRef name o))
    , c (Const (GetVar name o))
    , c (Const (TagChoice name i o EntityId))
    , c (Else v name i o)
    , c (PostfixFunc v name i o)
    , c (Term v name i o)
    )

instance Dep v c name i o => Recursively c (Assignment v name i o)
instance Dep v c name i o => Recursively c (Binder v name i o)
instance Dep v c name i o => Recursively c (Else v name i o)
instance Dep v c name i o => Recursively c (PostfixFunc v name i o)
instance Dep v c name i o => Recursively c (Term v name i o)

instance (Dep v c name i o, c (Function v name i o)) => Recursively c (Function v name i o)

instance RTraversable (Assignment v name i o)
instance RTraversable (Binder v name i o)
instance RTraversable (Else v name i o)
instance RTraversable (PostfixFunc v name i o)
instance RTraversable (Term v name i o)
