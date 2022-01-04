{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses, UndecidableInstances, DataKinds, GADTs, ConstraintKinds, FlexibleInstances #-}
module Lamdu.Sugar.Types.Expression
    ( Expr, Body
    , Term(..)
        , _BodyLam, _BodyLabeledApply, _BodySimpleApply
        , _BodyRecord, _BodyFragment, _BodyLeaf, _BodyNullaryInject
        , _BodyToNom, _BodyIfElse, _BodyPostfixApply, _BodyPostfixFunc
    , Leaf(..), _LeafLiteral, _LeafHole, _LeafGetVar, _LeafInject
    , AnnotatedArg(..), aaTag, aaExpr
    , OperatorArgs(..), oaLhs, oaRhs, oaSwapArguments
    , LabeledApply(..), aFunc, aMOpArgs, aAnnotatedArgs, aPunnedArgs
    , PostfixApply(..), pArg, pFunc
    , PostfixFunc(..), _PfCase, _PfFromNom, _PfGetField
    , App(..), appFunc, appArg
    , Lambda(..), lamFunc, lamMode, lamApplyLimit
    , Nominal(..), nTId, nVal
    -- Binders
    , Let(..), lValue, lName, lUsages, lDelete, lBody
    , Meta.DefinitionState(..)
    , BinderParamScopeId(..), bParamScopeId
    , Binder(..), bBody, bAddOuterLet
    , BinderBody(..), _BinderLet, _BinderTerm
    , Function(..), fChosenScopeProp, fParams, fBody, fBodyScopes
    , AssignPlain(..), apAddFirstParam, apBody
    , Assignment(..), _BodyFunction, _BodyPlain
    -- Holes
    , Hole(..), holeOptions
    , Query(..), qLangInfo, qSearchTerm
    , QueryLangInfo(..), qLangId, qLangDir, qCodeTexts, qUITexts, qNameTexts
        , hasQueryLangInfo
    -- Fragments
    , Fragment(..), fExpr, fHeal, fTypeMismatch, fOptions
    , FragOpt(..), _FragPostfix, _FragInject, _FragApplyFunc, _FragOp
    , FragOperator(..), oFunc, oRightArg, oAnnotatedArgs
    -- If/else
    , IfElse(..), iIf, iThen, iElse
    , Else(..), _SimpleElse, _ElseIf
    , ElseIfBody(..), eAddLet, eIfElse
    -- Record & Cases
    , Composite(..), cList, cPunnedItems, cTail
    , CompositeTail(..), _OpenComposite, _ClosedComposite
    , PunnedVar(..), pvVar, pvTagEntityId

    , MorphWitness(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Unit (Unit)
import           Data.Property (Property)
import           Data.Kind (Type)
import           Hyper
import           Hyper.Syntax (App(..), appFunc, appArg)
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

type Body e v name (i :: Type -> Type) o = e v name i o # Annotated (Payload v o)

data AnnotatedArg v name i o k = AnnotatedArg
    { _aaTag :: Tag name
    , _aaExpr :: k :# Term v name i o
    } deriving Generic

data OperatorArgs v name i o k = OperatorArgs
    { _oaLhs :: k :# Term v name i o
    , _oaRhs :: k :# Term v name i o
    , _oaSwapArguments :: o Bool -- Returns whether fragment were added or removed
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
    , _fTypeMismatch :: Maybe (Annotated EntityId # T.Type name Unit)
    , _fOptions :: i (Query -> i [Option FragOpt name i o])
    } deriving Generic

data FragOpt v name i o k
    = FragPostfix [k :# PostfixFunc v name i o] -- a single option can suggest chaining of multiple post-fix applications
    | FragInject (TagRef name i o)
    | FragWrapInRec (TagRef name i o)
    | FragApplyFunc (GetVar name o)
    | FragOp (FragOperator v name i o k)
    | FragToNom (TId name o)
    | FragLam
    | FragDefer
    | FragIf (k :# Term v name i o)
    | FragArgument (Term v name i o k) -- Apply fragmented expr with argument
    deriving Generic

data FragOperator v name i o k = FragOperator
    { _oFunc :: k :# Const (BinderVarRef name o)
    , -- Argument on right-hand-side (LTR) of operator.
      -- (usually a hole, but may be completed to other values)
      _oRightArg :: k :# Term v name i o
    , _oAnnotatedArgs :: [Tag name]
    } deriving Generic

newtype Hole name i o = Hole
    { _holeOptions ::
        i (Query -> i [Option Binder name i o])
        -- Inner `i` serves two purposes:
        -- Name walk requires monadic place to process names.
        -- Hole can prepare results depending on the query and avoid doing work
        -- if the query filters it out.
    } deriving stock Generic

data Else v name i o f
    = SimpleElse (Term v name i o f)
    | ElseIf (ElseIfBody v name i o f)
    deriving Generic

data ElseIfBody v name i o k = ElseIfBody
    { _eAddLet :: o EntityId
    , _eIfElse :: IfElse v name i o k
    } deriving Generic

data IfElse v name i o k = IfElse
    { _iIf :: k :# Term v name i o
    , _iThen :: k :# Term v name i o
    , _iElse :: k :# Else v name i o
    } deriving Generic

data CompositeTail v name i o k
    = OpenComposite (k :# Term v name i o)
    | ClosedComposite (ClosedCompositeActions o)
    deriving Generic

data Composite v name i o k = Composite
    { _cList :: TaggedList name i o (k :# Term v name i o)
    , -- Punned items are like Haskell's NamedFieldPuns
      _cPunnedItems :: [PunnedVar name o k]
    , _cTail :: CompositeTail v name i o k
    } deriving Generic

data Nominal v name i o k = Nominal
    { _nTId :: TId name o
    , _nVal :: k :# Binder v name i o
    } deriving Generic

data PostfixFunc v name i o k
    = PfCase (Composite v name i o k)
    | PfFromNom (TId name o)
    | PfGetField (TagRef name i o)
    deriving Generic

data Leaf name i o
    = LeafLiteral (Literal (Property o))
    | LeafHole (Hole name i o)
    | LeafGetVar (GetVar name o)
    | LeafInject (TagRef name i o)
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
    , _lName :: OptionalTag name i o -- let [[foo]] = bar in x
    , _lDelete :: o ()
    , _lBody :: k :# Binder v name i o -- "let foo = bar in [[x]]"
    } deriving Generic

-- An expression with 0 or more let items,
-- Appear in a:
-- * Function: "\x -> [[THIS]]"
-- * ToNom: "«X [[THIS]]"
-- * Definition or let item value: "x = [[THIS]]"
-- * Let-item/redex: "let x = y in [[THIS]]"
data Binder v name i o k = Binder
    { _bAddOuterLet :: o EntityId
    , _bBody :: BinderBody v name i o k
    } deriving Generic

data BinderBody v name i o k
    = BinderLet (Let v name i o k)
    | BinderTerm (Term v name i o k)
    deriving Generic

data Function v name i o k = Function
    { _fChosenScopeProp :: i (Property o (Maybe BinderParamScopeId))
    , _fParams :: Params v name i o
    , _fBody :: k :# Binder v name i o
    , -- The scope inside a lambda
      _fBodyScopes :: ParamScopes
    } deriving Generic

data AssignPlain v name i o f = AssignPlain
    { _apAddFirstParam :: o EntityId
    , _apBody :: Binder v name i o f
    } deriving Generic

data Assignment v name i o f
    = BodyFunction (Function v name i o f)
    | BodyPlain (AssignPlain v name i o f)
    deriving Generic

traverse Lens.makeLenses
    [ ''AnnotatedArg, ''AssignPlain, ''Binder
    , ''Composite, ''Fragment, ''FragOperator
    , ''Function, ''Hole
    , ''IfElse, ''ElseIfBody, ''LabeledApply, ''Lambda, ''Let
    , ''Nominal, ''OperatorArgs, ''PostfixApply
    ] <&> concat
traverse Lens.makePrisms
    [''Assignment, ''BinderBody, ''CompositeTail, ''Else, ''FragOpt, ''Leaf, ''PostfixFunc, ''Term] <&> concat

traverse makeHTraversableAndBases
    [ ''AnnotatedArg, ''Assignment, ''AssignPlain, ''Binder, ''BinderBody
    , ''Composite, ''CompositeTail, ''Else, ''ElseIfBody
    , ''Fragment, ''FragOperator, ''FragOpt, ''Function, ''IfElse
    , ''LabeledApply, ''Lambda, ''Let, ''Nominal
    , ''OperatorArgs, ''PostfixApply, ''PostfixFunc, ''Term
    ] <&> concat

traverse makeHMorph
    [ ''Composite, ''FragOperator, ''IfElse, ''LabeledApply, ''Let, ''OperatorArgs, ''PostfixApply, ''PostfixFunc
    ] <&> concat

-- TODO: Replace boilerplate below with TH

instance RNodes (Assignment v name i o)
instance RNodes (Binder v name i o)
instance RNodes (Else v name i o)
instance RNodes (Function v name i o)
instance RNodes (FragOpt v name i o)
instance RNodes (PostfixFunc v name i o)
instance RNodes (Term v name i o)

type Dep v (c :: HyperType -> Constraint) name i o =
    ( c (Assignment v name i o)
    , c (Binder v name i o)
    , c (Const (BinderVarRef name o))
    , c (Const (GetVar name o))
    , c (Const (i (TagChoice name o)))
    , c (Const (TagRef name i o))
    , c (Else v name i o)
    , c (PostfixFunc v name i o)
    , c (Term v name i o)
    )

instance Dep v c name i o => Recursively c (Assignment v name i o)
instance Dep v c name i o => Recursively c (Binder v name i o)
instance Dep v c name i o => Recursively c (Else v name i o)
instance Dep v c name i o => Recursively c (PostfixFunc v name i o)
instance Dep v c name i o => Recursively c (Term v name i o)

instance (Dep v c name i o, c (FragOpt v name i o)) => Recursively c (FragOpt v name i o)
instance (Dep v c name i o, c (Function v name i o)) => Recursively c (Function v name i o)

instance RTraversable (Assignment v name i o)
instance RTraversable (Binder v name i o)
instance RTraversable (Else v name i o)
instance RTraversable (PostfixFunc v name i o)
instance RTraversable (Term v name i o)
