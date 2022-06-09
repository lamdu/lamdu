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
    , Lambda(..), lamFunc, lamLightweight, lamApplyLimit
    , Nominal(..), nTId, nVal
    -- Binders
    , Let(..), lValue, lNames, lBody
    , Meta.DefinitionState(..)
    , BinderParamScopeId(..), bParamScopeId
    , Binder(..), bBody, bAddOuterLet
    , BinderBody(..), _BinderLet, _BinderTerm
    , Function(..), fChosenScopeProp, fParams, fBody, fBodyScopes
    , AssignPlain(..), apAddFirstParam, apBody
    , Assignment(..), _BodyFunction, _BodyPlain
    -- Holes
    , Hole(..), holeOptions, holeTagSuffixes
    , HoleOpt(..), _HoleBinder, _HoleVarsRecord
    , Query(..), qLangInfo, qSearchTerm
    , QueryLangInfo(..), qLangId, qLangDir, qCodeTexts, qUITexts, qNameTexts
        , hasQueryLangInfo
    -- Fragments
    , Fragment(..), fExpr, fHeal, fTypeMismatch, fOptions, fOptApply, fTagSuffixes
    , FragOpt(..), _FragPostfix, _FragInject, _FragApplyFunc, _FragOp
    , FragOperator(..), oFunc, oRightArg, oAnnotatedArgs
    -- If/else
    , IfElse(..), iIf, iThen, iElse
    , Else(..), _SimpleElse, _ElseIf
    , ElseIfBody(..), eAddLet, eIfElse
    -- Record & Cases
    , Composite(..), cList, cPunnedItems, cTail
    , CompositeTail(..), _OpenCompositeTail, _ClosedCompositeTail
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
import           Lamdu.Sugar.Types.GetVar (GetVar)
import           Lamdu.Sugar.Types.Lhs (LhsNames)
import           Lamdu.Sugar.Types.Parts
import           Lamdu.Sugar.Types.Tag
import           Lamdu.Sugar.Types.TaggedList (TaggedList)
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
    { _aFunc :: k :# Const (GetVar name o)
    , _aMOpArgs :: Maybe (OperatorArgs v name i o k)
    , _aAnnotatedArgs :: [AnnotatedArg v name i o k]
    , _aPunnedArgs :: [PunnedVar name o k]
    } deriving Generic

data PostfixApply v name i o k = PostfixApply
    { _pArg :: k :# Term v name i o
    , _pFunc :: k :# PostfixFunc v name i o
    } deriving Generic

data Lambda v name i o f = Lambda
    { _lamLightweight :: Bool
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
    , _fOptApply :: i (Option FragOpt name i o)
        -- An option to apply (with a hole).
        -- Used for the actions to turn this hole into literal (i.e pressing "5")
    , _fTagSuffixes :: TagSuffixes -- See comment for holeTagSuffixes
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
    | FragArgument (HoleOpt v name i o k) -- Apply fragmented expr with argument
    deriving Generic

data FragOperator v name i o k = FragOperator
    { _oFunc :: k :# Const (GetVar name o)
    , -- Argument on right-hand-side (LTR) of operator.
      -- (usually a hole, but may be completed to other values)
      _oRightArg :: k :# Term v name i o
    , _oAnnotatedArgs :: [Tag name]
    } deriving Generic

data Hole name i o = Hole
    { _holeOptions ::
        i (Query -> i [Option HoleOpt name i o])
        -- Inner `i` serves two purposes:
        -- Name walk requires monadic place to process names.
        -- Hole can prepare results depending on the query and avoid doing work
        -- if the query filters it out.
    , _holeTagSuffixes :: TagSuffixes
        -- When tag suffixes are created by the name pass this is populated,
        -- should be given back in the query.
        -- TODO: More elegant solution?
    } deriving stock Generic

data HoleOpt v name i o k
    = HoleBinder (Binder v name i o k)
    | HoleVarsRecord [name] -- List of fields
    deriving stock Generic

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
    = OpenCompositeTail (k :# Term v name i o)
    | ClosedCompositeTail (ClosedCompositeActions o)
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
        -- ^ A simple function application (aka function call): <function> <argument>
    | BodyPostfixApply (PostfixApply v name i o k)
        -- ^ A function application presented with postfix layout: <argument> .<function>
        -- (used for pattern matching, record field access and nominal type unwrapping)
    | BodyPostfixFunc (PostfixFunc v name i o k)
        -- ^ A function for which postfix application layout apply.
    | BodyLabeledApply (LabeledApply v name i o k)
        -- ^ A syntax sugar for function application with a record argument
    | BodyRecord (Composite v name i o k)
    | BodyIfElse (IfElse v name i o k)
    | BodyToNom (Nominal v name i o k)
        -- ^ Wrap a value with a nominal type constructor
    | BodyNullaryInject (NullaryInject name i o k)
        -- ^ A variant value with no content
    | BodyFragment (Fragment v name i o k)
        -- ^ A fragment holds an unfinished term in the code.
        -- Often generated when the inner term's type mismatches the expected type
        -- at the fragment.
        -- Also used as a placeholder for parentheses during typing.
    | BodyLeaf (Leaf name i o)
    deriving Generic

data Let v name i o k = Let
    { _lValue :: k :# Assignment v name i o -- "let foo = [[bar]] in x"
    , _lNames :: LhsNames name i o v -- let [[foo]] = bar in x
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
    , _fParams :: LhsNames name i o v
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
    [''Assignment, ''BinderBody, ''CompositeTail, ''Else
    , ''FragOpt, ''HoleOpt, ''Leaf, ''PostfixFunc, ''Term
    ] <&> concat

traverse makeHTraversableAndBases
    [ ''AnnotatedArg, ''Assignment, ''AssignPlain, ''Binder, ''BinderBody
    , ''Composite, ''CompositeTail, ''Else, ''ElseIfBody
    , ''Fragment, ''FragOperator, ''FragOpt, ''Function, ''HoleOpt, ''IfElse
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
instance RNodes (HoleOpt v name i o)
instance RNodes (PostfixFunc v name i o)
instance RNodes (Term v name i o)

type Dep v (c :: HyperType -> Constraint) name i o =
    ( c (Assignment v name i o)
    , c (Binder v name i o)
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

instance (Dep v c name i o, c (HoleOpt v name i o)) => Recursively c (HoleOpt v name i o)
instance (Dep v c name i o, c (FragOpt v name i o)) => Recursively c (FragOpt v name i o)
instance (Dep v c name i o, c (Function v name i o)) => Recursively c (Function v name i o)

instance RTraversable (Assignment v name i o)
instance RTraversable (Binder v name i o)
instance RTraversable (Else v name i o)
instance RTraversable (PostfixFunc v name i o)
instance RTraversable (Term v name i o)
