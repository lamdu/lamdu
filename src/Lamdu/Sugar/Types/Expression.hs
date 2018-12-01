{-# LANGUAGE TemplateHaskell, KindSignatures, TypeFamilies #-}
module Lamdu.Sugar.Types.Expression
    ( Body(..)
        , _BodyLam, _BodyLabeledApply, _BodySimpleApply
        , _BodyGetVar, _BodyGetField, _BodyInject, _BodyHole
        , _BodyLiteral, _BodyCase, _BodyRecord, _BodyFragment
        , _BodyFromNom, _BodyToNom, _BodyIfElse
    , Expression
    , AnnotatedArg(..), aaTag, aaExpr
    , LabeledApply(..), aFunc, aSpecialArgs, aAnnotatedArgs, aRelayedArgs
    , Fragment(..), fExpr, fHeal, fOptions
    , Lambda(..), lamFunc, lamMode, lamApplyLimit
    , InjectContent(..), _InjectVal, _InjectNullary
    , Inject(..), iTag, iContent
    -- Binders
    , Let(..)
        , lValue, lName, lUsages
        , lDelete, lBodyScope, lBody, lVarInfo
    , Meta.SpecialArgs(..), Meta._Verbose, Meta._Object, Meta._Infix
    , Meta.DefinitionState(..)
    , BinderParamScopeId(..), bParamScopeId
    , Binder(..), _BinderLet, _BinderExpr
    , Function(..)
        , fChosenScopeProp, fParams, fBody
        , fAddFirstParam, fBodyScopes
    , Assignment
    , AssignPlain(..), apAddFirstParam, apBody
    , AssignmentBody(..), _BodyFunction, _BodyPlain
    -- Holes
    , HoleOption(..), hoVal, hoSugaredBaseExpr, hoResults
    , OptionLiteral
    , Hole(..), holeOptions, holeOptionLiteral, holeMDelete
    , HoleResult(..), holeResultConverted, holeResultPick
    -- If/else
    , ElseIfContent(..), eiScopes, eiContent
    , Else(..), _SimpleElse, _ElseIf
    , IfElse(..), iIf, iThen, iElse
    ) where

import           AST (Node, LeafNode)
import           AST.Ann (Ann)
import           AST.Recursive (Recursive)
import           AST.TH (makeChildren)
import qualified Control.Lens as Lens
import           Control.Monad.ListT (ListT)
import           Data.Functor.Identity (Identity(..))
import           Data.Property (Property)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Data.Anchors (BinderParamScopeId(..), bParamScopeId)
import qualified Lamdu.Data.Meta as Meta
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Eval
import           Lamdu.Sugar.Types.GetVar (GetVar, BinderVarRef, BinderMode)
import           Lamdu.Sugar.Types.Parts
import           Lamdu.Sugar.Types.Simple
import           Lamdu.Sugar.Types.Tag

import           Lamdu.Prelude

type Expression name i o a = Node (Ann a) (Body name i o)

data AnnotatedArg name expr = AnnotatedArg
    { _aaTag :: TagInfo name
    , _aaExpr :: expr
    } deriving (Functor, Foldable, Traversable, Generic)

-- TODO: func + specialArgs into a single sum type so that field order
-- matches gui order, no need for special traversal code
data LabeledApply name i o f = LabeledApply
    { _aFunc :: LeafNode f (BinderVarRef name o)
    , _aSpecialArgs :: Meta.SpecialArgs (Node f (Body name i o))
    , _aAnnotatedArgs :: [AnnotatedArg name (Node f (Body name i o))]
    , _aRelayedArgs :: [LeafNode f (GetVar name o)]
    } deriving Generic

data InjectContent name i o f
    = InjectNullary (LeafNode f (NullaryVal name i o))
    | InjectVal (Node f (Body name i o))
    deriving Generic

data Inject name i o f = Inject
    { _iTag :: Tag name i o
    , _iContent :: InjectContent name i o f
    } deriving Generic

data Lambda name i o f = Lambda
    { _lamMode :: BinderMode
    , _lamApplyLimit :: FuncApplyLimit
    , _lamFunc :: Function name i o f
    } deriving Generic

-- | An expression marked for transformation.
-- Holds an expression to be transformed but acts like a hole.
data Fragment name i o f = Fragment
    { _fExpr :: Node f (Body name i o)
    , _fHeal :: Heal o
    , _fOptions :: i [HoleOption name i o]
    } deriving Generic

data HoleResult name i o = HoleResult
    { _holeResultConverted :: Node (Ann (Payload name i o ())) (Binder name i o)
    , _holeResultPick :: o ()
    } deriving Generic

data HoleOption name i o = HoleOption
    { _hoVal :: Val ()
    , _hoSugaredBaseExpr :: i (Node (Ann (Payload name i o ())) (Binder name i o))
    , -- A group in the hole results based on this option
      _hoResults :: ListT i (HoleResultScore, i (HoleResult name i o))
    } deriving Generic

type OptionLiteral name i o =
    Literal Identity -> i (HoleResultScore, i (HoleResult name i o))

data Hole name i o = Hole
    { _holeOptions :: i [HoleOption name i o]
        -- outer "i" here is used to read index of globals
        -- inner "i" is used to type-check/sugar every val in the option
      -- TODO: Lifter from i to o?
    , -- TODO: this is mostly duplicate of NodeActions setToLiteral functionality..
      _holeOptionLiteral :: OptionLiteral name i o
    , -- Changes the structure around the hole to remove the hole.
      -- For example (f _) becomes (f) or (2 + _) becomes 2
      _holeMDelete :: Maybe (o EntityId)
    } deriving Generic

-- An "elif <cond>: <then>" clause in an IfElse expression and the subtree under it
data ElseIfContent name i o f = ElseIfContent
    { _eiScopes :: ChildScopes
    , _eiContent :: IfElse name i o f
    } deriving Generic

data Else name i o f
    = SimpleElse (Body name i o f)
    | ElseIf (ElseIfContent name i o f)
    deriving Generic

data IfElse name i o f = IfElse
    { _iIf :: Node f (Body name i o)
    , _iThen :: Node f (Body name i o)
    , _iElse :: Node f (Else name i o)
    } deriving Generic

data Body name i o f
    = BodyLam (Lambda name i o f)
    | BodySimpleApply (V.Apply (Node f (Body name i o)))
    | BodyLabeledApply (LabeledApply name i o f)
    | BodyHole (Hole name i o)
    | BodyLiteral (Literal (Property o))
    | BodyRecord (Composite name i o (Node f (Body name i o)))
    | BodyGetField (GetField name i o (Node f (Body name i o)))
    | BodyCase (Case name i o (Node f (Body name i o)))
    | BodyIfElse (IfElse name i o f)
    | BodyInject (Inject name i o f)
    | BodyGetVar (GetVar name o)
    | BodyToNom (Nominal name (Node f (Binder name i o)))
    | BodyFromNom (Nominal name (Node f (Body name i o)))
    | BodyFragment (Fragment name i o f)
    | BodyPlaceHolder -- Used for hole results, shown as "★"
    deriving Generic

data Let name i o f = Let
    { _lValue :: Node f (AssignmentBody name i o) -- "let foo = [[bar]] in x"
    , _lVarInfo :: VarInfo
    , _lUsages :: [EntityId]
    , _lName :: Tag name i o -- let [[foo]] = bar in x
    , _lDelete :: o ()
    , _lBodyScope :: ChildScopes
    , _lBody :: Node f (Binder name i o) -- "let foo = bar in [[x]]"
    } deriving Generic

-- An expression with 0 or more let items,
-- Appear in a:
-- * Function: "\x -> [[THIS]]"
-- * ToNom: "«X [[THIS]]"
-- * Definition or let item value: "x = [[THIS]]"
-- * Let-item/redex: "let x = y in [[THIS]]"
data Binder name i o f
    = BinderLet (Let name i o f)
    | BinderExpr (Body name i o f)
    deriving Generic

data Function name i o f = Function
    { _fChosenScopeProp :: i (Property o (Maybe BinderParamScopeId))
    , _fParams :: BinderParams name i o
    , _fBody :: Node f (Binder name i o)
    , _fAddFirstParam :: AddFirstParam name i o
    , -- The scope inside a lambda
      _fBodyScopes :: BinderBodyScope
    } deriving Generic

data AssignPlain name i o f = AssignPlain
    { _apAddFirstParam :: AddFirstParam name i o
    , _apBody :: Binder name i o f
    } deriving Generic

data AssignmentBody name i o f
    = BodyFunction (Function name i o f)
    | BodyPlain (AssignPlain name i o f)
    deriving Generic

type Assignment name i o a = Node (Ann a) (AssignmentBody name i o)

Lens.makeLenses ''AnnotatedArg
Lens.makeLenses ''AssignPlain
Lens.makeLenses ''ElseIfContent
Lens.makeLenses ''Fragment
Lens.makeLenses ''Function
Lens.makeLenses ''Hole
Lens.makeLenses ''HoleOption
Lens.makeLenses ''HoleResult
Lens.makeLenses ''IfElse
Lens.makeLenses ''Inject
Lens.makeLenses ''LabeledApply
Lens.makeLenses ''Lambda
Lens.makeLenses ''Let
Lens.makePrisms ''AssignmentBody
Lens.makePrisms ''Binder
Lens.makePrisms ''Body
Lens.makePrisms ''Else
Lens.makePrisms ''InjectContent

makeChildren ''AssignmentBody
makeChildren ''AssignPlain
makeChildren ''Body
makeChildren ''Binder
makeChildren ''Else
makeChildren ''ElseIfContent
makeChildren ''Fragment
makeChildren ''Function
makeChildren ''IfElse
makeChildren ''Inject
makeChildren ''InjectContent
makeChildren ''LabeledApply
makeChildren ''Lambda
makeChildren ''Let

-- TODO: The instances below are a bit of boiler-plate,
-- perhaps `Recursive` shouldn't be a separate class from `Children`?

instance Recursive (AssignmentBody name i o)
instance Recursive (AssignPlain name i o)
instance Recursive (Body name i o)
instance Recursive (Binder name i o)
instance Recursive (Else name i o)
instance Recursive (ElseIfContent name i o)
instance Recursive (Fragment name i o)
instance Recursive (Function name i o)
instance Recursive (IfElse name i o)
instance Recursive (Inject name i o)
instance Recursive (InjectContent name i o)
instance Recursive (LabeledApply name i o)
instance Recursive (Lambda name i o)
instance Recursive (Let name i o)
