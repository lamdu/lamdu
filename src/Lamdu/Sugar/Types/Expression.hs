{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Expression
    ( Body(..)
        , _BodyLam, _BodyLabeledApply, _BodySimpleApply
        , _BodyGetVar, _BodyGetField, _BodyInject, _BodyHole
        , _BodyLiteral, _BodyCase, _BodyRecord, _BodyFragment
        , _BodyFromNom, _BodyToNom, _BodyIfElse
    , Expression(..), body, annotation
    , AnnotatedArg(..), aaTag, aaExpr
    , LabeledApply(..), aFunc, aSpecialArgs, aAnnotatedArgs, aRelayedArgs
    , Fragment(..), fExpr, fHeal, fOptions
    , Lambda(..), lamFunc, lamMode, lamApplyLimit
    , InjectVal(..), _InjectVal, _InjectNullary
    , Inject(..), iTag, iMVal
    -- Binders
    , Let(..)
        , lEntityId, lValue, lName, lUsages
        , lActions, lBodyScope, lBody, lVarInfo
    , Meta.SpecialArgs(..), Meta._Verbose, Meta._Object, Meta._Infix
    , Meta.DefinitionState(..)
    , BinderParamScopeId(..), bParamScopeId
    , Binder(..), bAddOuterLet, bContent
    , BinderContent(..), _BinderLet, _BinderExpr
    , Function(..)
        , fChosenScopeProp, fParams, fBody
        , fAddFirstParam, fBodyScopes
    , Assignment(..), aBody, aNodeActions
    , AssignFunction(..), afFunction, afLamId
    , AssignPlain(..), apAddFirstParam, apBody
    , AssignmentBody(..), _BodyFunction, _BodyPlain
    -- Holes
    , HoleOption(..), hoVal, hoSugaredBaseExpr, hoResults
    , OptionLiteral
    , Hole(..), holeOptions, holeOptionLiteral, holeMDelete
    , HoleResult(..), holeResultConverted, holeResultPick
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.ListT (ListT)
import           Data.Functor.Identity (Identity(..))
import           Data.Property (Property)
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Data.Anchors (BinderParamScopeId(..), bParamScopeId)
import qualified Lamdu.Data.Meta as Meta
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Eval
import           Lamdu.Sugar.Types.GetVar (GetVar, BinderMode)
import           Lamdu.Sugar.Types.Parts
import           Lamdu.Sugar.Types.Simple
import           Lamdu.Sugar.Types.Tag

import           Lamdu.Prelude

data Expression name i o a = Expression
    { _annotation :: a
    , _body :: Body name i o a
    } deriving (Functor, Foldable, Traversable, Generic)

data AnnotatedArg name expr = AnnotatedArg
    { _aaTag :: TagInfo name
    , _aaExpr :: expr
    } deriving (Functor, Foldable, Traversable, Generic)

-- TODO: func + specialArgs into a single sum type so that field order
-- matches gui order, no need for special traversal code
data LabeledApply name i o a = LabeledApply
    { _aFunc :: LabeledApplyFunc name o a
    , _aSpecialArgs :: Meta.SpecialArgs (Expression name i o a)
    , _aAnnotatedArgs :: [AnnotatedArg name (Expression name i o a)]
    , _aRelayedArgs :: [RelayedArg name o a]
    } deriving (Functor, Foldable, Traversable, Generic)

data InjectVal name i o a
    = InjectNullary (NullaryVal name i o a)
    | InjectVal (Expression name i o a)
    deriving (Functor, Foldable, Traversable, Generic)

data Inject name i o a = Inject
    { _iTag :: Tag name i o
    , _iMVal :: InjectVal name i o a
    } deriving (Functor, Foldable, Traversable, Generic)

data Lambda name i o a = Lambda
    { _lamMode :: BinderMode
    , _lamApplyLimit :: FuncApplyLimit
    , _lamFunc :: Function name i o a
    } deriving (Functor, Foldable, Traversable, Generic)

-- | An expression marked for transformation.
-- Holds an expression to be transformed but acts like a hole.
data Fragment name i o a = Fragment
    { _fExpr :: Expression name i o a
    , _fHeal :: Heal o
    , _fOptions :: i [HoleOption name i o]
    } deriving (Functor, Foldable, Traversable, Generic)

data HoleResult name i o = HoleResult
    { _holeResultConverted :: BinderContent name i o (Payload name i o ())
    , _holeResultPick :: o ()
    } deriving Generic

data HoleOption name i o = HoleOption
    { _hoVal :: Val ()
    , _hoSugaredBaseExpr :: i (BinderContent name i o (Payload name i o ()))
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
    , _holeOptionLiteral :: OptionLiteral name i o
    , -- Changes the structure around the hole to remove the hole.
      -- For example (f _) becomes (f) or (2 + _) becomes 2
      _holeMDelete :: Maybe (o EntityId)
    } deriving Generic

data Body name i o a
    = BodyLam (Lambda name i o a)
    | BodySimpleApply (V.Apply (Expression name i o a))
    | BodyLabeledApply (LabeledApply name i o a)
    | BodyHole (Hole name i o)
    | BodyLiteral (Literal (Property o))
    | BodyRecord (Composite name i o (Expression name i o a))
    | BodyGetField (GetField name i o (Expression name i o a))
    | BodyCase (Case name i o (Expression name i o a))
    | BodyIfElse (IfElse name i o (Expression name i o a))
    | BodyInject (Inject name i o a)
    | BodyGetVar (GetVar name o)
    | BodyToNom (Nominal name (Binder name i o a))
    | BodyFromNom (Nominal name (Expression name i o a))
    | BodyFragment (Fragment name i o a)
    | BodyPlaceHolder -- Used for hole results, shown as "★"
    deriving (Functor, Foldable, Traversable, Generic)

data Let name i o a = Let
    { _lValue :: Assignment name i o a -- "let foo = [[bar]] in x"
    , _lVarInfo :: VarInfo
    , _lEntityId :: EntityId
    , _lUsages :: [EntityId]
    , _lName :: Tag name i o -- let [[foo]] = bar in x
    , _lActions :: LetActions name i o
    , _lBodyScope :: ChildScopes
    , _lBody :: Binder name i o a -- "let foo = bar in [[x]]"
    } deriving (Functor, Foldable, Traversable, Generic)

data BinderContent name i o a
    = BinderLet (Let name i o a)
    | BinderExpr (Expression name i o a)
    deriving (Functor, Foldable, Traversable, Generic)

-- An expression with 0 or more let items,
-- Appear in a:
-- * Function: "\x -> [[THIS]]"
-- * ToNom: "«X [[THIS]]"
-- * Definition or let item value: "x = [[THIS]]"
-- * Let-item/redex: "let x = y in [[THIS]]"
data Binder name i o a = Binder
    { _bAddOuterLet :: o EntityId
    , _bContent :: BinderContent name i o a
    } deriving (Functor, Foldable, Traversable, Generic)

data Function name i o a = Function
    { _fChosenScopeProp :: i (Property o (Maybe BinderParamScopeId))
    , _fParams :: BinderParams name i o
    , _fBody :: Binder name i o a
    , _fAddFirstParam :: AddFirstParam name i o
    , -- The scope inside a lambda
      _fBodyScopes :: BinderBodyScope
    } deriving (Functor, Foldable, Traversable, Generic)

data AssignFunction name i o a = AssignFunction
    { _afLamId :: EntityId
    , _afFunction :: Function name i o a
    } deriving (Functor, Foldable, Traversable, Generic)

data AssignPlain name i o a = AssignPlain
    { _apAddFirstParam :: AddFirstParam name i o
    , _apBody :: Binder name i o a
    } deriving (Functor, Foldable, Traversable, Generic)

data AssignmentBody name i o a
    = BodyFunction (AssignFunction name i o a)
    | BodyPlain (AssignPlain name i o a)
    deriving (Functor, Foldable, Traversable, Generic)

data Assignment name i o a = Assignment
    { _aBody :: AssignmentBody name i o a
    , _aNodeActions :: NodeActions name i o
    } deriving (Functor, Foldable, Traversable, Generic)

Lens.makeLenses ''AnnotatedArg
Lens.makeLenses ''AssignFunction
Lens.makeLenses ''Assignment
Lens.makeLenses ''AssignPlain
Lens.makeLenses ''Binder
Lens.makeLenses ''Expression
Lens.makeLenses ''Fragment
Lens.makeLenses ''Function
Lens.makeLenses ''Hole
Lens.makeLenses ''HoleOption
Lens.makeLenses ''HoleResult
Lens.makeLenses ''Inject
Lens.makePrisms ''InjectVal
Lens.makeLenses ''LabeledApply
Lens.makeLenses ''Lambda
Lens.makeLenses ''Let
Lens.makePrisms ''AssignmentBody
Lens.makePrisms ''BinderContent
Lens.makePrisms ''Body
