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
    , Lambda(..), lamFunc, lamMode
    -- Binders
    , Let(..)
        , lEntityId, lValue, lName, lUsages
        , lActions, lBodyScope, lBody
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
import qualified Lamdu.Calc.Val as V
import           Lamdu.Data.Anchors (BinderParamScopeId(..), bParamScopeId)
import qualified Lamdu.Data.Meta as Meta
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Eval
import           Lamdu.Sugar.Types.GetVar (GetVar, BinderMode)
import           Lamdu.Sugar.Types.Hole (Hole, HoleOption)
import           Lamdu.Sugar.Types.Parts
import           Lamdu.Sugar.Types.Simple
import           Lamdu.Sugar.Types.Tag

import           Lamdu.Prelude

data Expression name i o a = Expression
    { _annotation :: Payload name i o a
    , _body :: Body name i o a
    } deriving (Functor, Foldable, Traversable, Generic)

data AnnotatedArg name expr = AnnotatedArg
    { _aaTag :: TagInfo name
    , _aaExpr :: expr
    } deriving (Functor, Foldable, Traversable, Generic)

data LabeledApply name i o expr = LabeledApply
    { _aFunc :: LabeledApplyFunc name i o ()
    , _aSpecialArgs :: Meta.SpecialArgs expr
    , _aAnnotatedArgs :: [AnnotatedArg name expr]
    , _aRelayedArgs :: [RelayedArg name i o]
    } deriving (Functor, Foldable, Traversable, Generic)

data Lambda name i o a = Lambda
    { _lamMode :: BinderMode
    , _lamFunc :: Function name i o a
    } deriving (Functor, Foldable, Traversable, Generic)

-- | An expression marked for transformation.
-- Holds an expression to be transformed but acts like a hole.
data Fragment name i o expr = Fragment
    { _fExpr :: expr
    , _fHeal :: Heal o
    , _fOptions :: i [HoleOption i o (Expression name i o ())]
    } deriving (Functor, Foldable, Traversable, Generic)

instance Show expr => Show (Fragment name i o expr) where
    show (Fragment expr _ _) = "(Fragment " ++ show expr ++ ")"

data Body name i o a
    = BodyLam (Lambda name i o a)
    | BodySimpleApply (V.Apply (Expression name i o a))
    | BodyLabeledApply (LabeledApply name i o (Expression name i o a))
    | BodyHole (Hole i o (Expression name i o ()))
    | BodyLiteral (Literal (Property o))
    | BodyRecord (Composite name i o (Expression name i o a))
    | BodyGetField (GetField name i o (Expression name i o a))
    | BodyCase (Case name i o (Expression name i o a))
    | BodyIfElse (IfElse name i o (Expression name i o a))
    | BodyInject (Inject name i o (Expression name i o a))
    | BodyGetVar (GetVar name o)
    | BodyToNom (Nominal name (BinderBody name i o a))
    | BodyFromNom (Nominal name (Expression name i o a))
    | BodyFragment (Fragment name i o (Expression name i o a))
    | BodyPlaceHolder -- Used for hole results, shown as "★"
    deriving (Functor, Foldable, Traversable, Generic)

instance (Show name, Show expr) => Show (LabeledApply name i o expr) where
    show (LabeledApply func specialArgs _annArgs _relayedArgs) =
        unwords ["LabeledApply of", show func, "with", show specialArgs, "..."]

data Let name i o a = Let
    { _lValue :: Assignment name i o a -- "let foo = [[bar]] in x"
    , _lEntityId :: EntityId
    , _lUsages :: [EntityId]
    , _lName :: Tag name i o -- let [[foo]] = bar in x
    , _lActions :: LetActions name i o
    , _lBodyScope :: ChildScopes
    , _lBody :: BinderBody name i o a -- "let foo = bar in [[x]]"
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
data BinderBody name i o a = BinderBody
    { _bbAddOuterLet :: o EntityId
    , _bbContent :: BinderContent name i o a
    } deriving (Functor, Foldable, Traversable, Generic)

data Function name i o a = Function
    { _fChosenScopeProp :: i (Property o (Maybe BinderParamScopeId))
    , _fParams :: BinderParams name i o
    , _fBody :: BinderBody name i o a
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
    , _apBody :: BinderBody name i o a
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
Lens.makeLenses ''BinderBody
Lens.makeLenses ''Expression
Lens.makeLenses ''Fragment
Lens.makeLenses ''Function
Lens.makeLenses ''LabeledApply
Lens.makeLenses ''Lambda
Lens.makeLenses ''Let
Lens.makePrisms ''AssignmentBody
Lens.makePrisms ''BinderContent
Lens.makePrisms ''Body
