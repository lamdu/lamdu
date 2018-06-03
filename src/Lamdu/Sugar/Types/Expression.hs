{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Expression
    ( Body(..)
        , _BodyLam, _BodyLabeledApply, _BodySimpleApply
        , _BodyGetVar, _BodyGetField, _BodyInject, _BodyHole
        , _BodyLiteral, _BodyCase, _BodyRecord, _BodyFragment
        , _BodyFromNom, _BodyToNom, _BodyIfElse
    , Payload(..), plEntityId, plAnnotation, plActions, plData
    , Expression(..), rBody, rPayload
    -- record:
    , CompositeItem(..), ciDelete, ciTag, ciExpr
    , ClosedCompositeActions(..), closedCompositeOpen
    , OpenCompositeActions(..), openCompositeClose
    , CompositeTail(..), _OpenComposite, _ClosedComposite
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
    , NullaryVal(..), nullaryPayload, nullaryClosedCompositeActions, nullaryAddItem
    , InjectVal(..), _InjectVal, _InjectNullary
    , Inject(..), iTag, iMVal
    , SpecialArgs(..)
    , AnnotatedArg(..), aaTag, aaExpr
    , RelayedArg(..), raValue, raId, raActions
    , LabeledApplyFunc(..), afVar, afPayload
    , LabeledApply(..), aFunc, aSpecialArgs, aAnnotatedArgs, aRelayedArgs
    , Fragment(..), fExpr, fHeal, fOptions
    , Heal(..), _HealAction, _TypeMismatch
    , Lambda(..), lamBinder, lamMode
    , V.Apply(..), V.applyFunc, V.applyArg
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Binder
import           Lamdu.Sugar.Types.Eval
import           Lamdu.Sugar.Types.GetVar (GetVar, BinderVarRef, BinderMode)
import           Lamdu.Sugar.Types.Hole (Hole, HoleOption, Literal)
import           Lamdu.Sugar.Types.Tag
import           Lamdu.Sugar.Types.Type

import           Lamdu.Prelude

data Payload name i o a = Payload
    { _plAnnotation :: Annotation name
    , _plActions :: NodeActions name i o
    , _plEntityId :: EntityId
    , _plData :: a
    } deriving (Functor, Foldable, Traversable, Generic)
instance Show a => Show (Payload name i o a) where
    show (Payload _ann _actions _entityId data_) = show data_

data Expression name i o a = Expression
    { _rBody :: Body name i o (Expression name i o a)
    , _rPayload :: Payload name i o a
    } deriving (Functor, Foldable, Traversable, Generic)
instance (Show name, Show a) => Show (Expression name i o a) where
    show (Expression body pl) = show body ++ "{" ++ show pl ++ "}"

{- Composites start -}
data CompositeItem name i o expr = CompositeItem
    { _ciDelete :: o EntityId
    , _ciTag :: Tag name i o
    , _ciExpr :: expr
    } deriving (Functor, Foldable, Traversable, Generic)

newtype ClosedCompositeActions o = ClosedCompositeActions
    { _closedCompositeOpen :: o EntityId
    } deriving Generic

newtype OpenCompositeActions o = OpenCompositeActions
    { _openCompositeClose :: o EntityId
    } deriving Generic

data CompositeTail o expr
    = OpenComposite (OpenCompositeActions o) expr
    | ClosedComposite (ClosedCompositeActions o)
    deriving (Functor, Foldable, Traversable, Generic)

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

-- | The empty record (for manipulations in GUI)
data NullaryVal name i o a = NullaryVal
    { _nullaryPayload :: Payload name i o a
    , _nullaryClosedCompositeActions :: ClosedCompositeActions o
    , _nullaryAddItem :: TagSelection name i o EntityId
    } deriving (Functor, Foldable, Traversable, Generic)

data InjectVal name i o expr
    = InjectNullary (NullaryVal name i o ())
    | InjectVal expr
    deriving (Functor, Foldable, Traversable, Generic)

data Inject name i o expr = Inject
    { _iTag :: Tag name i o
    , _iMVal :: InjectVal name i o expr
    } deriving (Functor, Foldable, Traversable, Generic)

data AnnotatedArg name expr = AnnotatedArg
    { _aaTag :: TagInfo name
    , _aaExpr :: expr
    } deriving (Functor, Foldable, Traversable, Generic)

data RelayedArg name i o = RelayedArg
    { _raValue :: GetVar name o
    , _raId :: EntityId
    , _raActions :: NodeActions name i o
    } deriving Generic

data LabeledApplyFunc name i o a = LabeledApplyFunc
    { _afVar :: BinderVarRef name o
    , _afPayload :: Payload name i o a
    } deriving (Functor, Foldable, Traversable, Generic)

data LabeledApply name i o expr = LabeledApply
    { _aFunc :: LabeledApplyFunc name i o ()
    , _aSpecialArgs :: SpecialArgs expr
    , _aAnnotatedArgs :: [AnnotatedArg name expr]
    , _aRelayedArgs :: [RelayedArg name i o]
    } deriving (Functor, Foldable, Traversable, Generic)

data Nominal name expr = Nominal
    { _nTId :: TId name
    , _nVal :: expr
    } deriving (Functor, Foldable, Traversable, Generic)

data Lambda name i o expr = Lambda
    { _lamMode :: BinderMode
    , _lamBinder :: Binder name i o expr
    } deriving (Functor, Foldable, Traversable, Generic)

data Heal o
    = HealAction (o EntityId)
    | TypeMismatch
    deriving Generic

-- | An expression marked for transformation.
-- Holds an expression to be transformed but acts like a hole.
data Fragment name i o expr = Fragment
    { _fExpr :: expr
    , _fHeal :: Heal o
    , _fOptions :: i [HoleOption i o (Expression name i o ())]
    } deriving (Functor, Foldable, Traversable, Generic)

instance Show expr => Show (Fragment name i o expr) where
    show (Fragment expr _ _) = "(Fragment " ++ show expr ++ ")"

data Body name i o expr
    = BodyLam (Lambda name i o expr)
    | BodySimpleApply (V.Apply expr)
    | BodyLabeledApply (LabeledApply name i o expr)
    | BodyHole (Hole i o (Expression name i o ()))
    | BodyLiteral (Literal (Property o))
    | BodyRecord (Composite name i o expr)
    | BodyGetField (GetField name i o expr)
    | BodyCase (Case name i o expr)
    | BodyIfElse (IfElse name i o expr)
    | BodyInject (Inject name i o expr)
    | BodyGetVar (GetVar name o)
    | BodyToNom (Nominal name (BinderBody name i o expr))
    | BodyFromNom (Nominal name expr)
    | BodyFragment (Fragment name i o expr)
    | BodyPlaceHolder -- Used for hole results, shown as "★"
    deriving (Functor, Foldable, Traversable, Generic)

instance (Show name, Show expr) => Show (LabeledApplyFunc name i o expr) where
    show (LabeledApplyFunc func pl) = concat [show func, "{", show pl, "}"]

instance (Show name, Show expr) => Show (LabeledApply name i o expr) where
    show (LabeledApply func specialArgs _annArgs _relayedArgs) =
        unwords ["LabeledApply of", show func, "with", show specialArgs, "..."]

instance (Show name, Show expr) => Show (Lambda name i o expr) where
    show (Lambda mode binder) = concat ["\\(", show mode, ") ", show binder]

instance (Show name, Show expr) => Show (Body name i o expr) where
    show (BodyLam lam) = show lam
    show BodyHole {} = "Hole"
    show BodyLiteral {} = "Literal"
    show BodySimpleApply {} = "SimpleApply:TODO"
    show (BodyLabeledApply x) = show x
    show BodyRecord {} = "Record:TODO"
    show BodyGetField {} = "GetField:TODO"
    show BodyCase {} = "Case:TODO"
    show BodyIfElse {} = "If:TODO"
    show BodyInject {} = "Inject:TODO"
    show BodyGetVar {} = "GetVar:TODO"
    show BodyFromNom {} = "FromNom:TODO"
    show BodyToNom {} = "ToNom:TODO"
    show BodyPlaceHolder {} = "InjectedExpression"
    show (BodyFragment x) = show x

Lens.makeLenses ''AnnotatedArg
Lens.makeLenses ''Body
Lens.makeLenses ''Case
Lens.makeLenses ''CaseArg
Lens.makeLenses ''ClosedCompositeActions
Lens.makeLenses ''Composite
Lens.makeLenses ''CompositeItem
Lens.makeLenses ''ElseIfContent
Lens.makeLenses ''Expression
Lens.makeLenses ''Fragment
Lens.makeLenses ''GetField
Lens.makeLenses ''IfElse
Lens.makeLenses ''IfThen
Lens.makeLenses ''Inject
Lens.makeLenses ''LabeledApply
Lens.makeLenses ''LabeledApplyFunc
Lens.makeLenses ''Lambda
Lens.makeLenses ''Nominal
Lens.makeLenses ''NullaryVal
Lens.makeLenses ''OpenCompositeActions
Lens.makeLenses ''Payload
Lens.makeLenses ''RelayedArg
Lens.makePrisms ''Body
Lens.makePrisms ''CaseKind
Lens.makePrisms ''CompositeTail
Lens.makePrisms ''Else
Lens.makePrisms ''Heal
Lens.makePrisms ''InjectVal
Lens.makePrisms ''Literal
