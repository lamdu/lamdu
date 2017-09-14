{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveTraversable #-}
module Lamdu.Sugar.Types.Expression
    ( WrapAction(..), _WrapperAlready, _WrappedAlready, _WrapNotAllowed, _WrapAction
    , SetToHole(..), _SetToHole, _SetWrapperToHole, _AlreadyAHole
    , ExtractToDestination(..)
    , Actions(..)
        , wrap, setToHole, extract, mReplaceParent
    , Body(..)
        , _BodyLam, _BodyLabeledApply, _BodySimpleApply
        , _BodyGetVar, _BodyGetField, _BodyInject, _BodyHole
        , _BodyLiteral, _BodyCase, _BodyRecord
        , _BodyFromNom, _BodyToNom, _BodyGuard
    , Payload(..), plEntityId, plAnnotation, plActions, plData
    , Expression(..), rBody, rPayload
    -- record:
    , CompositeItem(..), ciDelete, ciTag, ciExpr
    , CompositeTail(..), _CompositeExtending, _ClosedComposite
    , CompositeAddItemResult(..), cairNewTag, cairNewVal, cairItem
    , Composite(..), cItems, cAddItem, cTail
    -- case
    , CaseArg(..), caVal, caToLambdaCase
    , CaseKind(..), _LambdaCase, _CaseWithArg
    , Case(..), cKind, cBody
    , GuardElseIf(..), geScopes, geEntityId, geCond, geThen, geDelete, geCondAddLet
    , Guard(..), gIf, gThen, gElseIfs, gElse, gDeleteIf
    , Nominal(..), nTId, nVal
    --
    , GetField(..), gfRecord, gfTag
    , Inject(..), iTag, iMVal
    , SpecialArgs(..), _NoSpecialArgs, _ObjectArg, _InfixArgs
    , AnnotatedArg(..), aaTag, aaExpr, aaName
    , RelayedArg(..), raValue, raId, raActions
    , LabeledApply(..), aFunc, aSpecialArgs, aAnnotatedArgs, aRelayedArgs
    , TId(..), tidName, tidTId
    , Lambda(..), lamBinder, lamMode
    , V.Apply(..), V.applyFunc, V.applyArg
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction, Property)
import           Data.UUID.Types (UUID)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Binder
import           Lamdu.Sugar.Types.GetVar (GetVar, BinderVar, Param)
import           Lamdu.Sugar.Types.Hole (Hole, Literal)

import           Lamdu.Prelude

type T = Transaction

data WrapAction m
    = WrapperAlready (UUID, EntityId) -- I'm an apply-of-hole, (UUID,EntityId of hole), no need to wrap
    | WrappedAlready (UUID, EntityId) -- I'm an arg of apply-of-hole (UUID,EntityId of hole), no need to wrap
    | WrapNotAllowed -- I'm a hole
    | WrapAction (T m (UUID, EntityId)) -- Wrap me!

data SetToHole m
    = SetToHole (T m (UUID, EntityId))
    | SetWrapperToHole (T m (UUID, EntityId))
    | AlreadyAHole

data ExtractToDestination
    = ExtractToLet EntityId
    | ExtractToDef EntityId

data Actions m = Actions
    { _wrap :: WrapAction m
    , _setToHole :: SetToHole m
    , _extract :: T m ExtractToDestination
    , _mReplaceParent :: Maybe (T m EntityId)
    }

data Payload m a = Payload
    { _plAnnotation :: Annotation
    , _plActions :: Actions m
    , _plEntityId :: EntityId
    , _plData :: a
    } deriving (Functor, Foldable, Traversable)

data Expression name m a = Expression
    { _rBody :: Body name m (Expression name m a)
    , _rPayload :: Payload m a
    } deriving (Functor, Foldable, Traversable)

data TId name = TId
    { _tidName :: name
    , _tidTId :: T.NominalId
    }

{- Composites start -}
data CompositeItem name m expr = CompositeItem
    { _ciDelete :: T m EntityId
    , _ciTag :: Tag name m
    , _ciExpr :: expr
    } deriving (Functor, Foldable, Traversable)

data CompositeTail m expr
    = CompositeExtending expr
    | ClosedComposite (T m EntityId) -- delete action
    deriving (Functor, Foldable, Traversable)

data CompositeAddItemResult = CompositeAddItemResult
    { _cairNewTag :: TagInfo
    , _cairNewVal :: EntityId
    , _cairItem :: EntityId
    }

data Composite name m expr = Composite
    { _cItems :: [CompositeItem name m expr]
    , _cTail :: CompositeTail m expr
    , _cAddItem :: T m CompositeAddItemResult
    } deriving (Functor, Foldable, Traversable)

data CaseArg m expr = CaseArg
    { _caVal :: expr
    , _caToLambdaCase :: T m EntityId
    } deriving (Functor, Foldable, Traversable)

data CaseKind m expr
    = LambdaCase
    | CaseWithArg (CaseArg m expr)
    deriving (Functor, Foldable, Traversable)

data Case name m expr = Case
    { _cKind :: CaseKind m expr
    , _cBody :: Composite name m expr
    } deriving (Functor, Foldable, Traversable)
{- Composites end -}

data GuardElseIf m expr = GuardElseIf
    { _geScopes :: ChildScopeMapping
    , _geEntityId :: EntityId
    , _geCond :: expr
    , _geThen :: expr
    , _geDelete :: T m EntityId
    , _geCondAddLet :: T m EntityId
    } deriving (Functor, Foldable, Traversable)

data Guard m expr = Guard
    { -- "if" is different than "else if" in that it doesn't have a scope that it run in
      _gIf :: expr
    , _gThen :: expr
    , _gElseIfs :: [GuardElseIf m expr]
    , _gElse :: expr
    , _gDeleteIf :: T m EntityId
    } deriving (Functor, Foldable, Traversable)

data GetField name m expr = GetField
    { _gfRecord :: expr
    , _gfTag :: Tag name m
    } deriving (Functor, Foldable, Traversable)

data Inject name m expr = Inject
    { _iTag :: Tag name m
    , _iMVal :: Maybe expr
    } deriving (Functor, Foldable, Traversable)

data SpecialArgs expr
    = NoSpecialArgs
    | ObjectArg expr
    | InfixArgs expr expr
    deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

data AnnotatedArg name expr = AnnotatedArg
    { _aaTag :: TagInfo
    , _aaName :: name
    , _aaExpr :: expr
    } deriving (Functor, Foldable, Traversable)

data RelayedArg name m = RelayedArg
    { _raValue :: Param name m
    , _raId :: EntityId
    , _raActions :: Actions m
    }

data LabeledApply name m expr = LabeledApply
    { _aFunc :: BinderVar name m
    , _aSpecialArgs :: SpecialArgs expr
    , _aAnnotatedArgs :: [AnnotatedArg name expr]
    , _aRelayedArgs :: [RelayedArg name m]
    } deriving (Functor, Foldable, Traversable)

data Nominal name expr = Nominal
    { _nTId :: TId name
    , _nVal :: expr
    } deriving (Functor, Foldable, Traversable)

data Lambda name m expr = Lambda
    { _lamMode :: BinderMode
    , _lamBinder :: Binder name m expr
    } deriving (Functor, Foldable, Traversable)

data Body name m expr
    = BodyLam (Lambda name m expr)
    | BodySimpleApply (V.Apply expr)
    | BodyLabeledApply (LabeledApply name m expr)
    | BodyHole (Hole m (Expression name m ()) expr)
    | BodyLiteral (Literal (Property m))
    | BodyRecord (Composite name m expr)
    | BodyGetField (GetField name m expr)
    | BodyCase (Case name m expr)
    | BodyGuard (Guard m expr)
    | BodyInject (Inject name m expr)
    | BodyGetVar (GetVar name m)
    | BodyToNom (Nominal name (BinderBody name m expr))
    | BodyFromNom (Nominal name expr)
    | BodyInjectedExpression -- Used for hole results
    deriving (Functor, Foldable, Traversable)

instance Show (Body name m expr) where
    show (BodyLam _) = "TODO show lam"
    show BodyHole {} = "Hole"
    show BodyLiteral {} = "Literal"
    show BodySimpleApply {} = "SimpleApply:TODO"
    show BodyLabeledApply {} = "LabelledApply:TODO"
    show BodyRecord {} = "Record:TODO"
    show BodyGetField {} = "GetField:TODO"
    show BodyCase {} = "Case:TODO"
    show BodyGuard {} = "If:TODO"
    show BodyInject {} = "Inject:TODO"
    show BodyGetVar {} = "GetVar:TODO"
    show BodyFromNom {} = "FromNom:TODO"
    show BodyToNom {} = "ToNom:TODO"
    show BodyInjectedExpression {} = "InjectedExpression"

Lens.makeLenses ''Actions
Lens.makeLenses ''AnnotatedArg
Lens.makeLenses ''Body
Lens.makeLenses ''Case
Lens.makeLenses ''CaseArg
Lens.makeLenses ''Composite
Lens.makeLenses ''CompositeAddItemResult
Lens.makeLenses ''CompositeItem
Lens.makeLenses ''Expression
Lens.makeLenses ''GetField
Lens.makeLenses ''Guard
Lens.makeLenses ''GuardElseIf
Lens.makeLenses ''Inject
Lens.makeLenses ''LabeledApply
Lens.makeLenses ''Lambda
Lens.makeLenses ''Nominal
Lens.makeLenses ''Payload
Lens.makeLenses ''RelayedArg
Lens.makeLenses ''TId
Lens.makePrisms ''Body
Lens.makePrisms ''CaseKind
Lens.makePrisms ''CompositeTail
Lens.makePrisms ''Literal
Lens.makePrisms ''SetToHole
Lens.makePrisms ''SpecialArgs
Lens.makePrisms ''WrapAction
