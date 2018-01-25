{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, DeriveTraversable #-}
module Lamdu.Sugar.Types.Expression
    ( DetachAction(..), _FragmentAlready, _FragmentExprAlready, _DetachAction
    , Actions(..), detach, mSetToHole, extract, mReplaceParent
    , Body(..)
        , _BodyLam, _BodyLabeledApply, _BodySimpleApply
        , _BodyGetVar, _BodyGetField, _BodyInject, _BodyHole
        , _BodyLiteral, _BodyCase, _BodyRecord, _BodyFragment
        , _BodyFromNom, _BodyToNom, _BodyGuard
    , Payload(..), plEntityId, plAnnotation, plActions, plData
    , Expression(..), rBody, rPayload
    -- record:
    , CompositeItem(..), ciDelete, ciTag, ciExpr
    , ClosedCompositeActions(..), closedCompositeOpen
    , OpenCompositeActions(..), openCompositeClose
    , CompositeTail(..), _OpenComposite, _ClosedComposite
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
    , SpecialArgs(..)
    , AnnotatedArg(..), aaTag, aaExpr, aaName
    , RelayedArg(..), raValue, raId, raActions
    , LabeledApply(..), aFunc, aSpecialArgs, aAnnotatedArgs, aRelayedArgs
    , Fragment(..), fExpr, fAttach, fOptions
    , Attach(..), _AttachAction, _AttachTypeMismatch
    , TId(..), tidName, tidTId
    , Lambda(..), lamBinder, lamMode
    , V.Apply(..), V.applyFunc, V.applyArg
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Property (Property)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Binder
import           Lamdu.Sugar.Types.GetVar (GetVar, BinderVarRef, ParamRef)
import           Lamdu.Sugar.Types.Hole (Hole, HoleOption, Literal)

import           Lamdu.Prelude

data DetachAction m
    = FragmentAlready EntityId -- I'm an apply-of-hole, no need to detach
    | FragmentExprAlready EntityId -- I'm an arg of apply-of-hole, no need to detach
    | DetachAction (m EntityId) -- Detach me

data Actions m = Actions
    { _detach :: DetachAction m
    , _mSetToHole :: Maybe (m EntityId) -- (Not available for holes)
    , _extract :: m ExtractDestination
    , _mReplaceParent :: Maybe (m EntityId)
    }

data Payload m a = Payload
    { _plAnnotation :: Annotation
    , _plActions :: Actions m
    , _plEntityId :: EntityId
    , _plData :: a
    } deriving (Functor, Foldable, Traversable)
instance Show a => Show (Payload m a) where
    show (Payload _ann _actions _entityId data_) = show data_

data Expression name m a = Expression
    { _rBody :: Body name m (Expression name m a)
    , _rPayload :: Payload m a
    } deriving (Functor, Foldable, Traversable)
instance (Show name, Show a) => Show (Expression name m a) where
    show (Expression body pl) = show body ++ "{" ++ show pl ++ "}"

data TId name = TId
    { _tidName :: name
    , _tidTId :: T.NominalId
    }

{- Composites start -}
data CompositeItem name m expr = CompositeItem
    { _ciDelete :: m EntityId
    , _ciTag :: Tag name m
    , _ciExpr :: expr
    } deriving (Functor, Foldable, Traversable)

newtype ClosedCompositeActions m = ClosedCompositeActions
    { _closedCompositeOpen :: m EntityId
    }

newtype OpenCompositeActions m = OpenCompositeActions
    { _openCompositeClose :: m EntityId
    }

data CompositeTail m expr
    = OpenComposite (OpenCompositeActions m) expr
    | ClosedComposite (ClosedCompositeActions m)
    deriving (Functor, Foldable, Traversable)

data CompositeAddItemResult = CompositeAddItemResult
    { _cairNewTag :: TagInfo
    , _cairNewVal :: EntityId
    , _cairItem :: EntityId
    }

data Composite name m expr = Composite
    { _cItems :: [CompositeItem name m expr]
    , _cTail :: CompositeTail m expr
    , _cAddItem :: m CompositeAddItemResult
    } deriving (Functor, Foldable, Traversable)

data CaseArg m expr = CaseArg
    { _caVal :: expr
    , _caToLambdaCase :: m EntityId
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
    , _geDelete :: m EntityId
    , _geCondAddLet :: m EntityId
    } deriving (Functor, Foldable, Traversable)

data Guard m expr = Guard
    { -- "if" is different than "else if" in that it doesn't have a scope that it run in
      _gIf :: expr
    , _gThen :: expr
    , _gElseIfs :: [GuardElseIf m expr]
    , _gElse :: expr
    , _gDeleteIf :: m EntityId
    } deriving (Functor, Foldable, Traversable)

data GetField name m expr = GetField
    { _gfRecord :: expr
    , _gfTag :: Tag name m
    } deriving (Functor, Foldable, Traversable)

data Inject name m expr = Inject
    { _iTag :: Tag name m
    , _iMVal :: Maybe expr
    } deriving (Functor, Foldable, Traversable)

data AnnotatedArg name expr = AnnotatedArg
    { _aaTag :: TagInfo
    , _aaName :: name
    , _aaExpr :: expr
    } deriving (Functor, Foldable, Traversable)

data RelayedArg name m = RelayedArg
    { _raValue :: ParamRef name m
    , _raId :: EntityId
    , _raActions :: Actions m
    }

data LabeledApply name m expr = LabeledApply
    { _aFunc :: BinderVarRef name m
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

data Attach m
    = AttachAction (m EntityId)
    | AttachTypeMismatch

-- | An expression marked for transformation.
-- Holds an expression to be transformed but acts like a hole.
data Fragment name m expr = Fragment
    { _fExpr :: expr
    , _fAttach :: Attach m
    , _fOptions :: m [HoleOption m (Expression name m ())]
    } deriving (Functor, Foldable, Traversable)

data Body name m expr
    = BodyLam (Lambda name m expr)
    | BodySimpleApply (V.Apply expr)
    | BodyLabeledApply (LabeledApply name m expr)
    | BodyHole (Hole m (Expression name m ()))
    | BodyLiteral (Literal (Property m))
    | BodyRecord (Composite name m expr)
    | BodyGetField (GetField name m expr)
    | BodyCase (Case name m expr)
    | BodyGuard (Guard m expr)
    | BodyInject (Inject name m expr)
    | BodyGetVar (GetVar name m)
    | BodyToNom (Nominal name (BinderBody name m expr))
    | BodyFromNom (Nominal name expr)
    | BodyFragment (Fragment name m expr)
    | BodyPlaceHolder -- Used for hole results, shown as "★"
    deriving (Functor, Foldable, Traversable)

instance (Show name, Show expr) => Show (LabeledApply name m expr) where
    show (LabeledApply func specialArgs _annArgs _relayedArgs) =
        unwords ["LabeledApply of", show func, "with", show specialArgs, "..."]

instance (Show name, Show expr) => Show (Body name m expr) where
    show (BodyLam _) = "TODO show lam"
    show BodyHole {} = "Hole"
    show BodyLiteral {} = "Literal"
    show BodySimpleApply {} = "SimpleApply:TODO"
    show (BodyLabeledApply x) = show x
    show BodyRecord {} = "Record:TODO"
    show BodyGetField {} = "GetField:TODO"
    show BodyCase {} = "Case:TODO"
    show BodyGuard {} = "If:TODO"
    show BodyInject {} = "Inject:TODO"
    show BodyGetVar {} = "GetVar:TODO"
    show BodyFromNom {} = "FromNom:TODO"
    show BodyToNom {} = "ToNom:TODO"
    show BodyPlaceHolder {} = "InjectedExpression"
    show BodyFragment {} = "Fragment:TODO"

Lens.makeLenses ''Actions
Lens.makeLenses ''AnnotatedArg
Lens.makeLenses ''Body
Lens.makeLenses ''Case
Lens.makeLenses ''CaseArg
Lens.makeLenses ''ClosedCompositeActions
Lens.makeLenses ''Composite
Lens.makeLenses ''CompositeAddItemResult
Lens.makeLenses ''CompositeItem
Lens.makeLenses ''Expression
Lens.makeLenses ''Fragment
Lens.makeLenses ''GetField
Lens.makeLenses ''Guard
Lens.makeLenses ''GuardElseIf
Lens.makeLenses ''Inject
Lens.makeLenses ''LabeledApply
Lens.makeLenses ''Lambda
Lens.makeLenses ''Nominal
Lens.makeLenses ''OpenCompositeActions
Lens.makeLenses ''Payload
Lens.makeLenses ''RelayedArg
Lens.makeLenses ''TId
Lens.makePrisms ''Attach
Lens.makePrisms ''Body
Lens.makePrisms ''CaseKind
Lens.makePrisms ''CompositeTail
Lens.makePrisms ''Literal
Lens.makePrisms ''DetachAction
