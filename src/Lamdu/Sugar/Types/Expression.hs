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
import           Data.Property (Property)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Binder
import           Lamdu.Sugar.Types.GetVar (GetVar, BinderVarRef, ParamRef, BinderMode)
import           Lamdu.Sugar.Types.Hole (Hole, HoleOption, Literal)
import           Lamdu.Sugar.Types.Tag

import           Lamdu.Prelude

data Payload name m a = Payload
    { _plAnnotation :: Annotation
    , _plActions :: NodeActions name m
    , _plEntityId :: EntityId
    , _plData :: a
    } deriving (Functor, Foldable, Traversable)
instance Show a => Show (Payload name m a) where
    show (Payload _ann _actions _entityId data_) = show data_

data Expression name m a = Expression
    { _rBody :: Body name m (Expression name m a)
    , _rPayload :: Payload name m a
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

data Composite name m expr = Composite
    { _cItems :: [CompositeItem name m expr]
    , _cTail :: CompositeTail m expr
    , _cAddItem :: TagSelection name m EntityId
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

-- An "if/elif <cond>: <then>" clause in an IfElse expression
data IfThen m expr = IfThen
    { _itIf :: expr
    , _itThen :: expr
    , _itDelete :: m EntityId
    } deriving (Functor, Foldable, Traversable)

-- An "elif <cond>: <then>" clause in an IfElse expression and the subtree under it
data ElseIfContent name m expr = ElseIfContent
    { _eiScopes :: ChildScopeMapping
    , _eiEntityId :: EntityId
    , _eiContent :: IfElse name m expr
    , _eiCondAddLet :: m EntityId
    , _eiNodeActions :: NodeActions name m
    } deriving (Functor, Foldable, Traversable)

data Else name m expr = SimpleElse expr | ElseIf (ElseIfContent name m expr)
    deriving (Functor, Foldable, Traversable)

data IfElse name m expr = IfElse
    { _iIfThen :: IfThen m expr
    , _iElse :: Else name m expr
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
    , _raActions :: NodeActions name m
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
    | BodyIfElse (IfElse name m expr)
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
    show BodyIfElse {} = "If:TODO"
    show BodyInject {} = "Inject:TODO"
    show BodyGetVar {} = "GetVar:TODO"
    show BodyFromNom {} = "FromNom:TODO"
    show BodyToNom {} = "ToNom:TODO"
    show BodyPlaceHolder {} = "InjectedExpression"
    show BodyFragment {} = "Fragment:TODO"

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
Lens.makePrisms ''Else
Lens.makePrisms ''Literal
