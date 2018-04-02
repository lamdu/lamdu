{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Expression
    ( Body(..)
        , _BodyLam, _BodyLabeledApply, _BodySimpleApply
        , _BodyGetVar, _BodyGetField, _BodyInject, _BodyHole
        , _BodyLiteral, _BodyCase, _BodyRecord, _BodyFragment
        , _BodyFromNom, _BodyToNom, _BodyIfElse
    , Payload(..), plEntityId, plAnnotation, plActions, plData
    , Payload'
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
    , AnnotatedArg(..), aaTag, aaExpr
    , RelayedArg(..), raValue, raId, raActions
    , LabeledApply(..), aFunc, aSpecialArgs, aAnnotatedArgs, aRelayedArgs
    , Fragment(..), fExpr, fAttach, fOptions
    , Attach(..), _AttachAction, _AttachTypeMismatch
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

data Payload name im am a = Payload
    { _plAnnotation :: Annotation name
    , _plActions :: NodeActions name im am
    , _plEntityId :: EntityId
    , _plData :: a
    } deriving (Functor, Foldable, Traversable)
instance Show a => Show (Payload name im am a) where
    show (Payload _ann _actions _entityId data_) = show data_

type Payload' name m = Payload name m m

data Expression name im am a = Expression
    { _rBody :: Body name im am (Expression name im am a)
    , _rPayload :: Payload name im am a
    } deriving (Functor, Foldable, Traversable)
instance (Show name, Show a) => Show (Expression name im am a) where
    show (Expression body pl) = show body ++ "{" ++ show pl ++ "}"

{- Composites start -}
data CompositeItem name im am expr = CompositeItem
    { _ciDelete :: am EntityId
    , _ciTag :: Tag name im am
    , _ciExpr :: expr
    } deriving (Functor, Foldable, Traversable)

newtype ClosedCompositeActions am = ClosedCompositeActions
    { _closedCompositeOpen :: am EntityId
    }

newtype OpenCompositeActions am = OpenCompositeActions
    { _openCompositeClose :: am EntityId
    }

data CompositeTail am expr
    = OpenComposite (OpenCompositeActions am) expr
    | ClosedComposite (ClosedCompositeActions am)
    deriving (Functor, Foldable, Traversable)

data Composite name im am expr = Composite
    { _cItems :: [CompositeItem name im am expr]
    , _cTail :: CompositeTail am expr
    , _cAddItem :: TagSelection name im am EntityId
    } deriving (Functor, Foldable, Traversable)

data CaseArg am expr = CaseArg
    { _caVal :: expr
    , _caToLambdaCase :: am EntityId
    } deriving (Functor, Foldable, Traversable)

data CaseKind am expr
    = LambdaCase
    | CaseWithArg (CaseArg am expr)
    deriving (Functor, Foldable, Traversable)

data Case name im am expr = Case
    { _cKind :: CaseKind am expr
    , _cBody :: Composite name im am expr
    } deriving (Functor, Foldable, Traversable)
{- Composites end -}

-- An "if/elif <cond>: <then>" clause in an IfElse expression
data IfThen am expr = IfThen
    { _itIf :: expr
    , _itThen :: expr
    , _itDelete :: am EntityId
    } deriving (Functor, Foldable, Traversable)

-- An "elif <cond>: <then>" clause in an IfElse expression and the subtree under it
data ElseIfContent name im am expr = ElseIfContent
    { _eiScopes :: ChildScopes
    , _eiEntityId :: EntityId
    , _eiContent :: IfElse name im am expr
    , _eiCondAddLet :: am EntityId
    , _eiNodeActions :: NodeActions name im am
    } deriving (Functor, Foldable, Traversable)

data Else name im am expr = SimpleElse expr | ElseIf (ElseIfContent name im am expr)
    deriving (Functor, Foldable, Traversable)

data IfElse name im am expr = IfElse
    { _iIfThen :: IfThen am expr
    , _iElse :: Else name im am expr
    } deriving (Functor, Foldable, Traversable)

data GetField name im am expr = GetField
    { _gfRecord :: expr
    , _gfTag :: Tag name im am
    } deriving (Functor, Foldable, Traversable)

data Inject name im am expr = Inject
    { _iTag :: Tag name im am
    , _iMVal :: Maybe expr
    } deriving (Functor, Foldable, Traversable)

data AnnotatedArg name expr = AnnotatedArg
    { _aaTag :: TagInfo name
    , _aaExpr :: expr
    } deriving (Functor, Foldable, Traversable)

data RelayedArg name im am = RelayedArg
    { _raValue :: GetVar name am
    , _raId :: EntityId
    , _raActions :: NodeActions name im am
    }

data LabeledApply name im am expr = LabeledApply
    { _aFunc :: BinderVarRef name am
    , _aSpecialArgs :: SpecialArgs expr
    , _aAnnotatedArgs :: [AnnotatedArg name expr]
    , _aRelayedArgs :: [RelayedArg name im am]
    } deriving (Functor, Foldable, Traversable)

data Nominal name expr = Nominal
    { _nTId :: TId name
    , _nVal :: expr
    } deriving (Functor, Foldable, Traversable)

data Lambda name im am expr = Lambda
    { _lamMode :: BinderMode
    , _lamBinder :: Binder name im am expr
    } deriving (Functor, Foldable, Traversable)

data Attach am
    = AttachAction (am EntityId)
    | AttachTypeMismatch

-- | An expression marked for transformation.
-- Holds an expression to be transformed but acts like a hole.
data Fragment name im am expr = Fragment
    { _fExpr :: expr
    , _fAttach :: Attach am
    , _fOptions :: im [HoleOption im am (Expression name im am ())]
    } deriving (Functor, Foldable, Traversable)

data Body name im am expr
    = BodyLam (Lambda name im am expr)
    | BodySimpleApply (V.Apply expr)
    | BodyLabeledApply (LabeledApply name im am expr)
    | BodyHole (Hole im am (Expression name im am ()))
    | BodyLiteral (Literal (Property am))
    | BodyRecord (Composite name im am expr)
    | BodyGetField (GetField name im am expr)
    | BodyCase (Case name im am expr)
    | BodyIfElse (IfElse name im am expr)
    | BodyInject (Inject name im am expr)
    | BodyGetVar (GetVar name am)
    | BodyToNom (Nominal name (BinderBody name im am expr))
    | BodyFromNom (Nominal name expr)
    | BodyFragment (Fragment name im am expr)
    | BodyPlaceHolder -- Used for hole results, shown as "★"
    deriving (Functor, Foldable, Traversable)

instance (Show name, Show expr) => Show (LabeledApply name im am expr) where
    show (LabeledApply func specialArgs _annArgs _relayedArgs) =
        unwords ["LabeledApply of", show func, "with", show specialArgs, "..."]

instance (Show name, Show expr) => Show (Body name im am expr) where
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
Lens.makePrisms ''Attach
Lens.makePrisms ''Body
Lens.makePrisms ''CaseKind
Lens.makePrisms ''CompositeTail
Lens.makePrisms ''Else
Lens.makePrisms ''Literal
