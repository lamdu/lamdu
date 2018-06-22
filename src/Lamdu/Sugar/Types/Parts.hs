-- | Different leaf types in the Sugar expressions.
-- These don't contain more expressions in them.
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Parts
    ( FuncApplyLimit(..), _UnlimitedFuncApply, _AtMostOneFuncApply
    , Literal(..), _LiteralNum, _LiteralBytes, _LiteralText
    , HoleResultScore(..), hrsNumFragments, hrsScore
    , -- Annotations
      Annotation(..), aInferredType, aMEvaluationResult
    -- Node actions
    , DetachAction(..), _FragmentAlready, _FragmentExprAlready, _DetachAction
    , NodeActions(..), detach, mSetToHole, extract, mReplaceParent, wrapInRecord
    , -- Let
      ExtractDestination(..)
    , LetActions(..), laDelete, laNodeActions
    , -- Binders
      BinderParams(..), _NullParam, _Params
    , BinderBodyScope(..)
    , FuncParam(..), fpInfo, fpAnnotation
    , FuncParamActions(..), fpAddNext, fpDelete, fpMOrderBefore, fpMOrderAfter
    , NullParamActions(..), npDeleteLambda
    , ParamInfo(..), piActions, piTag
    , AddFirstParam(..), _AddInitialParam, _PrependParam, _NeedToPickTagToAddFirst
    , AddNextParam(..), _AddNext, _NeedToPickTagToAddNext
    , -- Expressions
      Payload(..), plEntityId, plAnnotation, plActions, plData
    , ClosedCompositeActions(..), closedCompositeOpen
    , OpenCompositeActions(..), openCompositeClose
    , CompositeTail(..), _OpenComposite, _ClosedComposite
    , NullaryVal(..), nullaryPayload, nullaryClosedCompositeActions, nullaryAddItem
    , LabeledApplyFunc(..), afVar, afPayload
    , RelayedArg(..), raValue, raId, raActions
    , Heal(..), _HealAction, _TypeMismatch
    ) where

import qualified Control.Lens as Lens
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Eval
import           Lamdu.Sugar.Types.GetVar
import           Lamdu.Sugar.Types.Tag
import           Lamdu.Sugar.Types.Type

import           Lamdu.Prelude

-- Can a lambda be called more than once? (Used for scope selector presentations)
data FuncApplyLimit = UnlimitedFuncApply | AtMostOneFuncApply
    deriving (Eq, Ord, Show, Generic)

data Annotation name = Annotation
    { _aInferredType :: Type name
    , _aMEvaluationResult :: EvaluationScopes name
    } deriving (Show, Generic)

data AddNextParam name i o
    = AddNext (TagSelection name i o ())
    | -- When the param has anon tag one can't add another one,
      -- contains the EntityId of the param requiring tag.
      NeedToPickTagToAddNext EntityId
    deriving Generic

data FuncParamActions name i o =
    FuncParamActions
    { _fpAddNext :: AddNextParam name i o
    , _fpDelete :: o ()
    , _fpMOrderBefore :: Maybe (o ())
    , _fpMOrderAfter :: Maybe (o ())
    } deriving Generic

newtype NullParamActions o = NullParamActions
    { _npDeleteLambda :: o ()
    } deriving Generic
instance Show (NullParamActions o) where
    show (NullParamActions _) = "(NullParamActions)"

data ParamInfo name i o = ParamInfo
    { _piTag :: Tag name i o
    , _piActions :: FuncParamActions name i o
    } deriving Generic
instance Show name => Show (ParamInfo name i o) where
    show (ParamInfo tag _) = show tag

data FuncParam name info = FuncParam
    { _fpAnnotation :: Annotation name
    , _fpInfo :: info
    } deriving (Functor, Foldable, Traversable, Generic)

instance (Show info, Show name) => Show (FuncParam name info) where
    show (FuncParam a i) = "(FuncParam " ++ show a ++ " " ++ show i ++ " )"

data ExtractDestination
    = ExtractToLet EntityId
    | ExtractToDef EntityId

data DetachAction o
    = FragmentAlready EntityId -- I'o an apply-of-hole, no need to detach
    | FragmentExprAlready EntityId -- I'o an arg of apply-of-hole, no need to detach
    | DetachAction (o EntityId) -- Detach me
    deriving Generic

data NodeActions name i o = NodeActions
    { _detach :: DetachAction o
    , _mSetToHole :: Maybe (o EntityId) -- (Not available for holes)
    , _extract :: o ExtractDestination
    , _mReplaceParent :: Maybe (o EntityId)
    , _wrapInRecord :: TagSelection name i o ()
    } deriving Generic

data LetActions name i o = LetActions
    { _laDelete :: o ()
    , _laNodeActions :: NodeActions name i o
    } deriving Generic

data AddFirstParam name i o
    = -- The inital param is created with anon-tag
      AddInitialParam (o EntityId)
    | PrependParam (TagSelection name i o ())
    | -- When the param has anon tag one can't add another one,
      -- contains the EntityId of the param requiring tag.
      NeedToPickTagToAddFirst EntityId
    deriving Generic

data BinderParams name i o
    = -- null param represents a lambda whose parameter's type is inferred
      -- to be the empty record.
      -- This is often used to represent "deferred execution"
      NullParam (FuncParam name (NullParamActions o))
    | Params [FuncParam name (ParamInfo name i o)]
    deriving Generic

data BinderBodyScope
    = SameAsParentScope
      -- ^ no binder params
    | BinderBodyScope ParamScopes
      -- ^ binder has params, use the map to get the param application
      -- scopes
    deriving (Generic, Eq)

data Payload name i o a = Payload
    { _plAnnotation :: Annotation name
    , _plActions :: NodeActions name i o
    , _plEntityId :: EntityId
    , _plData :: a
    } deriving (Functor, Foldable, Traversable, Generic)
instance Show a => Show (Payload name i o a) where
    show (Payload _ann _actions _entityId data_) = show data_

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

-- | The empty record (for manipulations in GUI)
data NullaryVal name i o a = NullaryVal
    { _nullaryPayload :: a
    , _nullaryClosedCompositeActions :: ClosedCompositeActions o
    , _nullaryAddItem :: TagSelection name i o EntityId
    } deriving (Functor, Foldable, Traversable, Generic)

data RelayedArg name i o = RelayedArg
    { _raValue :: GetVar name o
    , _raId :: EntityId
    , _raActions :: NodeActions name i o
    } deriving Generic

data LabeledApplyFunc name o a = LabeledApplyFunc
    { _afVar :: BinderVarRef name o
    , _afPayload :: a
    } deriving (Functor, Foldable, Traversable, Generic)

instance (Show name, Show a) => Show (LabeledApplyFunc name o a) where
    show (LabeledApplyFunc func pl) = concat [show func, "{", show pl, "}"]

data Heal o
    = HealAction (o EntityId)
    | TypeMismatch
    deriving Generic

data Literal f
    = LiteralNum (f Double)
    | LiteralBytes (f ByteString)
    | LiteralText (f Text)
    deriving Generic

data HoleResultScore = HoleResultScore
    { _hrsNumFragments :: !Int
    , _hrsScore :: ![Int]
    } deriving (Eq, Ord, Generic)

Lens.makeLenses ''Annotation
Lens.makeLenses ''ClosedCompositeActions
Lens.makeLenses ''FuncParam
Lens.makeLenses ''FuncParamActions
Lens.makeLenses ''HoleResultScore
Lens.makeLenses ''LabeledApplyFunc
Lens.makeLenses ''LetActions
Lens.makeLenses ''NodeActions
Lens.makeLenses ''NullaryVal
Lens.makeLenses ''NullParamActions
Lens.makeLenses ''OpenCompositeActions
Lens.makeLenses ''ParamInfo
Lens.makeLenses ''Payload
Lens.makeLenses ''RelayedArg
Lens.makePrisms ''AddFirstParam
Lens.makePrisms ''AddNextParam
Lens.makePrisms ''BinderParams
Lens.makePrisms ''CompositeTail
Lens.makePrisms ''DetachAction
Lens.makePrisms ''FuncApplyLimit
Lens.makePrisms ''Heal
Lens.makePrisms ''Literal
