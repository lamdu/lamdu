-- | Different leaf types in the Sugar expressions.
-- These don't contain more expressions in them.
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Parts
    ( VarInfo(..), _VarNormal, _VarFunction, _VarAction
    , FuncApplyLimit(..), _UnlimitedFuncApply, _AtMostOneFuncApply
    , Literal(..), _LiteralNum, _LiteralBytes, _LiteralText
    , HoleResultScore(..), hrsNumFragments, hrsScore
    , -- Annotations
      Annotation(..), _AnnotationVal, _AnnotationType, _AnnotationNone
    , ValAnnotation(..), annotationVal, annotationType
    -- Node actions
    , DetachAction(..), _FragmentAlready, _FragmentExprAlready, _DetachAction
    , NodeActions(..), detach, mSetToHole, setToLiteral, extract, mReplaceParent, wrapInRecord
        , mNewLet
    , -- Let
      ExtractDestination(..)
    , -- Binders
      BinderParams(..), _NullParam, _Params
    , BinderBodyScope(..)
    , FuncParam(..), fpInfo, fpAnnotation, fpVarInfo
    , FuncParamActions(..), fpAddNext, fpDelete, fpMOrderBefore, fpMOrderAfter
    , NullParamActions(..), npDeleteLambda
    , ParamInfo(..), piActions, piTag
    , AddFirstParam(..), _AddInitialParam, _PrependParam, _NeedToPickTagToAddFirst
    , AddNextParam(..), _AddNext, _NeedToPickTagToAddNext
    , -- Expressions
      Payload(..), plEntityId, plAnnotation, plNeverShrinkAnnotation, plActions, plData
    , ClosedCompositeActions(..), closedCompositeOpen
    , OpenCompositeActions(..), openCompositeClose
    , CompositeTail(..), _OpenComposite, _ClosedComposite
    , NullaryVal(..), nullaryClosedCompositeActions, nullaryAddItem
    , Heal(..), _HealAction, _TypeMismatch
    ) where

import qualified Control.Lens as Lens
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Eval
import           Lamdu.Sugar.Types.Tag
import           Lamdu.Sugar.Types.Type

import           Lamdu.Prelude

-- Can a lambda be called more than once? (Used for scope selector presentations)
data FuncApplyLimit = UnlimitedFuncApply | AtMostOneFuncApply
    deriving (Eq, Ord, Generic)

-- Value annotations may also have types as fallbacks in scopes where no value was calculated
data ValAnnotation name i =
    ValAnnotation
    { _annotationVal :: EvaluationScopes name i
    , _annotationType :: Maybe (Type name)
    } deriving Generic

data Annotation name i
    = AnnotationType (Type name)
    | AnnotationVal (ValAnnotation name i)
    | AnnotationNone
    deriving Generic

data AddNextParam name i o
    = AddNext (TagReplace name i o ())
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

data ParamInfo name i o = ParamInfo
    { _piTag :: TagRef name i o
    , _piActions :: FuncParamActions name i o
    } deriving Generic

data FuncParam name i info = FuncParam
    { _fpAnnotation :: Annotation name i
    , _fpVarInfo :: VarInfo
    , _fpInfo :: info
    } deriving (Functor, Foldable, Traversable, Generic)

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
    , _setToLiteral :: Literal Identity -> o EntityId
    , _extract :: o ExtractDestination
    , _mReplaceParent :: Maybe (o EntityId)
    , _wrapInRecord :: TagReplace name i o ()
    , _mNewLet :: Maybe (o EntityId)
    } deriving Generic

data AddFirstParam name i o
    = -- The inital param is created with anon-tag
      AddInitialParam (o EntityId)
    | PrependParam (TagReplace name i o ())
    | -- When the param has anon tag one can't add another one,
      -- contains the EntityId of the param requiring tag.
      NeedToPickTagToAddFirst EntityId
    deriving Generic

-- TODO: rename BinderParams -> Params
data BinderParams name i o
    = -- null param represents a lambda whose parameter's type is inferred
      -- to be the empty record.
      -- This is often used to represent "deferred execution"
      NullParam (FuncParam name i (NullParamActions o))
    | Params [FuncParam name i (ParamInfo name i o)]
    deriving Generic

data BinderBodyScope
    = SameAsParentScope
      -- ^ no binder params
    | BinderBodyScope ParamScopes
      -- ^ binder has params, use the map to get the param application
      -- scopes
    deriving (Generic, Eq)

data VarInfo = VarNormal | VarFunction | VarAction
    deriving (Generic, Eq)

data Payload name i o a = Payload
    { _plAnnotation :: Annotation name i
    , _plNeverShrinkAnnotation :: Bool
    , _plActions :: NodeActions name i o
    , _plEntityId :: EntityId
    , _plData :: a
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

-- | The empty record in a nullary inject
data NullaryVal name i o = NullaryVal
    { _nullaryClosedCompositeActions :: ClosedCompositeActions o
    , _nullaryAddItem :: TagReplace name i o EntityId
    } deriving Generic

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

Lens.makeLenses ''ClosedCompositeActions
Lens.makeLenses ''FuncParam
Lens.makeLenses ''FuncParamActions
Lens.makeLenses ''HoleResultScore
Lens.makeLenses ''NodeActions
Lens.makeLenses ''NullaryVal
Lens.makeLenses ''NullParamActions
Lens.makeLenses ''OpenCompositeActions
Lens.makeLenses ''ParamInfo
Lens.makeLenses ''Payload
Lens.makeLenses ''ValAnnotation
Lens.makePrisms ''AddFirstParam
Lens.makePrisms ''AddNextParam
Lens.makePrisms ''Annotation
Lens.makePrisms ''BinderParams
Lens.makePrisms ''CompositeTail
Lens.makePrisms ''DetachAction
Lens.makePrisms ''FuncApplyLimit
Lens.makePrisms ''Heal
Lens.makePrisms ''Literal
Lens.makePrisms ''VarInfo
