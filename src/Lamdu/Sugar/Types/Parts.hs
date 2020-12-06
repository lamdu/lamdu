-- | Different leaf types in the Sugar expressions.
-- These don't contain more expressions in them.
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Parts
    ( VarInfo(..), _VarNominal, _VarGeneric, _VarFunction, _VarRecord, _VarVariant
    , FuncApplyLimit(..), _UnlimitedFuncApply, _AtMostOneFuncApply
    , Literal(..), _LiteralNum, _LiteralBytes, _LiteralText
    , HoleResultScore(..), hrsNumFragments, hrsScore
    , HoleTerm(..), _HoleName
    , -- Annotations
      Annotation(..), _AnnotationVal, _AnnotationType, _AnnotationNone
    -- Node actions
    , DetachAction(..), _FragmentAlready, _FragmentExprAlready, _DetachAction
    , NodeActions(..)
        , detach, mSetToHole, setToLiteral, setToEmptyRecord
        , extract, mReplaceParent, wrapInRecord, mNewLet
    , -- Let
      ExtractDestination(..)
    , -- Binders
      BinderParams(..), _NullParam, _Params
    , FuncParam(..), fpAnnotation, fpVarInfo
    , FuncParamActions(..), fpAddNext, fpDelete, fpMOrderBefore, fpMOrderAfter
    , NullParamActions(..), npDeleteLambda
    , ParamInfo(..), piActions, piTag
    , AddFirstParam(..), _AddInitialParam, _PrependParam, _NeedToPickTagToAddFirst
    , AddNextParam(..), _AddNext, _NeedToPickTagToAddNext
    , -- Expressions
      Payload(..), plEntityId, plAnnotation, plNeverShrinkTypeAnnotations, plActions
    , ClosedCompositeActions(..), closedCompositeOpen
    , OpenCompositeActions(..), openCompositeClose
    , NullaryVal(..), nullaryClosedCompositeActions, nullaryAddItem

    , ParenInfo(..), piNeedParens, piMinOpPrec
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Tag
import           Lamdu.Sugar.Types.Type

import           Lamdu.Prelude

-- Can a lambda be called more than once?
-- Used to avoid showing scope selector presentations for case alternative lambdas.
data FuncApplyLimit = UnlimitedFuncApply | AtMostOneFuncApply
    deriving (Eq, Ord, Generic)

data Annotation v name
    = AnnotationType (Annotated EntityId # Type name)
    | AnnotationVal v
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
    } deriving stock Generic

data ParamInfo name i o = ParamInfo
    { _piTag :: TagRef name i o
    , _piActions :: FuncParamActions name i o
    } deriving Generic

data FuncParam v name = FuncParam
    { _fpAnnotation :: v
    , _fpVarInfo :: VarInfo
    } deriving Generic

data ExtractDestination
    = ExtractToLet EntityId
    | ExtractToDef EntityId

data DetachAction o
    = FragmentAlready EntityId -- I'm an apply-of-hole, no need to detach
    | FragmentExprAlready EntityId -- I'm an arg of apply-of-hole, no need to detach
    | DetachAction (o EntityId) -- Detach me
    deriving Generic

data NodeActions name i o = NodeActions
    { _detach :: DetachAction o
    , _mSetToHole :: Maybe (o EntityId) -- (Not available for holes)
    , _setToLiteral :: Literal Identity -> o EntityId
    , _setToEmptyRecord :: o EntityId
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
data BinderParams v name i o
    = -- null param represents a lambda whose parameter's type is inferred
      -- to be the empty record.
      -- This is often used to represent "deferred execution"
      NullParam (FuncParam v name, NullParamActions o)
    | Params [(FuncParam v name, ParamInfo name i o)]
    deriving Generic

-- VarInfo is used for:
-- * Differentiating Mut actions so UI can suggest executing them
-- * Name pass giving parameters names according to types
data VarInfo
    = VarNominal (TId T.Tag)
    | VarGeneric | VarFunction | VarRecord | VarVariant
    deriving (Generic, Eq)

data Payload v name i o = Payload
    { _plAnnotation :: v
    , _plNeverShrinkTypeAnnotations :: Bool
    , _plActions :: NodeActions name i o
    , _plEntityId :: EntityId
    } deriving Generic

newtype ClosedCompositeActions o = ClosedCompositeActions
    { _closedCompositeOpen :: o EntityId
    } deriving stock Generic

newtype OpenCompositeActions o = OpenCompositeActions
    { _openCompositeClose :: o EntityId
    } deriving stock Generic

-- | The empty record in a nullary inject
data NullaryVal name i o = NullaryVal
    { _nullaryClosedCompositeActions :: ClosedCompositeActions o
    , _nullaryAddItem :: TagReplace name i o EntityId
    } deriving Generic

data Literal f
    = LiteralNum (f Double)
    | LiteralBytes (f ByteString)
    | LiteralText (f Text)
    deriving Generic

data HoleResultScore = HoleResultScore
    { _hrsNumFragments :: !Int
    , _hrsScore :: ![Int]
    } deriving (Eq, Ord, Generic)

data HoleTerm name
    = HoleGetDef name
    | HoleName name
    | HoleInject name
    | HoleGetField name
    | HoleParamsRecord
    | HoleRecord
    | HoleCase
    | HoleEmptyCase
    | HoleIf
    | HoleLambda
    | HoleLet
    deriving (Functor, Foldable, Traversable, Show)

data ParenInfo = ParenInfo
    { _piMinOpPrec :: !Int
    , _piNeedParens :: !Bool
    } deriving (Eq, Show, Generic)

Lens.makeLenses ''ClosedCompositeActions
Lens.makeLenses ''FuncParam
Lens.makeLenses ''FuncParamActions
Lens.makeLenses ''HoleResultScore
Lens.makeLenses ''NodeActions
Lens.makeLenses ''NullParamActions
Lens.makeLenses ''NullaryVal
Lens.makeLenses ''OpenCompositeActions
Lens.makeLenses ''ParamInfo
Lens.makeLenses ''ParenInfo
Lens.makeLenses ''Payload
Lens.makePrisms ''AddFirstParam
Lens.makePrisms ''AddNextParam
Lens.makePrisms ''Annotation
Lens.makePrisms ''BinderParams
Lens.makePrisms ''DetachAction
Lens.makePrisms ''FuncApplyLimit
Lens.makePrisms ''HoleTerm
Lens.makePrisms ''Literal
Lens.makePrisms ''VarInfo
