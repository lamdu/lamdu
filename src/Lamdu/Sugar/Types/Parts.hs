-- | Different leaf types in the Sugar expressions.
-- These don't contain more expressions in them.
{-# LANGUAGE TemplateHaskell, DataKinds, GADTs, TypeFamilies #-}
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
    , Delete(..), _SetToHole, _Delete, _CannotDelete
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
      ClosedCompositeActions(..), closedCompositeOpen
    , OpenCompositeActions(..), openCompositeClose
    , PunnedVar(..), pvVar, pvTagEntityId
    , NullaryVal(..), nullaryClosedCompositeActions, nullaryAddItem

    , ParenInfo(..), piNeedParens, piMinOpPrec
    ) where

import qualified Control.Lens as Lens
import           Hyper (makeHTraversableAndBases)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.GetVar
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

data Delete m
    = SetToHole (m EntityId)
    | -- Changes the structure around the hole to remove the hole.
      -- For example (f _) becomes (f) or (2 + _) becomes 2
      Delete (m EntityId)
    | CannotDelete
    deriving Generic

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

data PunnedVar name o k = PunnedVar
    { _pvVar :: k :# Const (GetVar name o)
    , _pvTagEntityId :: EntityId
    } deriving Generic

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

traverse Lens.makeLenses
    [ ''ClosedCompositeActions, ''FuncParam, ''FuncParamActions, ''HoleResultScore
    , ''NullParamActions, ''NullaryVal, ''OpenCompositeActions, ''ParamInfo, ''ParenInfo, ''PunnedVar
    ] <&> concat
traverse Lens.makePrisms
    [ ''AddFirstParam, ''AddNextParam, ''Annotation, ''BinderParams, ''Delete
    , ''DetachAction, ''FuncApplyLimit, ''HoleTerm, ''Literal, ''VarInfo
    ] <&> concat
makeHTraversableAndBases ''PunnedVar
