-- | Different leaf types in the Sugar expressions.
-- These don't contain more expressions in them.
{-# LANGUAGE TemplateHaskell, DataKinds, GADTs, TypeFamilies, MultiParamTypeClasses #-}
module Lamdu.Sugar.Types.Parts
    ( VarInfo(..), _VarNominal, _VarGeneric, _VarFunction, _VarRecord, _VarVariant
    , FuncApplyLimit(..), _UnlimitedFuncApply, _AtMostOneFuncApply
    , Literal(..), _LiteralNum, _LiteralBytes, _LiteralText
    , -- Annotations
      Annotation(..), _AnnotationVal, _AnnotationType, _AnnotationNone
    -- Node actions
    , DetachAction(..), _FragmentedAlready, _DetachAction
    , Delete(..), _SetToHole, _Delete, _CannotDelete
    , NodeActions(..)
        , detach, delete, setToLiteral, extract, mReplaceParent, mNewLet, mApply
    , -- Let
      ExtractDestination(..)
    , -- TaggedList
      TaggedList(..), tlAddFirst, tlItems
    , TaggedSwappableItem(..), tsiItem, tsiSwapWithPrevious
    , TaggedListBody(..), tlHead, tlTail
    , TaggedItem(..), tiTag, tiDelete, tiValue, tiAddAfter
    , -- Binders
      BinderParams(..), _NullParam, _VarParam, _RecordParams
    , FuncParam(..), fpAnnotation, fpVarInfo
    , NullParamActions(..), npDeleteLambda
    , VarParamInfo(..), vpiTag, vpiAddNext, vpiDelete
    , RecordParamInfo(..), piTag, piAddNext, piDelete, piMOrderBefore, piMOrderAfter
    , AddFirstParam(..), _AddInitialParam, _PrependParam, _NeedToPickTagToAddFirst
    , AddNextParam(..), _AddNext, _NeedToPickTagToAddNext
    , -- Expressions
      Payload(..), plEntityId, plAnnotation, plActions, plHiddenEntityIds, plParenInfo
    , ClosedCompositeActions(..), closedCompositeOpen
    , PunnedVar(..), pvVar, pvTagEntityId
    , NullaryInject(..), iInject, iContent

    , ParenInfo(..), piNeedParens, piMinOpPrec
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Unit (Unit)
import           Hyper (makeHTraversableAndBases, makeHMorph)
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
    = AnnotationType (Annotated EntityId # Type name Unit)
    | AnnotationVal v
    | AnnotationNone
    deriving Generic

data AddNextParam name i o
    = AddNext (TagChoice name i o ())
    | -- When the param has anon tag one can't add another one,
      -- contains the EntityId of the param requiring tag.
      NeedToPickTagToAddNext EntityId
    deriving Generic

newtype NullParamActions o = NullParamActions
    { _npDeleteLambda :: o ()
    } deriving stock Generic

data VarParamInfo name i o = VarParamInfo
    { _vpiTag :: TagRef name i o
    , _vpiAddNext :: AddNextParam name i o
    , _vpiDelete :: o ()
    } deriving Generic

data RecordParamInfo name i o = RecordParamInfo
    { _piTag :: TagRef name i o
    , _piAddNext :: TagChoice name i o ()
    , _piDelete :: o ()
    , _piMOrderBefore :: Maybe (o ())
    , _piMOrderAfter :: Maybe (o ())
    } deriving Generic

data FuncParam v = FuncParam
    { _fpAnnotation :: v
    , _fpVarInfo :: VarInfo
    } deriving Generic

data ExtractDestination
    = ExtractToLet EntityId
    | ExtractToDef EntityId

data DetachAction o
    = FragmentedAlready EntityId -- Already a fragment, its expr or hole
    | DetachAction (o EntityId) -- Detach me
    deriving Generic

data Delete m
    = SetToHole (m EntityId)
    | -- Changes the structure around the hole to remove the hole.
      -- For example (f _) becomes (f) or (2 + _) becomes 2
      Delete (m EntityId)
    | CannotDelete
    deriving Generic

data NodeActions o = NodeActions
    { _detach :: DetachAction o
    , _delete :: Delete o
    , _setToLiteral :: Literal Identity -> o EntityId
    , _extract :: o ExtractDestination
    , _mReplaceParent :: Maybe (o EntityId)
    , _mNewLet :: Maybe (o EntityId)
    , _mApply :: Maybe (o EntityId)
    } deriving Generic

data TaggedItem name i o a = TaggedItem
    { _tiTag :: TagRef name i o
    , _tiDelete :: o EntityId
    , _tiAddAfter :: TagChoice name i o EntityId
    , _tiValue :: a
    } deriving (Generic, Functor, Foldable, Traversable)

data TaggedSwappableItem name i o a = TaggedSwappableItem
    { _tsiItem :: TaggedItem name i o a
    , _tsiSwapWithPrevious :: o ()
    } deriving (Generic, Functor, Foldable, Traversable)

data TaggedListBody name i o a = TaggedListBody
    { _tlHead :: TaggedItem name i o a
        -- The 2nd tagged item onwards can be swapped with their previous item
    , _tlTail :: [TaggedSwappableItem name i o a]
    } deriving (Generic, Functor, Foldable, Traversable)

data TaggedList name i o a = TaggedList
    { _tlAddFirst :: TagChoice name i o EntityId
    , _tlItems :: Maybe (TaggedListBody name i o a)
    } deriving (Generic, Functor, Foldable, Traversable)

data AddFirstParam name i o
    = -- The inital param is created with anon-tag
      AddInitialParam (o EntityId)
    | PrependParam (TagChoice name i o ())
    | -- When the param has anon tag one can't add another one,
      -- contains the EntityId of the param requiring tag.
      NeedToPickTagToAddFirst EntityId
    deriving Generic

-- TODO: rename BinderParams -> Params
data BinderParams v name i o
    = -- null param represents a lambda whose parameter's type is inferred
      -- to be the empty record.
      -- This is often used to represent "deferred execution"
      NullParam (FuncParam v, NullParamActions o)
    | VarParam (FuncParam v, VarParamInfo name i o)
    | RecordParams [(FuncParam v, RecordParamInfo name i o)]
    deriving Generic

-- VarInfo is used for:
-- * Differentiating Mut actions so UI can suggest executing them
-- * Name pass giving parameters names according to types
data VarInfo
    = VarNominal (TId T.Tag Unit)
    | VarGeneric | VarFunction | VarRecord | VarUnit | VarVariant | VarVoid
    deriving (Generic, Eq)

data Payload v o = Payload
    { _plAnnotation :: v
    , _plActions :: NodeActions o
    , _plEntityId :: EntityId
    , _plParenInfo :: !ParenInfo
    , _plHiddenEntityIds :: [EntityId]
    } deriving Generic

newtype ClosedCompositeActions o = ClosedCompositeActions
    { _closedCompositeOpen :: o EntityId
    } deriving stock Generic

data Literal f
    = LiteralNum (f Double)
    | LiteralBytes (f ByteString)
    | LiteralText (f Text)
    deriving Generic

data NullaryInject name i o k = NullaryInject
    { _iInject :: k :# Const (TagRef name i o)
    , -- Child represents the empty record, and has action to add an item
      _iContent :: k :# Const (TagChoice name i o EntityId)
    } deriving Generic

data PunnedVar name o k = PunnedVar
    { _pvVar :: k :# Const (GetVar name o)
    , _pvTagEntityId :: EntityId
    } deriving Generic

data ParenInfo = ParenInfo
    { _piMinOpPrec :: !Int
    , _piNeedParens :: !Bool
    } deriving (Eq, Show, Generic)

traverse Lens.makeLenses
    [ ''ClosedCompositeActions, ''FuncParam, ''NodeActions
    , ''NullParamActions, ''NullaryInject, ''VarParamInfo, ''RecordParamInfo, ''ParenInfo, ''Payload, ''PunnedVar
    , ''TaggedList, ''TaggedListBody, ''TaggedItem, ''TaggedSwappableItem
    ] <&> concat
traverse Lens.makePrisms
    [ ''AddFirstParam, ''AddNextParam, ''Annotation, ''BinderParams, ''Delete
    , ''DetachAction, ''FuncApplyLimit, ''Literal, ''VarInfo
    ] <&> concat
traverse makeHTraversableAndBases [''NullaryInject, ''PunnedVar] <&> concat
makeHMorph ''NullaryInject
