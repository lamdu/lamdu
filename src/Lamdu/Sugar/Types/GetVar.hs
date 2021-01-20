{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.GetVar
    ( NameRef(..), nrName, nrGotoDefinition
    , ParamRef(..), pNameRef, pBinderMode
    , BinderVarForm(..), _GetDefinition, _GetLet
    , DefinitionForm(..), _DefUpToDate, _DefDeleted, _DefTypeChanged
    , DefinitionOutdatedType(..), defTypeWhenUsed, defTypeCurrent, defTypeUseCurrent
    , BinderVarInline(..), _InlineVar, _CannotInlineDueToUses, _CannotInline
    , BinderVarRef(..), bvNameRef, bvForm, bvVar, bvInline
    , BinderMode(..), _NormalBinder, _LightLambda
    , GetVar(..), _GetParam, _GetParamsRecord, _GetBinder
    , ParamsRecordVarRef(..), prvFieldNames
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Term as V
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Type

import           Lamdu.Prelude

data BinderMode = NormalBinder | LightLambda
    deriving (Generic, Eq)

data NameRef name o = NameRef
    { _nrName :: name
    , _nrGotoDefinition :: o EntityId
    } deriving Generic

data ParamRef name o = ParamRef
    { _pNameRef :: NameRef name o
    , _pBinderMode :: BinderMode
    } deriving Generic

data DefinitionOutdatedType name o a = DefinitionOutdatedType
    { _defTypeWhenUsed :: Scheme name
    , _defTypeCurrent :: Scheme name
    , _defTypeUseCurrent :: o a
    } deriving (Functor, Foldable, Traversable, Generic)

data DefinitionForm name o
    = DefUpToDate
    | DefDeleted
    | DefTypeChanged (DefinitionOutdatedType name o EntityId)
    deriving (Generic)

data BinderVarForm name o
    = GetDefinition (DefinitionForm name o)
    | GetLet
    deriving (Generic)

data BinderVarInline o
    = InlineVar (o EntityId)
    | CannotInlineDueToUses [EntityId]
    | CannotInline
    deriving Generic

data BinderVarRef name o = BinderVarRef
    { _bvNameRef :: NameRef name o
    , _bvForm :: BinderVarForm name o
    , _bvVar :: V.Var
    , -- Just means it is stored and inlinable:
      _bvInline :: BinderVarInline o
    } deriving Generic

newtype ParamsRecordVarRef name = ParamsRecordVarRef
    { _prvFieldNames :: [name]
    } deriving stock (Eq, Ord, Functor, Foldable, Traversable, Generic)

data GetVar name o
    = GetParam (ParamRef name o)
    | GetParamsRecord (ParamsRecordVarRef name)
    | GetBinder (BinderVarRef name o)
    deriving Generic

traverse Lens.makeLenses
    [''BinderVarRef, ''DefinitionOutdatedType, ''NameRef, ''ParamRef, ''ParamsRecordVarRef] <&> concat
traverse Lens.makePrisms
    [''BinderMode, ''BinderVarForm, ''BinderVarInline, ''DefinitionForm, ''GetVar] <&> concat
