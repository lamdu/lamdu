{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.GetVar
    ( NameRef(..), nrName, nrGotoDefinition
    , ParamRef(..), pNameRef, pBinderMode
    , BinderVarForm(..), _GetDefinition, _GetLet
    , DefinitionForm(..), _DefUpToDate, _DefDeleted, _DefTypeChanged
    , DefinitionOutdatedType(..), defTypeWhenUsed, defTypeCurrent, defTypeUseCurrent
    , BinderVarInline(..), _InlineVar, _CannotInlineDueToUses, _CannotInline
    , BinderVarRef(..), bvNameRef, bvForm, bvVar, bvInline
    , BinderMode(..)
    , GetVar(..), _GetParam, _GetParamsRecord, _GetBinder
    , ParamsRecordVarRef(..), prvFieldNames
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Val as V
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Type

import           Lamdu.Prelude

data BinderMode = NormalBinder | LightLambda
    deriving (Show, Generic)

data NameRef name o = NameRef
    { _nrName :: name
    , _nrGotoDefinition :: o EntityId
    } deriving Generic
instance Show name => Show (NameRef name o) where
    show (NameRef name _) = show name

data ParamRef name o = ParamRef
    { _pNameRef :: NameRef name o
    , _pBinderMode :: BinderMode
    } deriving Generic

data DefinitionOutdatedType name a = DefinitionOutdatedType
    { _defTypeWhenUsed :: Scheme name
    , _defTypeCurrent :: Scheme name
    , _defTypeUseCurrent :: a
    } deriving (Functor, Foldable, Traversable, Generic)
instance Show name => Show (DefinitionOutdatedType name a) where
    show (DefinitionOutdatedType usedType newType _) =
        "(Used @type: " ++ show usedType ++ " now type: " ++ show newType ++ ")"

data DefinitionForm name o =
    DefUpToDate | DefDeleted | DefTypeChanged (DefinitionOutdatedType name (o EntityId))
    deriving (Show, Generic)

data BinderVarForm name o
    = GetDefinition (DefinitionForm name o)
    | GetLet
    deriving (Show, Generic)

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
instance Show name => Show (BinderVarRef name o) where
    show (BinderVarRef nameRef form _ _) = "(BinderVar " ++ show nameRef ++ " (form=" ++ show form ++ "))"

newtype ParamsRecordVarRef name = ParamsRecordVarRef
    { _prvFieldNames :: [name]
    } deriving (Eq, Ord, Functor, Foldable, Traversable, Generic)

data GetVar name o
    = GetParam (ParamRef name o)
    | GetParamsRecord (ParamsRecordVarRef name)
    | GetBinder (BinderVarRef name o)
    deriving Generic

Lens.makeLenses ''BinderVarRef
Lens.makeLenses ''DefinitionOutdatedType
Lens.makeLenses ''NameRef
Lens.makeLenses ''ParamRef
Lens.makeLenses ''ParamsRecordVarRef
Lens.makePrisms ''BinderVarForm
Lens.makePrisms ''BinderVarInline
Lens.makePrisms ''DefinitionForm
Lens.makePrisms ''GetVar
