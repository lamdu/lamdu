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

data NameRef name am = NameRef
    { _nrName :: name
    , _nrGotoDefinition :: am EntityId
    }
instance Show name => Show (NameRef name am) where
    show (NameRef name _) = show name

data ParamRef name am = ParamRef
    { _pNameRef :: NameRef name am
    , _pBinderMode :: BinderMode
    }

data DefinitionOutdatedType name a = DefinitionOutdatedType
    { _defTypeWhenUsed :: Scheme name
    , _defTypeCurrent :: Scheme name
    , _defTypeUseCurrent :: a
    } deriving (Functor, Foldable, Traversable)
instance Show name => Show (DefinitionOutdatedType name a) where
    show (DefinitionOutdatedType usedType newType _) =
        "(Used @type: " ++ show usedType ++ " now type: " ++ show newType ++ ")"

data DefinitionForm name am =
    DefUpToDate | DefDeleted | DefTypeChanged (DefinitionOutdatedType name (am EntityId))
    deriving Show

data BinderVarForm name am = GetDefinition (DefinitionForm name am) | GetLet deriving Show

data BinderVarInline am
    = InlineVar (am EntityId)
    | CannotInlineDueToUses [EntityId]
    | CannotInline

data BinderVarRef name am = BinderVarRef
    { _bvNameRef :: NameRef name am
    , _bvForm :: BinderVarForm name am
    , _bvVar :: V.Var
    , -- Just means it is stored and inlinable:
      _bvInline :: BinderVarInline am
    }
instance Show name => Show (BinderVarRef name am) where
    show (BinderVarRef nameRef form _ _) = "(BinderVar " ++ show nameRef ++ " (form=" ++ show form ++ "))"

newtype ParamsRecordVarRef name = ParamsRecordVarRef
    { _prvFieldNames :: [name]
    } deriving (Eq, Ord, Functor, Foldable, Traversable)

data GetVar name am
    = GetParam (ParamRef name am)
    | GetParamsRecord (ParamsRecordVarRef name)
    | GetBinder (BinderVarRef name am)

Lens.makeLenses ''BinderVarRef
Lens.makeLenses ''DefinitionOutdatedType
Lens.makeLenses ''NameRef
Lens.makeLenses ''ParamRef
Lens.makeLenses ''ParamsRecordVarRef
Lens.makePrisms ''BinderVarForm
Lens.makePrisms ''BinderVarInline
Lens.makePrisms ''DefinitionForm
Lens.makePrisms ''GetVar
