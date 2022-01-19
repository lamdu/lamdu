{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.GetVar
    ( VarForm(..), _GetDefinition, _GetNormalVar, _GetLightParam
    , DefinitionForm(..), _DefUpToDate, _DefDeleted, _DefTypeChanged
    , DefinitionOutdatedType(..), defTypeWhenUsed, defTypeCurrent, defTypeUseCurrent
    , VarInline(..), _InlineVar, _CannotInlineDueToUses, _CannotInline
    , GetVar(..), vNameRef, vForm, vVar, vInline
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Unit (Unit)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Type
import           Lamdu.Sugar.Types.NameRef (NameRef)

import           Lamdu.Prelude

data DefinitionOutdatedType name o a = DefinitionOutdatedType
    { _defTypeWhenUsed :: Scheme name Unit
    , _defTypeCurrent :: Scheme name Unit
    , _defTypeUseCurrent :: o a
    } deriving (Functor, Foldable, Traversable, Generic)

data DefinitionForm name o
    = DefUpToDate
    | DefDeleted
    | DefTypeChanged (DefinitionOutdatedType name o EntityId)
    deriving (Generic)

data VarForm name o
    = GetDefinition (DefinitionForm name o)
    | GetNormalVar
    | GetLightParam
    deriving (Generic)

data VarInline o
    = InlineVar (o EntityId)
    | CannotInlineDueToUses [EntityId]
    | CannotInline
    deriving Generic

data GetVar name o = GetVar
    { _vNameRef :: NameRef name o
    , _vForm :: VarForm name o
    , _vVar :: V.Var
    , -- Just means it is stored and inlinable:
      _vInline :: VarInline o
    } deriving Generic

traverse Lens.makeLenses
    [''GetVar, ''DefinitionOutdatedType] <&> concat
traverse Lens.makePrisms
    [''VarForm, ''VarInline, ''DefinitionForm] <&> concat
