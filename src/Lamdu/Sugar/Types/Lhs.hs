{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Lhs
    ( VarInfo(..), _VarNominal, _VarGeneric, _VarFunction, _VarRecord, _VarVariant
    , LhsNames(..), _LhsVar, _LhsRecord
    , FuncParam(..), fpAnnotation, fpUsages, fpVarInfo
    , Var(..), vTag, vAddPrev, vAddNext, vDelete, vIsNullParam, vParam
    , LhsField(..), fParam, fSubFields
    , AddParam(..), _AddNext, _NeedToPickTagToAddNext
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Internal.EntityId (EntityId)
import           Lamdu.Sugar.Types.Tag
import           Lamdu.Sugar.Types.TaggedList (TaggedList)
import qualified Lamdu.Sugar.Types.Type as SugarType

import           Lamdu.Prelude

data AddParam name i o
    = AddNext (i (TagChoice name o))
    | -- When the param has anon tag one can't add another one,
      -- contains the EntityId of the param requiring tag.
      NeedToPickTagToAddNext EntityId
    deriving Generic

data FuncParam v = FuncParam -- TODO: Better name
    { _fpAnnotation :: v
    , _fpUsages :: [EntityId]
    , _fpVarInfo :: VarInfo
    } deriving (Generic, Functor, Foldable, Traversable)

data Var name i o v = Var
    { _vParam :: FuncParam v
    , _vTag :: OptionalTag name i o
    , _vAddPrev :: AddParam name i o
    , _vAddNext :: AddParam name i o
    , _vDelete :: o ()
    , _vIsNullParam :: Bool
    } deriving (Generic, Functor, Foldable, Traversable)

-- TODO: Is there a standard term for this?
data LhsField name v = LhsField
    { _fParam :: FuncParam v
    , _fSubFields :: Maybe [(Tag name, LhsField name v)]
    } deriving (Generic, Functor, Foldable, Traversable)

data LhsNames name i o v
    = LhsVar (Var name i o v)
    | LhsRecord (TaggedList name i o (LhsField name v))
    deriving (Generic, Functor, Foldable, Traversable)

-- VarInfo is used for:
-- * Differentiating Mut actions so UI can suggest executing them
-- * Name pass giving parameters names according to types
data VarInfo
    = VarNominal (SugarType.TId T.Tag)
    | VarGeneric | VarFunction | VarRecord | VarUnit | VarVariant | VarVoid
    deriving (Generic, Eq)

traverse Lens.makeLenses [''FuncParam, ''LhsField, ''Var] <&> concat
traverse Lens.makePrisms [''AddParam, ''LhsNames, ''VarInfo] <&> concat
