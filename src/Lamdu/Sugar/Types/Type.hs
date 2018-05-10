-- | Sugaring of Lamdu.Calc.Type modules/ASTs
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Type
    ( Scheme(..), schemeForAll, schemeConstraints, schemeType
    , TypeVars(..), Constraints(..)
    , RecordTag, VariantTag
    , T.RecordVar, T.VariantVar, T.TypeVar, TVar
    , RecordType, VariantType
    , NominalId, ParamId
    , CompositeFields(..), compositeFields, compositeExtension
    , TBody(..), _TVar, _TFun, _TInst, _TRecord, _TVariant
    , Type(..), tPayload, tBody
    , TId(..), tidName, tidTId
    ) where

import qualified Control.Lens as Lens
import           Lamdu.Calc.Type (NominalId, RecordTag, VariantTag, ParamId)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Type.Constraints (Constraints(..))
import           Lamdu.Calc.Type.Vars (TypeVars(..))
import           Lamdu.Sugar.EntityId (EntityId)
import           Lamdu.Sugar.Types.Tag (TagInfo)

import           Lamdu.Prelude

type RecordType = CompositeFields RecordTag
type VariantType = CompositeFields VariantTag

type TVar = T.Var

type CompositeVar p = TVar (T.Composite p)

data CompositeFields p name a = CompositeFields
    { _compositeFields :: [(TagInfo name, a)]
    , _compositeExtension :: Maybe (CompositeVar p) -- TyVar of more possible fields
    } deriving (Show, Functor, Foldable, Traversable, Generic)

data TId name = TId
    { _tidName :: name
    , _tidTId :: NominalId
    } deriving (Eq, Ord, Show, Generic)

data TBody name a
    = TVar T.TypeVar
      -- ^ A type variable
    | TFun a a
      -- ^ A (non-dependent) function of the given parameter and result types
    | TInst (TId name) (Map ParamId a)
      -- ^ An instantiation of a nominal type of the given id with the
      -- given keyword type arguments
    | TRecord (RecordType name a)
      -- ^ Lifts a composite record type
    | TVariant (VariantType name a)
      -- ^ Lifts a composite variant type
    deriving (Show, Functor, Foldable, Traversable, Generic)

data Type name = Type
    { _tPayload :: EntityId
    , _tBody :: TBody name (Type name)
    } deriving (Show, Generic)

data Scheme name = Scheme
    { _schemeForAll :: TypeVars
    , _schemeConstraints :: Constraints
    , _schemeType :: Type name
    } deriving (Show, Generic)

Lens.makeLenses ''CompositeFields
Lens.makeLenses ''Scheme
Lens.makeLenses ''TId
Lens.makeLenses ''Type
Lens.makePrisms ''TBody

