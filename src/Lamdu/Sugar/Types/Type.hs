-- | Sugaring of Lamdu.Calc.Type modules/ASTs
{-# LANGUAGE TemplateHaskell, TypeFamilies, GADTs, StandaloneDeriving #-}
module Lamdu.Sugar.Types.Type
    ( Scheme(..), schemeForAll, schemeType
    , T.NominalId
    , CompositeFields(..), compositeFields, compositeExtension
    , Type(..), _TVar, _TFun, _TInst, _TRecord, _TVariant
    , TId(..), tidName, tidTId, tidGotoDefinition
    ) where

import qualified Control.Lens as Lens
import           Hyper (makeHTraversableAndBases)
import           Hyper.Syntax (FuncType)
import           Hyper.Syntax.Scheme (QVars)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.EntityId (EntityId)
import           Lamdu.Sugar.Types.Tag (Tag)

import           Lamdu.Prelude

-- TODO: Will making this a knot will not require `UndecidableInstances`?
data CompositeFields name a = CompositeFields
    { _compositeFields :: [(Tag name, a)]
    , _compositeExtension :: Maybe name -- TyVar of more possible fields
    } deriving (Functor, Foldable, Traversable, Generic)

data TId name o = TId
    { _tidName :: name
    , _tidTId :: T.NominalId
    , _tidGotoDefinition :: o EntityId
    } deriving Generic

deriving instance (Eq name, Eq (o EntityId)) => Eq (TId name o)

data Type name o k
    = TVar name
      -- ^ A type variable
    | TFun (FuncType (Type name o) k)
      -- ^ A (non-dependent) function of the given parameter and result types
    | TInst (TId name o) [(name, k :# Type name o)]
      -- ^ An instantiation of a nominal type of the given id with the
      -- given keyword type arguments
    | TRecord (CompositeFields name (k :# Type name o))
      -- ^ Lifts a composite record type
    | TVariant (CompositeFields name (k :# Type name o))
      -- ^ Lifts a composite variant type
    deriving Generic

data Scheme name o = Scheme
    { _schemeForAll :: T.Types # QVars
    , _schemeType :: Annotated EntityId # Type name o
    } deriving Generic

Lens.makeLenses ''CompositeFields
Lens.makeLenses ''Scheme
Lens.makeLenses ''TId
Lens.makePrisms ''Type
makeHTraversableAndBases ''Type
