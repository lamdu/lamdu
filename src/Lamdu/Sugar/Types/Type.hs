-- | Sugaring of Lamdu.Calc.Type modules/ASTs
{-# LANGUAGE TemplateHaskell, TypeFamilies, GADTs, StandaloneDeriving #-}
module Lamdu.Sugar.Types.Type
    ( Scheme(..), schemeForAll, schemeType
    , T.NominalId
    , CompositeFields(..), compositeFields, compositeExtension
    , Type(..), _TVar, _TFun, _TInst, _TRecord, _TVariant
    , TId(..), tidName, tidTId
    ) where

import qualified Control.Lens as Lens
import           Hyper (makeHTraversableAndBases)
import           Hyper.Syntax (FuncType)
import           Hyper.Syntax.Scheme (QVars)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.EntityId (EntityId)
import           Lamdu.Sugar.Types.Tag (Tag)

import           Lamdu.Prelude

data CompositeFields name k = CompositeFields
    { _compositeFields :: [(Tag name, k :# Type name)]
    , _compositeExtension :: Maybe name -- TyVar of more possible fields
    } deriving Generic

data TId name = TId
    { _tidName :: name
    , _tidTId :: T.NominalId
    } deriving (Generic, Eq)

data Type name k
    = TVar name
      -- ^ A type variable
    | TFun (FuncType (Type name) k)
      -- ^ A (non-dependent) function of the given parameter and result types
    | TInst (TId name) [(Tag name, k :# Type name)]
      -- ^ An instantiation of a nominal type of the given id with the
      -- given keyword type arguments
    | TRecord (CompositeFields name k)
      -- ^ Lifts a composite record type
    | TVariant (CompositeFields name k)
      -- ^ Lifts a composite variant type
    deriving Generic

data Scheme name = Scheme
    { _schemeForAll :: T.Types # QVars
    , _schemeType :: Annotated EntityId # Type name
    } deriving Generic

traverse Lens.makeLenses [''CompositeFields, ''Scheme, ''TId] <&> concat
Lens.makePrisms ''Type
traverse makeHTraversableAndBases [''Type, ''CompositeFields] <&> concat
