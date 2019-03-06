-- | Sugaring of Lamdu.Calc.Type modules/ASTs
{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Sugar.Types.Type
    ( Scheme(..), schemeForAll, schemeType
    , T.NominalId
    , CompositeFields(..), compositeFields, compositeExtension
    , TBody(..), _TVar, _TFun, _TInst, _TRecord, _TVariant
    , Type(..), tPayload, tBody
    , TId(..), tidName, tidTId
    ) where

import           AST (Tree)
import           AST.Term.Scheme (QVars)
import qualified Control.Lens as Lens
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.EntityId (EntityId)
import           Lamdu.Sugar.Types.Tag (TagInfo)

import           Lamdu.Prelude

data CompositeFields name a = CompositeFields
    { _compositeFields :: [(TagInfo name, a)]
    , _compositeExtension :: Maybe name -- TyVar of more possible fields
    } deriving (Show, Functor, Foldable, Traversable, Generic)

data TId name = TId
    { _tidName :: name
    , _tidTId :: T.NominalId
    } deriving (Eq, Ord, Show, Generic)

data TBody name a
    = TVar name
      -- ^ A type variable
    | TFun a a
      -- ^ A (non-dependent) function of the given parameter and result types
    | TInst (TId name) [(name, a)]
      -- ^ An instantiation of a nominal type of the given id with the
      -- given keyword type arguments
    | TRecord (CompositeFields name a)
      -- ^ Lifts a composite record type
    | TVariant (CompositeFields name a)
      -- ^ Lifts a composite variant type
    deriving (Show, Functor, Foldable, Traversable, Generic)

data Type name = Type
    { _tPayload :: EntityId
    , _tBody :: TBody name (Type name)
    } deriving (Show, Generic)

data Scheme name = Scheme
    { _schemeForAll :: Tree T.Types QVars
    , _schemeType :: Type name
    } deriving (Show, Generic)

Lens.makeLenses ''CompositeFields
Lens.makeLenses ''Scheme
Lens.makeLenses ''TId
Lens.makeLenses ''Type
Lens.makePrisms ''TBody

