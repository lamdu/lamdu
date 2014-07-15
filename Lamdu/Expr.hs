{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, RankNTypes, NoMonomorphismRestriction #-}
module Lamdu.Expr
  ( VariableRef(..), _ParameterRef, _DefinitionRef
  , Kind(..), _KVal, _KType
  , Lam(..), lamKind, lamParamId, lamParamType, lamResult
  , Apply(..), applyFunc, applyArg
  , Tag(..), tag
  , GetField(..), getFieldRecord, getFieldTag
  , Record(..), recordKind, recordFields
  , Leaf(..), _VVar, _VLiteralInteger, _VHole, _Type, _IntegerType
  , Body(..), _VAbs, _VApp, _VLeaf, _VRec, _VGetField
  , BodyExpr
  , Expr(..), eBody, ePayload
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.DeepSeq (NFData(..))
import Data.Binary (Binary(..))
import Data.Binary.Get (getWord8)
import Data.Binary.Put (putWord8)
import Data.Derive.Binary (makeBinary)
import Data.Derive.NFData (makeNFData)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable(..))
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable)
import Data.Typeable (Typeable)
import qualified Control.Lens as Lens

data Kind = KVal | KType
  deriving (Eq, Ord, Show, Typeable)

data Lam par expr = Lam
  { _lamKind :: !Kind
  , _lamParamId :: par
  , _lamParamType :: expr
  , _lamResult :: expr
  } deriving (Eq, Ord, Functor, Foldable, Traversable)

data Apply expr = Apply
  { _applyFunc :: expr
  , _applyArg :: expr
  } deriving (Eq, Ord, Functor, Foldable, Traversable, Show)

data VariableRef def par
  = ParameterRef par -- of the lambda/pi
  | DefinitionRef def
  deriving (Eq, Ord, Functor, Foldable, Traversable, Typeable)

instance (Show def, Show par) => Show (VariableRef def par) where
  showsPrec _ (ParameterRef paramId) = shows paramId
  showsPrec _ (DefinitionRef defI) = shows defI

-- TODO: Consider extracting all but "VVar" to "Primitive"?
data Leaf def par
  = VVar !(VariableRef def par)
  | VLiteralInteger !Integer
  | Type
  | IntegerType
  | VHole
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance (Show def, Show par) => Show (Leaf def par) where
  showsPrec _ leaf =
    case leaf of
    VVar varRef -> shows varRef
    VLiteralInteger int -> shows int
    Type -> showString "Type"
    IntegerType -> showString "Int"
    VHole -> showString "?"

newtype Tag = Tag Guid
  deriving (Eq, Ord, Show)

data Record expr = Record
  { _recordKind :: !Kind
  , _recordFields :: [(Tag, expr)]
  } deriving (Eq, Ord, Functor, Foldable, Traversable)

data GetField expr = GetField
  { _getFieldRecord :: expr
  , _getFieldTag :: Tag
  } deriving (Eq, Ord, Functor, Foldable, Traversable)

data Body def par expr
  = VAbs {-# UNPACK #-}!(Lam par expr)
  | VApp {-# UNPACK #-}!(Apply expr)
  | VRec {-# UNPACK #-}!(Record expr)
  | VGetField {-# UNPACK #-}!(GetField expr)
  | VLeaf !(Leaf def par)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

type BodyExpr def par a = Body def par (Expr def par a)

data Expr def par a = Expr
  { _eBody :: Body def par (Expr def par a)
  , _ePayload :: a
  } deriving (Functor, Eq, Ord, Foldable, Traversable, Typeable)

fmap concat $ mapM Lens.makePrisms [''Kind, ''VariableRef, ''Leaf, ''Body]
fmap concat $ mapM Lens.makeLenses [''Expr, ''Record, ''GetField, ''Lam, ''Apply]
Lens.makeIso ''Tag

fmap concat . sequence $
  derive
  <$> [makeBinary, makeNFData]
  <*> [ ''Kind, ''VariableRef, ''Lam, ''Apply, ''Leaf, ''Body
      , ''Record, ''GetField, ''Expr , ''Tag
      ]
