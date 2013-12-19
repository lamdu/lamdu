{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, RankNTypes, NoMonomorphismRestriction #-}
module Lamdu.Data.Expr
  ( VariableRef(..), _ParameterRef, _DefinitionRef
  , Kind(..), _KVal, _KType
  , Lam(..), lamKind, lamParamId, lamParamType, lamResult
  , Apply(..), applyFunc, applyArg
  , GetField(..), getFieldRecord, getFieldTag
  , Record(..), recordKind, recordFields
  , Leaf(..), _GetVariable, _LiteralInteger, _Hole, _Type, _IntegerType, _Tag, _TagType
  , Body(..), _BodyLam, _BodyApply, _BodyLeaf, _BodyRecord, _BodyGetField
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

data Lam expr = Lam
  { _lamKind :: !Kind
  , _lamParamId :: {-# UNPACK #-}!Guid
  , _lamParamType :: expr
  , _lamResult :: expr
  } deriving (Eq, Ord, Functor, Foldable, Traversable)

data Apply expr = Apply
  { _applyFunc :: expr
  , _applyArg :: expr
  } deriving (Eq, Ord, Functor, Foldable, Traversable, Show)

data VariableRef def
  = ParameterRef {-# UNPACK #-} !Guid -- of the lambda/pi
  | DefinitionRef def
  deriving (Eq, Ord, Functor, Foldable, Traversable, Typeable)

instance Show def => Show (VariableRef def) where
  showsPrec _ (ParameterRef paramId) = shows paramId
  showsPrec _ (DefinitionRef defI) = shows defI

data Leaf def
  = GetVariable !(VariableRef def)
  | LiteralInteger !Integer
  | Type
  | IntegerType
  | Hole
  | TagType
  | Tag Guid
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show def => Show (Leaf def) where
  showsPrec _ leaf =
    case leaf of
    GetVariable varRef -> shows varRef
    LiteralInteger int -> shows int
    Tag guid -> showString "Tag<" . shows guid . showChar '>'
    Type -> showString "Type"
    IntegerType -> showString "Int"
    Hole -> showString "?"
    TagType -> showString "Tag"

data Record expr = Record
  { _recordKind :: !Kind
  , _recordFields :: [(expr, expr)]
  } deriving (Eq, Ord, Functor, Foldable, Traversable)

data GetField expr = GetField
  { _getFieldRecord :: expr
  , _getFieldTag :: expr
  } deriving (Eq, Ord, Functor, Foldable, Traversable)

data Body def expr
  = BodyLam {-# UNPACK #-}!(Lam expr)
  | BodyApply {-# UNPACK #-}!(Apply expr)
  | BodyRecord {-# UNPACK #-}!(Record expr)
  | BodyGetField {-# UNPACK #-}!(GetField expr)
  | BodyLeaf !(Leaf def)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

type BodyExpr def a = Body def (Expr def a)

data Expr def a = Expr
  { _eBody :: Body def (Expr def a)
  , _ePayload :: a
  } deriving (Functor, Eq, Ord, Foldable, Traversable, Typeable)

fmap concat $ mapM Lens.makePrisms [''Kind, ''VariableRef, ''Leaf, ''Body]
fmap concat $ mapM Lens.makeLenses [''Expr, ''Record, ''GetField, ''Lam, ''Apply]

fmap concat . sequence $
  derive
  <$> [makeBinary, makeNFData]
  <*> [ ''Kind, ''VariableRef, ''Lam, ''Apply, ''Leaf, ''Body, ''Record, ''GetField, ''Expr ]
