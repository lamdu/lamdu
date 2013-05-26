{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, RankNTypes, NoMonomorphismRestriction #-}
module Lamdu.Data.Expression
  ( VariableRef(..), _ParameterRef, _DefinitionRef
  , Kind(..), _Val, _Type
  , Lambda(..), lambdaKind, lambdaParamId, lambdaParamType, lambdaResult
  , Apply(..), applyFunc, applyArg
  , GetField(..), getFieldRecord, getFieldTag
  , Record(..), recordKind, recordFields
  , Leaf(..), _GetVariable, _LiteralInteger, _Hole, _Set, _IntegerType, _Tag, _TagType
  , Body(..), _BodyLam, _BodyApply, _BodyLeaf, _BodyRecord, _BodyGetField
  , BodyExpr
  , Expression(..), eBody, ePayload
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
import qualified Control.Lens.TH as LensTH

data Kind = Val | Type
  deriving (Eq, Ord, Show, Typeable)

data Lambda expr = Lambda
  { _lambdaKind :: Kind
  , _lambdaParamId :: {-# UNPACK #-}!Guid
  , _lambdaParamType :: expr
  -- TODO: Rename to _lambdaResult (for Pi it is not a body)
  , _lambdaResult :: expr
  } deriving (Eq, Ord, Functor, Foldable, Traversable)

data Apply expr = Apply
  { _applyFunc :: expr
  , _applyArg :: expr
  } deriving (Eq, Ord, Functor, Foldable, Traversable)

data VariableRef def
  = ParameterRef {-# UNPACK #-} !Guid -- of the lambda/pi
  | DefinitionRef def
  deriving (Eq, Ord, Functor, Foldable, Traversable, Typeable)

data Leaf def
  = GetVariable !(VariableRef def)
  | LiteralInteger !Integer
  | Set
  | IntegerType
  | Hole
  | TagType
  | Tag Guid
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show def => Show (Leaf def) where
  showsPrec _ leaf =
    case leaf of
    GetVariable (ParameterRef paramId) -> shows paramId
    GetVariable (DefinitionRef defI) -> shows defI
    LiteralInteger int -> shows int
    Tag guid -> showString "Tag<" . shows guid . showChar '>'
    Set -> showString "Set"
    IntegerType -> showString "Int"
    Hole -> showString "?"
    TagType -> showString "Tag"

data Record expr = Record
  { _recordKind :: Kind
  , _recordFields :: [(expr, expr)]
  } deriving (Eq, Ord, Functor, Foldable, Traversable)

data GetField expr = GetField
  { _getFieldRecord :: expr
  , _getFieldTag :: expr
  } deriving (Eq, Ord, Functor, Foldable, Traversable)

data Body def expr
  = BodyLam {-# UNPACK #-}!(Lambda expr)
  | BodyApply {-# UNPACK #-}!(Apply expr)
  | BodyRecord {-# UNPACK #-}!(Record expr)
  | BodyGetField {-# UNPACK #-}!(GetField expr)
  | BodyLeaf !(Leaf def)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

type BodyExpr def a = Body def (Expression def a)

-- TODO: Expression = Cofree, do we want to use that?
data Expression def a = Expression
  { _eBody :: Body def (Expression def a)
  , _ePayload :: a
  } deriving (Functor, Eq, Ord, Foldable, Traversable, Typeable)

fmap concat $ mapM LensTH.makePrisms [''Kind, ''VariableRef, ''Leaf, ''Body]
fmap concat $ mapM LensTH.makeLenses [''Expression, ''Record, ''GetField, ''Lambda, ''Apply]

fmap concat . sequence $
  derive
  <$> [makeBinary, makeNFData]
  <*> [ ''Kind, ''VariableRef, ''Lambda, ''Apply, ''Leaf, ''Body, ''Record, ''GetField
      , ''Expression
      ]
