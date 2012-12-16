{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Lamdu.Data.Infer.Types
  ( InferNode(..), Inferred(..)
  , IsRestrictedPoly(..)
  , Origin, mkOrigin
  , ExprRef(..)
  , RefExpression, makeRefExpr
  , RefExprPayload(..), rplOrigin, rplSubstitutedArgs, rplRestrictedPoly
  , Scope, TypedValue(..)
  ) where

import Control.Applicative (Applicative(..), (<*), (<$>), (<*>))
import Control.Monad.Trans.State (State)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Foldable (sequenceA_)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Monoid.Instances ()
import Data.Store.Guid (Guid)
import Data.Typeable (Typeable)
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.State as State
import qualified Data.Monoid as Monoid
import qualified Lamdu.Data as Data

newtype ExprRef = ExprRef { unExprRef :: Int } deriving (Eq, Ord)
derive makeBinary ''ExprRef
instance Show ExprRef where
  show = ('E' :) . show . unExprRef

data TypedValue = TypedValue
  { tvVal :: ExprRef
  , tvType :: ExprRef
  }
derive makeBinary ''TypedValue
instance Show TypedValue where
  show (TypedValue v t) = unwords [show v, ":", show t]

-- Not a newtype so that we can easily use IntSet/IntMap/etc.
-- This is used to detect type cycles (infinite types)
type Origin = Int

mkOrigin :: State Origin Origin
mkOrigin = State.get <* State.modify (+1)

data RefExprPayload = RefExprPayload
  { _rplSubstitutedArgs :: IntSet
  , _rplRestrictedPoly :: Monoid.Any
  , _rplOrigin :: Origin
  } deriving (Show)
derive makeBinary ''RefExprPayload
LensTH.makeLenses ''RefExprPayload

type RefExpression def = Data.Expression def RefExprPayload

makeRefExpr :: Origin -> Data.ExpressionBody def (RefExpression def) -> RefExpression def
makeRefExpr g expr = Data.Expression expr $ RefExprPayload mempty (Monoid.Any False) g

-- Map from params to their Param type,
-- also including the recursive ref to the definition.
-- (hence not just parameters)
-- TODO: Convert to list
type Scope def = Map (Data.VariableRef def) ExprRef

-- Used to refer to expressions in the inference state and resume inference.
data InferNode def = InferNode
  { nRefs :: TypedValue
  , nScope :: Scope def
  } deriving (Typeable)

-- "derive makeBinary ''InferNode" fails because of the Ord constraint

instance (Ord def, Binary def) => Binary (InferNode def) where
  get = InferNode <$> get <*> get
  put (InferNode a b) = sequenceA_ [put a, put b]

data IsRestrictedPoly = UnrestrictedPoly | RestrictedPoly
  deriving (Eq, Ord, Show, Typeable)
derive makeBinary ''IsRestrictedPoly

data Inferred def = Inferred
  { iPoint :: InferNode def
  , iValue :: Data.Expression def IsRestrictedPoly
  , iType :: Data.Expression def IsRestrictedPoly
  , iScope :: Map Guid (Data.Expression def IsRestrictedPoly)
  }
-- Cannot derive Binary instance because binary instance of Scope
-- requires (Ord def)
instance (Ord def, Binary def) => Binary (Inferred def) where
  get = Inferred <$> get <*> get <*> get <*> get
  put (Inferred a b c d) = sequenceA_ [put a, put b, put c, put d]
