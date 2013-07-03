{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Lamdu.Data.Expression.Infer.Types
  ( Node(..), Inferred(..)
  , IsRestrictedPoly(..)
  , ExprRef(..)
  , RefExpression, makeRefExpr
  , RefExprPayload(..), rplOrigins, rplSubstitutedArgs, rplRestrictedPoly
  , Scope, TypedValue(..)
  ) where

import Control.Applicative (Applicative(..), (<$>), (<*>))
import Control.DeepSeq (NFData(..))
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.Derive.NFData (makeNFData)
import Data.DeriveTH (derive)
import Data.Foldable (sequenceA_)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Monoid.Instances ()
import Data.Store.Guid (Guid)
import Data.Typeable (Typeable)
import qualified Control.Lens as Lens
import qualified Data.Monoid as Monoid
import qualified Lamdu.Data.Expression as Expr

newtype ExprRef = ExprRef { unExprRef :: Int } deriving (Eq, Ord, Typeable)
instance Show ExprRef where
  show = ('E' :) . show . unExprRef

data TypedValue = TypedValue
  { tvVal :: {-# UNPACK #-}! ExprRef
  , tvType :: {-# UNPACK #-}! ExprRef
  } deriving (Eq, Ord)
instance Show TypedValue where
  show (TypedValue v t) = unwords [show v, ":", show t]

data RefExprPayload = RefExprPayload
  { _rplSubstitutedArgs :: !IntSet
  , _rplRestrictedPoly :: !Monoid.Any
  , _rplOrigins :: !IntSet
  } deriving (Show, Eq, Ord)
Lens.makeLenses ''RefExprPayload

type RefExpression def = Expr.Expression def RefExprPayload

makeRefExpr :: Expr.Body def (RefExpression def) -> RefExpression def
makeRefExpr expr = Expr.Expression expr $ RefExprPayload mempty (Monoid.Any False) mempty

-- Map from params to their Param type,
-- also including the recursive ref to the definition.
-- (hence not just parameters)
-- TODO: Convert to list
type Scope def = Map (Expr.VariableRef def) ExprRef

-- Used to refer to expressions in the inference state and resume inference.
data Node def = Node
  { nRefs :: {-# UNPACK #-}!TypedValue
  , nScope :: Scope def
  } deriving (Typeable, Eq, Ord)

-- "derive makeBinary ''Node" fails because of the Ord constraint
instance (Ord def, Binary def) => Binary (Node def) where
  get = Node <$> get <*> get
  put (Node a b) = sequenceA_ [put a, put b]
derive makeNFData ''Node

data IsRestrictedPoly = UnrestrictedPoly | RestrictedPoly
  deriving (Eq, Ord, Show, Typeable)

data Inferred def = Inferred
  { iNode :: Node def
  , iValue :: Expr.Expression def IsRestrictedPoly
  , iType  :: Expr.Expression def IsRestrictedPoly
  , iScope :: Map Guid (Expr.Expression def IsRestrictedPoly)
  } deriving (Typeable)
-- Cannot derive Binary instance because binary instance of Scope
-- requires (Ord def)
instance (Ord def, Binary def) => Binary (Inferred def) where
  get = Inferred <$> get <*> get <*> get <*> get
  put (Inferred a b c d) = sequenceA_ [put a, put b, put c, put d]
derive makeNFData ''Inferred

fmap concat . sequence $
  derive
  <$> [makeBinary, makeNFData]
  <*> [''IsRestrictedPoly, ''ExprRef, ''TypedValue, ''RefExprPayload]
