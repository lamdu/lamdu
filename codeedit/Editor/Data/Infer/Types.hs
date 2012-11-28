{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Editor.Data.Infer.Types
  ( InferNode(..)
  , Origin, mkOrigin
  , ExprRef(..)
  , RefExpression, makeRefExpr
  , RefExprPayload(..), rplOrigin, rplSubstitutedArgs
  , Scope, TypedValue(..)
  ) where

import Control.Applicative (Applicative(..), (<*))
import Control.Monad.Trans.State (State)
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.State as State
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef

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
  , _rplOrigin :: Origin
  } deriving (Show)
derive makeBinary ''RefExprPayload
LensTH.makeLenses ''RefExprPayload

type RefExpression = Data.Expression DataIRef.DefinitionIRef RefExprPayload

makeRefExpr :: Origin -> Data.ExpressionBody DataIRef.DefinitionIRef RefExpression -> RefExpression
makeRefExpr g expr = Data.Expression expr $ RefExprPayload mempty g

-- Map from params to their Param type,
-- also including the recursive ref to the definition.
-- (hence not just parameters)
type Scope = Map (Data.VariableRef DataIRef.DefinitionIRef) ExprRef

-- Used to refer to expressions in the inference state and resume inference.
data InferNode = InferNode
  { nRefs :: TypedValue
  , nScope :: Scope
  } deriving (Typeable)
derive makeBinary ''InferNode
