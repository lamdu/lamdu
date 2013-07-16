-- TODO: Move to bottlelib, and put fork of IntDisjointSet in bottlelib
{-# LANGUAGE TemplateHaskell #-}
module Data.UnionFind
  ( UnionFind
  , freshRef, lookup, union, equivalent
  , empty
  , Ref, RefMap, RefSet
  ) where

import Prelude hiding (lookup)
import Data.IntSet (IntSet)
import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad.Trans.State (StateT(..), state)
import Control.MonadA (MonadA)
import Data.IntMap (IntMap)
import Data.Maybe.Utils (unsafeUnjust)
import qualified Control.Lens as Lens
import qualified Data.IntDisjointSet as IDS

-- TODO: newtype Ref a = Ref Int
type Ref = Int
-- TODO: newtype RefSet a = RefSet IntSet
type RefSet = IntSet
-- TODO: newtype RefMap a = RefMap IntMap
type RefMap = IntMap

-- TODO: UnionFind a
data UnionFind = UnionFind
  { _ufRefs :: IDS.IntDisjointSet
  , _ufNextRef :: Int
  }
Lens.makeLenses ''UnionFind

empty :: UnionFind
empty =
  UnionFind
  { _ufRefs = IDS.empty
  , _ufNextRef = 0
  }

ufState ::
  Monad m =>
  (IDS.IntDisjointSet -> (b, IDS.IntDisjointSet)) ->
  StateT UnionFind m b
ufState = Lens.zoom ufRefs . state

freshRef :: MonadA m => StateT UnionFind m Ref
freshRef = do
  ref <- Lens.use ufNextRef
  ufNextRef += 1
  ufRefs %= IDS.insert ref
  return ref

lookup :: MonadA m => String -> Ref -> StateT UnionFind m Ref
lookup msg ref = unsafeUnjust (msg ++ ": " ++ show ref) <$> ufState (IDS.lookup ref)

union :: MonadA m => Ref -> Ref -> StateT UnionFind m Ref
union x y = unsafeUnjust "union on invalid ref" <$> ufState (IDS.unionRep x y)

equivalent :: Monad m => Ref -> Ref -> StateT UnionFind m Bool
equivalent x y = ufState $ IDS.equivalent x y
