-- TODO: Move to bottlelib, and put fork of IntDisjointSet in bottlelib
{-# LANGUAGE TemplateHaskell #-}
module Data.UnionFind
  ( UnionFind
  , freshRef, lookup, union, equivalent
  , empty
  , Ref, RefMap, RefSet
  , unmaintainedRefMapLookup
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT(..), state)
import Control.MonadA (MonadA)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Maybe.Utils (unsafeUnjust)
import Prelude hiding (lookup)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.IntDisjointSet as IDS
import qualified Data.IntMap as IntMap

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

unmaintainedRefMapLookup ::
  MonadA m => Ref -> StateT (RefMap a) (StateT UnionFind m) (Maybe a)
unmaintainedRefMapLookup ref = do
  tryNow ref $ do
    rep <- lift $ lookup "unmaintainedRefMapLookup.ref" ref
    tryNow rep $ do
      oldMap <- State.get
      newMap <- lift $ normalizeMap oldMap
      State.put newMap
      tryNow rep $ return Nothing
  where
    normalizeMap oldMap =
      oldMap
      & IntMap.toList
      & (Lens.traverse . Lens._1 %%~ lookup "unmaintainedRefMapLookup.mapKey")
      <&> IntMap.fromList
    tryNow r notFound = do
      mFound <- State.gets (IntMap.lookup r)
      case mFound of
        Just found -> return $ Just found
        Nothing -> notFound

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
