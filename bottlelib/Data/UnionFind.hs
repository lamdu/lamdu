{-# LANGUAGE TemplateHaskell #-}
module Data.UnionFind
  ( UnionFind
  , freshRef, lookup, union, equivalent
  , empty
  , unmaintainedRefMapLookup
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT(..), state)
import Control.MonadA (MonadA)
import Data.Function (on)
import Data.Maybe.Utils (unsafeUnjust)
import Data.OpaqueRef (Ref, RefMap)
import Prelude hiding (lookup)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.IntDisjointSet as IDS
import qualified Data.OpaqueRef as OpaqueRef

data UnionFind p = UnionFind
  { _ufRefs :: IDS.IntDisjointSet
  , _ufFresh :: OpaqueRef.Fresh p
  }
Lens.makeLenses ''UnionFind

unmaintainedRefMapLookup ::
  MonadA m => (String -> Ref p -> m (Ref p)) -> Ref p -> StateT (RefMap p a) m (Maybe a)
unmaintainedRefMapLookup doLookup ref =
  tryNow ref $ do
    rep <- lift $ doLookup "unmaintainedRefMapLookup.ref" ref
    tryNow rep $ do
      oldMap <- State.get
      newMap <- lift $ oldMap & OpaqueRef.unsafeRefMapItems . Lens._1 %%~ doLookup "unmaintainedRefMapLookup.mapKey"
      State.put newMap
      tryNow rep $ return Nothing
  where
    tryNow r notFound = do
      mFound <- State.gets (^. Lens.at r)
      case mFound of
        Just found -> return $ Just found
        Nothing -> notFound

empty :: UnionFind p
empty =
  UnionFind
  { _ufRefs = IDS.empty
  , _ufFresh = OpaqueRef.initialFresh
  }

ufState ::
  Monad m =>
  (IDS.IntDisjointSet -> (b, IDS.IntDisjointSet)) ->
  StateT (UnionFind p) m b
ufState = Lens.zoom ufRefs . state

freshRef :: MonadA m => StateT (UnionFind p) m (Ref p)
freshRef = do
  ref <- Lens.zoom ufFresh OpaqueRef.freshRef
  ufRefs %= IDS.insert (OpaqueRef.unsafeAsInt ref)
  return ref

lookup :: MonadA m => String -> Ref p -> StateT (UnionFind p) m (Ref p)
lookup msg ref =
  OpaqueRef.unsafeFromInt .
  unsafeUnjust (msg ++ ": " ++ show ref) <$>
  (ufState . IDS.lookup . OpaqueRef.unsafeAsInt) ref

union :: MonadA m => Ref p -> Ref p -> StateT (UnionFind p) m (Ref p)
union x y =
  OpaqueRef.unsafeFromInt .
  unsafeUnjust "union on invalid ref" <$> ufState (on IDS.unionRep OpaqueRef.unsafeAsInt x y)

equivalent :: Monad m => Ref p -> Ref p -> StateT (UnionFind p) m Bool
equivalent x y = ufState $ on IDS.equivalent OpaqueRef.unsafeAsInt x y
