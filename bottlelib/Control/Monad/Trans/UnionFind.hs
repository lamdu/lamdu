{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Trans.UnionFind
  ( UnionFindT, runUnionFindT, evalUnionFindT, resumeUnionFindT
  , Point
  , new, findRepr
  , descr, setDescr
  , union, unionWith
  , equivalent
  ) where

import Control.Applicative (Applicative)
import Control.Monad (liftM, unless)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT(..), runStateT)
import Data.UnionFind.IntMap (Point)
import qualified Control.Monad.Trans.State as State
import qualified Data.UnionFind.IntMap as UF

newtype UnionFindT p m a = UnionFindT {
  unUnionFindT :: StateT (UF.UnionFind p) m a
  } deriving (Functor, Applicative, Monad, MonadTrans)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

runUnionFindT :: Monad m => UnionFindT p m a -> m (UF.UnionFind p, a)
runUnionFindT = resumeUnionFindT UF.newUnionFind

resumeUnionFindT :: Monad m => UF.UnionFind p -> UnionFindT p m a -> m (UF.UnionFind p, a)
resumeUnionFindT state = liftM swap . (`runStateT` state) . unUnionFindT

evalUnionFindT :: Monad m => UnionFindT p m a -> m a
evalUnionFindT = liftM snd . runUnionFindT

new :: Monad m => p -> UnionFindT p m (Point p)
new x = UnionFindT . StateT $ return . swap . flip UF.fresh x

findRepr :: Monad m => Point p -> UnionFindT p m (Point p)
findRepr = UnionFindT . State.gets . UF.repr

descr :: Monad m => Point p -> UnionFindT p m p
descr = UnionFindT . State.gets . UF.descr

setDescr :: Monad m => Point p -> p -> UnionFindT p m ()
setDescr p = UnionFindT . State.modify . UF.setDescr p

union :: Monad m => Point p -> Point p -> UnionFindT p m ()
union p = UnionFindT . State.modify . UF.union p

-- | Convenience function to handle a potential unification
unionWith
  :: Monad m => (p -> p -> p) -> Point p -> Point p -> UnionFindT p m ()
unionWith f p1 p2 = do
  isEq <- equivalent p1 p2
  unless isEq $ do
    v1 <- descr p1
    v2 <- descr p2
    p1 `union` p2
    setDescr p1 $ f v1 v2

-- | findRepr x == findRepr y
equivalent :: Monad m => Point p -> Point p -> UnionFindT p m Bool
equivalent p = UnionFindT . State.gets . UF.equivalent p
