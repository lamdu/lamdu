{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Trans.UnionFind
  ( UnionFindT, runUnionFindT
  , Point
  , new, findRepr
  , descr, setDescr
  , union, unionWith
  , equivalent
  ) where

import Control.Applicative (Applicative)
import Control.Monad (unless)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Data.UnionFind.IntMap (Point)
import qualified Control.Monad.Trans.State as State
import qualified Data.UnionFind.IntMap as UF

newtype UnionFindT p m a = UnionFindT {
  unUnionFindT :: StateT (UF.PointSupply p) m a
  } deriving (Functor, Applicative, Monad, MonadTrans)

runUnionFindT :: Monad m => UnionFindT p m a -> m a
runUnionFindT = (`evalStateT` UF.newPointSupply) . unUnionFindT

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

new :: Monad m => p -> UnionFindT p m (Point p)
new x = UnionFindT . StateT $ return . swap . flip UF.fresh x

findRepr :: Monad m => Point p -> UnionFindT p m (Point p)
findRepr = UnionFindT . State.gets . flip UF.repr

descr :: Monad m => Point p -> UnionFindT p m p
descr = UnionFindT . State.gets . flip UF.descr

setDescr :: Monad m => Point p -> p -> UnionFindT p m ()
setDescr p val = UnionFindT . State.modify $ \ps -> UF.setDescr ps p val

union :: Monad m => Point p -> Point p -> UnionFindT p m ()
union p1 p2 = UnionFindT . State.modify $ \x -> UF.union x p1 p2

-- | Convenience function to handle a potential unification
unionWith
  :: Monad m => (p -> p -> p) -> Point p -> Point p -> UnionFindT p m ()
unionWith f p1 p2 = do
  isEq <- equivalent p1 p2
  unless isEq $ do
    v1 <- descr p1
    v2 <- descr p2
    union p1 p2
    setDescr p1 $ f v1 v2

-- | findRepr x == findRepr y
equivalent :: Monad m => Point p -> Point p -> UnionFindT p m Bool
equivalent p1 p2 = UnionFindT . State.gets $ \x -> UF.equivalent x p1 p2
