{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Trans.Random(RandomT, nextRandom, runRandomT) where

import Control.Applicative (Applicative)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT(..), evalStateT)
import System.Random (Random, RandomGen)
import qualified System.Random as Random

newtype RandomT g m a = RandomT
  { unRandomT :: StateT g m a
  } deriving (Functor, Applicative, Monad, MonadTrans)

nextRandom :: (Monad m, RandomGen g, Random a) => RandomT g m a
nextRandom = RandomT $ StateT (return . Random.random)

runRandomT :: Monad m => g -> RandomT g m a -> m a
runRandomT g = (`evalStateT` g) . unRandomT
