module Data.Store.Map
  ( mapStore, runEmpty
  ) where

import Control.Lens ((%~), (^.), (.=))
import Control.Monad.Trans.State (State, runState, state)
import Data.Map (Map)
import Data.Monoid (mempty)
import Data.Store.Rev.Change (Key, Value)
import Data.Store.Transaction (Store(..))
import System.Random (RandomGen, random)
import qualified Control.Lens as Lens
import qualified System.Random as Random

mapStore :: RandomGen g => Store (State (Map Key Value, g))
mapStore = Store
  { storeNewKey = Lens.zoom Lens._2 $ state random
  , storeLookup = sLookup
  , storeAtomicWrite = mapM_ sWrite
  }
  where
    sLookup key = Lens.use (Lens._1 . Lens.at key)
    sWrite (key, mValue) = Lens._1 . Lens.at key .= mValue

runEmpty :: State (Map Key Value, Random.StdGen) a -> (a, Map Key Value)
runEmpty = (Lens._2 %~ (^. Lens._1)) . (`runState` (mempty, Random.mkStdGen 0))
