-- This is currently unused code, but will be useful for a purely
-- functional data-store (for testing/etc)

{-# LANGUAGE NoImplicitPrelude #-}
module Data.Store.Map
    ( mapStore, runEmpty
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Trans.State (State, runState, state)
import           Data.Store.Rev.Change (Key, Value)
import           Data.Store.Transaction (Store(..))
import           System.Random (RandomGen, random)
import qualified System.Random as Random

import           Lamdu.Prelude

mapStore :: RandomGen g => Store (State (Map Key Value, g))
mapStore = Store
    { storeNewKey = Lens.zoom _2 $ state random
    , storeLookup = sLookup
    , storeAtomicWrite = mapM_ sWrite
    }
    where
        sLookup key = Lens.use (_1 . Lens.at key)
        sWrite (key, mValue) = _1 . Lens.at key .= mValue

runEmpty :: State (Map Key Value, Random.StdGen) a -> (a, Map Key Value)
runEmpty = (_2 %~ (^. _1)) . (`runState` (mempty, Random.mkStdGen 0))
