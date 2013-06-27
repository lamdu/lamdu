module System.Random.Utils
  ( splits, randFunc, genFromHashable
  ) where

import Data.Hashable (Hashable, hashWithSalt)
import System.Random (RandomGen, Random, StdGen, split, mkStdGen, random)

splits :: RandomGen g => g -> [g]
splits = map fst . iterate (split . snd) . split

randFunc :: (Hashable h, Random r) => h -> r
randFunc = fst . random . genFromHashable

genFromHashable :: Hashable a => a -> StdGen
genFromHashable = mkStdGen . hashWithSalt 0
