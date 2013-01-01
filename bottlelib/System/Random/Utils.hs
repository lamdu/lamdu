module System.Random.Utils
  ( splits, randFunc
  ) where

import Data.Hashable (Hashable, hashWithSalt)
import System.Random (RandomGen, Random, split, mkStdGen, random)

splits :: RandomGen g => g -> [g]
splits = map fst . iterate (split . snd) . split

randFunc :: (Hashable h, Random r) => h -> r
randFunc = fst . random . mkStdGen . hashWithSalt 0
