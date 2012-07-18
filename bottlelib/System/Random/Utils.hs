module System.Random.Utils
  ( splits
  ) where

import System.Random (RandomGen, split)

splits :: RandomGen g => g -> [g]
splits = map fst . iterate (split . snd) . split
