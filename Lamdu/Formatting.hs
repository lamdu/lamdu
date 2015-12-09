module Lamdu.Formatting
    ( formatNum
    ) where

import Text.Printf (printf)

formatNum :: Double -> String
formatNum x
    | fromIntegral i /= x = printf "%f" x
    | isInfinite x = ['-' | x < 0] ++ "Inf"
    | otherwise = show i
    where
        i :: Integer
        i = truncate x
