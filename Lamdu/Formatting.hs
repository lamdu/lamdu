module Lamdu.Formatting
    ( formatNum
    ) where

import Text.Printf (printf)

formatNum :: Double -> String
formatNum x
    | fromIntegral i == x = show i
    | otherwise = printf "%f" x
    where
        i :: Integer
        i = truncate x
