{-# OPTIONS -Wall #-}
module Data.Maybe.Utils(unsafeUnjust) where

unsafeUnjust :: String -> Maybe a -> a
unsafeUnjust msg Nothing = error ("unsafeUnjust: " ++ msg)
unsafeUnjust _ (Just x) = x
