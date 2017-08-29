{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.CharClassification
    ( operatorChars, bracketChars, digitChars, hexDigitChars, charPrecedence
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map

import Lamdu.Prelude

operatorChars :: String
operatorChars = "\\+-*/^=><&|%$:.!;#?@~≥≤≠⋲"

bracketChars :: String
bracketChars = "()[]{}"

digitChars :: String
digitChars = ['0'..'9']

hexDigitChars :: String
hexDigitChars = ['a'..'f'] ++ ['A' .. 'F'] ++ digitChars

charPrecedence :: Char -> Int
charPrecedence c =
    case precedenceMap ^. Lens.at c of
    Just p -> p
    Nothing
        | c `elem` operatorChars -> 5
        | otherwise -> 20

-- Based on Table 2 in https://www.haskell.org/onlinereport/decls.html
precedenceMap :: Map Char Int
precedenceMap =
    zipWith zip
    [ "$" -- 0
    , ";" -- 1 ";" stands for monadic bind (">>=" in Haskell)
    , "|" -- 2
    , "&" -- 3
    , "=><≠≥≤" -- 4
    , "." -- 5
    , "+-" -- 6
    , "*/%" -- 7
    , "^" -- 8
    , "!" -- 9
    ]
    ([0..] <&> repeat)
    & concat
    & Map.fromList
