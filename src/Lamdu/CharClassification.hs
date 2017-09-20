{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.CharClassification
    ( operator, bracket, digit, hexDigit, charPrecedence
    , disallowedInHole
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map

import Lamdu.Prelude

disallowedInHole :: String
disallowedInHole = ",`\"\n "

operator :: String
operator = "\\+-*/^=><&|%$:.!;#?@~≥≤≠⋲"

bracket :: String
bracket = "()[]{}"

digit :: String
digit = ['0'..'9']

hexDigit :: String
hexDigit = ['a'..'f'] ++ ['A' .. 'F'] ++ digit

charPrecedence :: Char -> Int
charPrecedence c =
    case precedenceMap ^. Lens.at c of
    Just p -> p
    Nothing
        | c `elem` operator -> 5
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
