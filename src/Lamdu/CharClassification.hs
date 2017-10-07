{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.CharClassification
    ( operator, bracket, digit, hexDigit, precedence
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

-- | Returns a precedence between 0..10
precedence :: Char -> Int
precedence c = precedenceMap ^. Lens.at c & fromMaybe 12

-- | Returns a precedence between 0..10
-- Based on Table 2 in https://www.haskell.org/onlinereport/decls.html
precedenceMap :: Map Char Int
precedenceMap =
    [ ('$', 0)
    , (';', 1)
    , ('|', 2)
    , ('&', 3)
    ] ++
    [ (c  , 4) | c <- "=><≠≥≤" ] ++
    [ ('.', 5) ] ++
    [ (c  , 6) | c <- "+-" ] ++
    [ (c  , 7) | c <- "*/%" ] ++
    [ ('^', 8)
    , ('!', 9) ]
    & Map.fromList
    <&> (+ 2)
