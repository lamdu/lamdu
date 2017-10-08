{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.CharClassification
    ( operator, bracket, digit, hexDigit
    , disallowedInHole
    ) where

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
