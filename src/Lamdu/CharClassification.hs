module Lamdu.CharClassification
    ( operator, bracket, digit, hexDigit
    ) where

import Lamdu.Prelude

operator :: String
operator = "+-*/\\^=><&|%$:.!;?@~≥≤≠⋲"

bracket :: String
bracket = "()[]{}"

digit :: String
digit = ['0'..'9']

hexDigit :: String
hexDigit = ['a'..'f'] ++ ['A' .. 'F'] ++ digit
