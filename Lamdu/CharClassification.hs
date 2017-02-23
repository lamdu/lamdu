module Lamdu.CharClassification
    ( operatorChars, bracketChars, digitChars, hexDigitChars
    ) where

operatorChars :: String
operatorChars = "\\+-*/^=><&|%$:.,!;#?@~"

bracketChars :: String
bracketChars = "()[]{}"

digitChars :: String
digitChars = ['0'..'9']

hexDigitChars :: String
hexDigitChars = ['a'..'f'] ++ ['A' .. 'F'] ++ digitChars
