module Lamdu.CharClassification
    ( operatorChars, bracketChars, digitChars
    ) where

operatorChars :: String
operatorChars = "\\+-*/^=><&|%$:.,!;#?@~"

bracketChars :: String
bracketChars = "()[]{}"

digitChars :: String
digitChars = ['0'..'9']
