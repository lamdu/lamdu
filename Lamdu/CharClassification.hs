module Lamdu.CharClassification
    ( operatorChars, alphaNumericChars, bracketChars, digitChars
    ) where

operatorChars :: String
operatorChars = "\\+-*/^=><&|%$:.!"

bracketChars :: String
bracketChars = "()[]{}"

digitChars :: String
digitChars = ['0'..'9']

alphaNumericChars :: String
alphaNumericChars = ['a'..'z'] ++ digitChars
