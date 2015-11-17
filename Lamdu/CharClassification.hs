module Lamdu.CharClassification
    ( operatorChars, alphaNumericChars, digitChars
    ) where

operatorChars :: String
operatorChars = "\\+-*/^=><&|%$:.(){}!"

digitChars :: String
digitChars = ['0'..'9']

alphaNumericChars :: String
alphaNumericChars = ['a'..'z'] ++ digitChars
