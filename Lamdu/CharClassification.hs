module Lamdu.CharClassification
  ( operatorChars, alphaNumericChars
  ) where

operatorChars :: String
operatorChars = "\\+-*/^=><&|%$:."

alphaNumericChars :: String
alphaNumericChars = ['a'..'z'] ++ ['0'..'9']
