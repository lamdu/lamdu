module Lamdu.CharClassification
  ( operatorChars, alphaNumericChars
  ) where

operatorChars :: [Char]
operatorChars = "\\+-*/^=><&|%$:."

alphaNumericChars :: [Char]
alphaNumericChars = ['a'..'z'] ++ ['0'..'9']
