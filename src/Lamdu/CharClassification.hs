module Lamdu.CharClassification
    ( operator, digit
    ) where

import Lamdu.Prelude (String)

operator :: String
operator = "+-*/\\^=><&|%$:.!;?@~≥≤≠⋲"

digit :: String
digit = ['0'..'9']
