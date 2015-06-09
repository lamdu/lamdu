{-# LANGUAGE OverloadedStrings #-}
module Lamdu.Builtins.Anchors where

import qualified Lamdu.Expr.Val as V
import Lamdu.Expr.Type (Tag)

recurseVar :: V.Var
recurseVar = "RECURSE"

objTag :: Tag
objTag = "BI:object"

thenTag :: Tag
thenTag = "BI:then"

elseTag :: Tag
elseTag = "BI:else"

infixlTag :: Tag
infixlTag = "BI:infixl"

infixrTag :: Tag
infixrTag = "BI:infixr"
