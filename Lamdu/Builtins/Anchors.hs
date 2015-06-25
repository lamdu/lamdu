{-# LANGUAGE OverloadedStrings #-}
module Lamdu.Builtins.Anchors where

import           Lamdu.Expr.Type (Tag)
import qualified Lamdu.Expr.Val as V

recurseVar :: V.Var
recurseVar = "RECURSE"

objTag :: Tag
objTag = "BI:object"

infixlTag :: Tag
infixlTag = "BI:infixl"

infixrTag :: Tag
infixrTag = "BI:infixr"

headTag :: Tag
headTag = "BI:head"

tailTag :: Tag
tailTag = "BI:tail"

consTag :: Tag
consTag = "BI:cons"

nilTag :: Tag
nilTag = "BI:nil"

trueTag :: Tag
trueTag = "BI:true"

falseTag :: Tag
falseTag = "BI:false"

justTag :: Tag
justTag = "BI:just"

nothingTag :: Tag
nothingTag = "BI:nothing"

anchorNames :: [(Tag, String)]
anchorNames =
    [ (objTag, "object")
    , (infixlTag, "infixl")
    , (infixrTag, "infixr")
    , (headTag, "head")
    , (tailTag, "tail")
    , (consTag, "NonEmpty")
    , (nilTag, "Empty")
    , (trueTag, "True")
    , (falseTag, "False")
    , (justTag, "Just")
    , (nothingTag, "Nothing")
    ]
