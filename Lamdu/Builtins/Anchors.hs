{-# LANGUAGE OverloadedStrings #-}
module Lamdu.Builtins.Anchors
    ( objTag, infixlTag, infixrTag, listTid, textTid
    , headTag, tailTag, consTag, nilTag, trueTag, falseTag, justTag, nothingTag
    , startTag, stopTag, indexTag
    , Order, anchorTags
    ) where

import           Lamdu.Expr.Type (Tag)
import qualified Lamdu.Expr.Type as T

objTag :: Tag
objTag = "BI:object"

infixlTag :: Tag
infixlTag = "BI:infixl"

infixrTag :: Tag
infixrTag = "BI:infixr"

indexTag :: Tag
indexTag = "BI:index"

startTag :: Tag
startTag = "BI:start"

stopTag :: Tag
stopTag = "BI:stop"

listTid :: T.NominalId
listTid = "BI:list"

textTid :: T.NominalId
textTid = "BI:text"

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

type Order = Int

anchorTags :: [(Order, Tag, String)]
anchorTags =
    [ (0, objTag, "object")
    , (1, startTag, "start")
    , (2, stopTag, "stop")
    , (1, indexTag, "index")
    , (0, infixlTag, "infixl")
    , (1, infixrTag, "infixr")
    , (0, headTag, "head")
    , (1, tailTag, "tail")
    , (0, nilTag, "Empty")
    , (1, consTag, "NonEmpty")
    , (0, trueTag, "True")
    , (1, falseTag, "False")
    , (0, nothingTag, "Nothing")
    , (1, justTag, "Just")
    ]
