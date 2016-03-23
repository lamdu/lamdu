-- Constant tag names which have special support in the runtime or the sugaring.
-- Those which are supported in the runtime are repeated in JS in rts.js.

module Lamdu.Builtins.Anchors
    ( objTag, infixlTag, infixrTag, listTid, textTid
    , headTag, tailTag, consTag, nilTag, trueTag, falseTag, justTag, nothingTag
    , startTag, stopTag, indexTag
    , Order, anchorTags
    ) where

import qualified Data.Store.Guid as Guid
import           Data.String (IsString(..))
import           Lamdu.Expr.Type (Tag)
import qualified Lamdu.Expr.Type as T

rightPad :: Int -> a -> [a] -> [a]
rightPad l x xs
    | len >= l = xs
    | otherwise = xs ++ replicate (l - len) x
    where
        len = length xs

-- We want the translation to Guid and back to not be lossy, so we
-- canonize to Guid format
bi :: IsString a => String -> a
bi = fromString . rightPad Guid.length '\x00' . ("BI:" ++)

objTag :: Tag
objTag = bi "object"

infixlTag :: Tag
infixlTag = bi "infixl"

infixrTag :: Tag
infixrTag = bi "infixr"

indexTag :: Tag
indexTag = bi "index"

startTag :: Tag
startTag = bi "start"

stopTag :: Tag
stopTag = bi "stop"

listTid :: T.NominalId
listTid = bi "list"

textTid :: T.NominalId
textTid = bi "text"

headTag :: Tag
headTag = bi "head"

tailTag :: Tag
tailTag = bi "tail"

consTag :: Tag
consTag = bi "cons"

nilTag :: Tag
nilTag = bi "nil"

trueTag :: Tag
trueTag = bi "true"

falseTag :: Tag
falseTag = bi "false"

justTag :: Tag
justTag = bi "just"

nothingTag :: Tag
nothingTag = bi "nothing"

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
