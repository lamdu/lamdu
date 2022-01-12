-- Constant tag names which have special support in the runtime or the sugaring.
-- Those which are supported in the runtime are repeated in JS in rts.js.

module Lamdu.Builtins.Anchors
    ( bytesTid, floatTid, listTid, textTid, charTid, treeTid, arrayTid, boolTid, mutTid
    , headTag, tailTag, rootTag, subtreesTag, trueTag, falseTag
    , valTypeParamId
    , genericVarTag, functionTag, recordTag, variantTag, unitTag, voidTag
    ) where

import           Data.List.Extended (rightPad)
import           Data.String (IsString(..))
import           Lamdu.Calc.Type (Tag)
import qualified Lamdu.Calc.Type as T

import           Lamdu.Prelude

-- We want the translation to UUID and back to not be lossy, so we
-- canonize to UUID format
bi :: IsString a => String -> a
bi = fromString . rightPad uuidLength '\x00' . ("BI:" ++)
    where
        uuidLength = 16

boolTid :: T.NominalId
boolTid = fromString (rightPad 16 '\x00' "Bool")

bytesTid :: T.NominalId
bytesTid = bi "bytes"

floatTid :: T.NominalId
floatTid = bi "float"

listTid :: T.NominalId
listTid = bi "stream"

arrayTid :: T.NominalId
arrayTid = bi "array"

charTid :: T.NominalId
charTid = bi "char"

textTid :: T.NominalId
textTid = bi "text"

treeTid :: T.NominalId
treeTid = bi "tree"

mutTid :: T.NominalId
mutTid = bi "ST"

headTag :: Tag
headTag = bi "head"

tailTag :: Tag
tailTag = bi "tail"

trueTag :: Tag
trueTag = bi "true"

falseTag :: Tag
falseTag = bi "false"

rootTag :: Tag
rootTag = bi "root"

subtreesTag :: Tag
subtreesTag = bi "subtrees"

valTypeParamId :: T.TypeVar
valTypeParamId = bi "val"

functionTag :: Tag
functionTag = bi "function"

recordTag :: Tag
recordTag = bi "record"

variantTag :: Tag
variantTag = bi "variant"

unitTag :: Tag
unitTag = bi "unit"

voidTag :: Tag
voidTag = bi "void"

genericVarTag :: Tag
genericVarTag =
    -- This random UUID was generated in the stdlib prior to being a "builtin"
    "\xa3\x86\xd9\x80\xd9os$Q\xf7\xb7\xb7\xeb\xab\x44\x1c"
