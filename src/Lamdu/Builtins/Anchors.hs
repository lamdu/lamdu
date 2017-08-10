-- Constant tag names which have special support in the runtime or the sugaring.
-- Those which are supported in the runtime are repeated in JS in rts.js.

module Lamdu.Builtins.Anchors
    ( bytesTid, floatTid, streamTid, textTid, treeTid, arrayTid
    , headTag, tailTag, rootTag, subtreesTag, trueTag, falseTag
    , valTypeParamId
    ) where

import           Data.List.Utils (rightPad)
import           Data.String (IsString(..))
import           Lamdu.Calc.Type (Tag)
import qualified Lamdu.Calc.Type as T

-- We want the translation to UUID and back to not be lossy, so we
-- canonize to UUID format
bi :: IsString a => String -> a
bi = fromString . rightPad uuidLength '\x00' . ("BI:" ++)
    where
        uuidLength = 16

bytesTid :: T.NominalId
bytesTid = bi "bytes"

floatTid :: T.NominalId
floatTid = bi "float"

streamTid :: T.NominalId
streamTid = bi "stream"

arrayTid :: T.NominalId
arrayTid = bi "array"

textTid :: T.NominalId
textTid = bi "text"

treeTid :: T.NominalId
treeTid = bi "tree"

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

valTypeParamId :: T.ParamId
valTypeParamId = bi "val"
