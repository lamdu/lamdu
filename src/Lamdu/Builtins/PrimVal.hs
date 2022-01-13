module Lamdu.Builtins.PrimVal
    ( KnownPrim(..)
    , fromKnown, toKnown
    ) where

import           Data.Binary.Extended (encodeS, decodeS)
import           Lamdu.Builtins.Anchors (bytesTid, floatTid, charTid)
import qualified Lamdu.Calc.Term as V

import           Lamdu.Prelude

data KnownPrim
    = Float Double
    | Bytes ByteString
    | Char Char
    deriving (Eq, Ord, Show)

toKnown :: V.PrimVal -> KnownPrim
toKnown (V.PrimVal litId bytes)
    | litId == floatTid = Float (decodeS bytes)
    | litId == bytesTid = Bytes bytes
    | litId == charTid = Char (decodeS bytes)
    | otherwise = error $ "Unknown prim id: " ++ show litId

fromKnown :: KnownPrim -> V.PrimVal
fromKnown (Float dbl) = V.PrimVal floatTid (encodeS dbl)
fromKnown (Bytes bytes) = V.PrimVal bytesTid bytes
fromKnown (Char c) = V.PrimVal charTid (encodeS c)
