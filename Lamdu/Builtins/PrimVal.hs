{-# LANGUAGE OverloadedStrings #-}
module Lamdu.Builtins.PrimVal
    ( KnownPrim(..)
    , fromKnown, toKnown
    ) where

import           Data.Binary.Utils (encodeS, decodeS)
import           Data.ByteString (ByteString)
import           Lamdu.Builtins.Anchors (bytesTid, floatTid)
import qualified Lamdu.Calc.Val as V

data KnownPrim
    = Float Double
    | Bytes ByteString
    deriving (Eq, Ord, Show)

toKnown :: V.PrimVal -> KnownPrim
toKnown (V.PrimVal litId bytes)
    | litId == floatTid = Float (decodeS bytes)
    | litId == bytesTid = Bytes bytes
    | otherwise = error $ "Unknown prim id: " ++ show litId

fromKnown :: KnownPrim -> V.PrimVal
fromKnown (Float dbl) = V.PrimVal floatTid (encodeS dbl)
fromKnown (Bytes bytes) = V.PrimVal bytesTid bytes
