{-# LANGUAGE OverloadedStrings #-}
module Lamdu.Builtins.PrimVal
    ( KnownPrim(..)
    , fromKnown, toKnown
    , floatType, bytesType
    , nameOf
    ) where

import           Data.Binary.Utils (encodeS, decodeS)
import           Data.ByteString (ByteString)
import           Lamdu.Builtins.Anchors (bytesTid, floatTid)
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

data KnownPrim
    = Float Double
    | Bytes ByteString
    deriving (Eq, Ord, Show)

nameOf :: T.NominalId -> String
nameOf p
    | p == bytesTid = "Bytes"
    | p == floatTid = "Num"
    | otherwise = error $ "Invalid prim id: " ++ show p

floatType :: Type
floatType = T.TInst floatTid mempty

bytesType :: Type
bytesType = T.TInst bytesTid mempty

toKnown :: V.PrimVal -> KnownPrim
toKnown (V.PrimVal litId bytes)
    | litId == floatTid = Float (decodeS bytes)
    | litId == bytesTid = Bytes bytes
    | otherwise = error $ "Unknown prim id: " ++ show litId

fromKnown :: KnownPrim -> V.PrimVal
fromKnown (Float dbl) = V.PrimVal floatTid (encodeS dbl)
fromKnown (Bytes bytes) = V.PrimVal bytesTid bytes
