{-# LANGUAGE OverloadedStrings #-}
module Lamdu.Builtins.Literal
    ( Lit(..)
    , fromLit, toLit
    , floatType, bytesType
    , nameOf
    ) where

import           Data.Binary.Utils (encodeS, decodeS)
import           Data.ByteString (ByteString)
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

data Lit
    = LitFloat Double
    | LitBytes ByteString
    deriving (Eq, Ord, Show)

bytesId :: T.PrimId
bytesId = "BI:bytes"

floatId :: T.PrimId
floatId = "BI:float"

nameOf :: T.PrimId -> String
nameOf p
    | p == bytesId = "Bytes"
    | p == floatId = "Num"
    | otherwise = error $ "Invalid prim id: " ++ show p

floatType :: Type
floatType = T.TPrim floatId

bytesType :: Type
bytesType = T.TPrim bytesId

toLit :: V.Literal -> Lit
toLit (V.Literal litId bytes)
    | litId == floatId = LitFloat (decodeS bytes)
    | litId == bytesId = LitBytes bytes
    | otherwise = error $ "Unknown literal id: " ++ show litId

fromLit :: Lit -> V.Literal
fromLit (LitFloat dbl) = V.Literal floatId (encodeS dbl)
fromLit (LitBytes bytes) = V.Literal bytesId bytes
