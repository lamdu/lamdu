{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.Formatting
    ( Format(..)
    , formatTextContents
    ) where

import           Control.Lens.Operators
import           Control.Monad (guard, mplus)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8 as BS
import qualified Data.Char as Char
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Prelude.Compat

formatTextContents :: Text -> Text
formatTextContents =
    Text.concatMap escape
    where
        escape '\n' = "\n"
        escape c
            | Char.isControl c = Text.pack (Char.showLitChar c "")
            | otherwise = Text.singleton c

class Format a where
    tryParse :: Text -> Maybe a
    format :: a -> Text

instance Format ByteString where
    tryParse str =
        case Text.uncons str of
        Just ('#', xs) ->
            do
                BS.null remain & guard
                Just result
            where
                (result, remain) = encodeUtf8 xs & Hex.decode
        _ -> Nothing
    format bs = Text.cons '#' $ decodeUtf8 (Hex.encode bs)

instance Format Double where
    tryParse searchTerm
        | "." `Text.isPrefixOf` searchTerm =
              readMaybe ('0':Text.unpack searchTerm)
        | otherwise =
            case reads (Text.unpack searchTerm) of
            [(val, "")] -> Just val
            [(val, ".")] | '.' `notElem` init (Text.unpack searchTerm) -> Just val
            _ -> Nothing
    format x
        | fromIntegral i /= x = printf "%f" x & Text.pack
        | isInfinite x = ["-" | x < 0] ++ ["Inf"] & mconcat
        | otherwise = Text.pack (show i)
        where
            i :: Integer
            i = truncate x

instance Format Text where
    tryParse x = mplus (readMaybe (Text.unpack x)) (readMaybe (Text.unpack x ++ "\""))
    format text = mconcat ["\"", formatTextContents text, "\""]
