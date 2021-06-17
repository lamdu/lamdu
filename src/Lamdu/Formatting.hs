module Lamdu.Formatting
    ( Format(..)
    , formatTextContents
    ) where

import qualified Control.Lens as Lens
import           Control.Monad (mplus)
import qualified Data.ByteString.Base16 as Hex
import qualified Data.Char as Char
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Lamdu.Prelude

formatTextContents :: Text -> Text
formatTextContents =
    Text.concatMap escape
    where
        escape '\n' = "\n"
        escape '\\' = "\\\\"
        escape c
            | Char.isControl c = Text.pack (Char.showLitChar c "")
            | otherwise = Text.singleton c

class Format a where
    tryParse :: Text -> Maybe a
    format :: a -> Text

instance Format ByteString where
    tryParse str =
        case Text.uncons str of
        Just ('#', xs) -> Hex.decode (encodeUtf8 xs) ^? Lens._Right
        _ -> Nothing
    format bs = Text.cons '#' $ decodeUtf8 (Hex.encode bs)

instance Format Double where
    tryParse str
        | "." `Text.isPrefixOf` str =
            readMaybe ('0':Text.unpack str)
        | "-." `Text.isPrefixOf` str =
            tryParse (Text.tail str) <&> negate
        | otherwise =
            case reads (Text.unpack str) of
            [(val, "")] -> Just val
            [(val, ".")] | '.' `notElem` init (Text.unpack str) -> Just val
            _ -> Nothing
    format x
        | fromIntegral i /= x = printf "%f" x & Text.pack
        | isInfinite x = ["-" | x < 0] ++ ["Inf"] & mconcat
        | otherwise = Text.pack (show i)
        where
            i :: Integer
            i = truncate x

instance Format Int where
    tryParse str =
        case reads (Text.unpack str) of
        [(val, "")] -> Just val
        _ -> Nothing
    format = Text.pack . show

instance Format Text where
    tryParse x = mplus (readMaybe (Text.unpack x)) (readMaybe (Text.unpack x ++ "\""))
    format text = mconcat ["\"", formatTextContents text, "\""]
