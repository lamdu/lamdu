{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
module Lamdu.Formatting
    ( Format(..)
    ) where

import           Control.Lens.Operators
import qualified Data.ByteString as SBS
import qualified Data.Char as Char
import           Data.Word (Word8)
import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Prelude.Compat

chunks :: Int -> [a] -> Maybe [[a]]
chunks _ [] = Just []
chunks n xs
    | length firstChunk == n = chunks n rest <&> (firstChunk :)
    | otherwise = Nothing
    where
        (firstChunk, rest) = splitAt n xs

parseHexDigit :: Char -> Maybe Word8
parseHexDigit x
    | Char.isHexDigit x = Just (fromIntegral (Char.digitToInt x))
    | otherwise = Nothing

parseHexByte :: String -> Maybe Word8
parseHexByte [x,y] =
    (+)
    <$> (parseHexDigit x <&> (16 *))
    <*> parseHexDigit y
parseHexByte _ = Nothing

showHexByte :: Word8 -> String
showHexByte = printf "%02X"

parseHexDigits :: String -> Maybe SBS.ByteString
parseHexDigits str =
    chunks 2 str >>= mapM parseHexByte <&> SBS.pack

class Format a where
    tryParse :: String -> Maybe a
    format :: a -> String

instance Format SBS.ByteString where
    tryParse ('#':xs) = parseHexDigits xs
    tryParse _ = Nothing
    format bs = '#' : concatMap showHexByte (SBS.unpack bs)

instance Format Double where
    tryParse ('.':searchTerm) = readMaybe ('0':searchTerm)
    tryParse searchTerm =
        case reads searchTerm of
        [(val, "")] -> Just val
        [(val, ".")] | '.' `notElem` init searchTerm -> Just val
        _ -> Nothing
    format x
        | fromIntegral i /= x = printf "%f" x
        | isInfinite x = ['-' | x < 0] ++ "Inf"
        | otherwise = show i
        where
            i :: Integer
            i = truncate x

instance Format [Char] where
    tryParse = readMaybe
    format text =
        concat ["\"", concatMap escape text, "\""]
        where
            escape '\n' = "\n"
            escape c
                | Char.isControl c = Char.showLitChar c ""
                | otherwise = [c]
