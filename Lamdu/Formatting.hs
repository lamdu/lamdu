{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Formatting
    ( formatNum
    , formatBytes, parseBytes
    , formatText, parseText
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

parseBytes :: String -> Maybe SBS.ByteString
parseBytes ('#':xs) = parseHexDigits xs
parseBytes _ = Nothing

formatNum :: Double -> String
formatNum x
    | fromIntegral i /= x = printf "%f" x
    | isInfinite x = ['-' | x < 0] ++ "Inf"
    | otherwise = show i
    where
        i :: Integer
        i = truncate x

formatBytes :: SBS.ByteString -> String
formatBytes bs = '#' : concatMap showHexByte (SBS.unpack bs)

parseText :: String -> Maybe String
parseText = readMaybe

formatText :: String -> String
formatText text =
    concat ["\"", concatMap escape text, "\""]
    where
        escape c
            | Char.isControl c = Char.showLitChar c ""
            | otherwise = [c]
