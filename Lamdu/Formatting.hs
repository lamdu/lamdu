{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
module Lamdu.Formatting
    ( Format(..)
    , formatTextContents
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (mplus)
import qualified Data.ByteString as SBS
import           Data.ByteString.Hex (showHexBytes, parseHexDigits)
import qualified Data.Char as Char
import           Text.Printf (printf)
import           Text.Read (readMaybe)

import           Prelude.Compat

formatTextContents :: String -> String
formatTextContents =
    concatMap escape
    where
        escape '\n' = "\n"
        escape c
            | Char.isControl c = Char.showLitChar c ""
            | otherwise = [c]

class Format a where
    tryParse :: String -> Maybe a
    format :: a -> String

instance Format SBS.ByteString where
    tryParse ('#':xs) = parseHexDigits xs ^? Lens._Right
    tryParse _ = Nothing
    format bs = '#' : showHexBytes bs

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
    tryParse x = mplus (readMaybe x) (readMaybe (x ++ "\""))
    format text = concat ["\"", formatTextContents text, "\""]
