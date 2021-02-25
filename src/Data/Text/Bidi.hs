-- | Simple temporary half-assed bidi implementation
-- which simply reverses the whole thing if the first letter is an RTL letter.

module Data.Text.Bidi
    ( toVisual, isLeftToRight
    ) where

import           Data.Char.Properties.BidiCategory (BidiCategory(..), getBidiCategory)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Prelude

isLeftToRight :: Text -> Bool
isLeftToRight t
    | Text.null t = True
    | otherwise =
        case getBidiCategory (Text.head t) of
        BidiL -> True -- Left to right
        BidiLRE -> True -- Left-to-right embedding
        BidiLRO -> True -- Left-to-right override
        BidiR -> False -- Right to left
        BidiAL -> False -- Arabic letter
        BidiRLE -> False -- Right-to-left embedding
        BidiRLO -> False -- Right-to-left override
        _ -> isLeftToRight (Text.tail t)

toVisualLine :: Text -> Text
toVisualLine t
    | isLeftToRight t = t
    | otherwise = Text.map flipChar (Text.reverse t)

toVisual :: Text -> Text
toVisual = Text.intercalate "\n" . map toVisualLine . Text.splitOn "\n"

flipChar :: Char -> Char
flipChar '(' = ')'
flipChar ')' = '('
flipChar x = x
