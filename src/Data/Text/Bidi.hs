-- | Simple temporary half-assed bidi implementation
-- which simply reverses the whole thing if the first letter is an RTL letter.

module Data.Text.Bidi
    ( toVisual
    ) where

import           Data.Char.Properties.BidiCategory (BidiCategory(..), getBidiCategory)
import qualified Data.Text as Text
import           Data.Text (Text)

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

toVisual :: Text -> Text
toVisual t
    | isLeftToRight t = t
    | otherwise = Text.map flipChar (Text.reverse t)

flipChar :: Char -> Char
flipChar '(' = ')'
flipChar ')' = '('
flipChar x = x