-- | Simple temporary half-assed bidi implementation
-- which simply reverses the whole thing if the first letter is an RTL letter.

module Data.Text.Bidi
    ( toVisual
    ) where

import           Data.Char.Properties.BidiCategory (BidiCategory(..), getBidiCategory)
import qualified Data.Text as Text
import           Data.Text (Text)

toVisual :: Text -> Text
toVisual t | Text.null t = t
toVisual t =
    case getBidiCategory (Text.head t) of
    BidiR -> r -- Right to left
    BidiAL -> r -- Arabic letter
    BidiRLE -> r -- Right-to-left embedding
    BidiRLO -> r -- Right-to-left override
    _ -> t
    where
        r = Text.reverse t
