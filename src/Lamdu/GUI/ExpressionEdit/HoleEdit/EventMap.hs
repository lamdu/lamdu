{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap
    ( allowedSearchTermCommon
    ) where

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Lamdu.CharClassification as Chars

import           Lamdu.Prelude

allowedSearchTermCommon :: Text -> Bool
allowedSearchTermCommon searchTerm =
    any (searchTerm &)
    [ Text.all (`elem` Chars.operator)
    , Text.all Char.isAlphaNum
    , (`Text.isPrefixOf` "{}")
    ]
