{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Lamdu.GUI.ExpressionEdit.HoleEdit.AllowedSearchTerm
    ( allowedSearchTermCommon, allowedFragmentSearchTerm
    ) where

import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Lamdu.CharClassification as Chars

import           Lamdu.Prelude

type Suffix = Char

allowedSearchTermCommon :: [Suffix] -> Text -> Bool
allowedSearchTermCommon suffixes searchTerm =
    any (searchTerm &)
    [ Text.all (`elem` Chars.operator)
    , Text.all Char.isAlphaNum
    , (`Text.isPrefixOf` "{}")
    , (== "\\")
    , Lens.has (Lens.reversed . Lens._Cons . Lens.filtered inj)
    ]
    where
        inj (lastChar, revInit) =
            lastChar `elem` suffixes && Text.all Char.isAlphaNum revInit

allowedFragmentSearchTerm :: Text -> Bool
allowedFragmentSearchTerm searchTerm =
    allowedSearchTermCommon ":" searchTerm || isGetField searchTerm
    where
        isGetField t =
            case Text.uncons t of
            Just (c, rest) -> c == '.' && Text.all Char.isAlphaNum rest
            Nothing -> False
