{-# LANGUAGE CPP #-}

#ifdef NO_CODE
-- This module provides helpers for TemplateHaskell deriveJSON calls.
-- When running with -fno-code these won't work.
-- Making this module unavaliable triggers compilation errors in those calls
-- that are much better than the linking errors that would trigger otherwise.

module Data.Aeson.Utils () where

import Prelude ()
#else
module Data.Aeson.Utils
    ( removePrefix, removeOptionalUnderscore
    , decapitalize
    ) where

import Data.Char (toLower)
import Data.List (isPrefixOf)

import Prelude

decapitalize :: String -> String
decapitalize [] = []
decapitalize (c:cs) = toLower c : cs

removePrefix :: (Show a, Eq a) => [a] -> [a] -> [a]
removePrefix prefix str
    | prefix `isPrefixOf` str = drop (length prefix) str
    | otherwise = error ("removePrefix " ++ show prefix ++ " on " ++ show str)

removeOptionalUnderscore :: String -> String
removeOptionalUnderscore ('_':xs) = xs
removeOptionalUnderscore xs = xs

#endif
