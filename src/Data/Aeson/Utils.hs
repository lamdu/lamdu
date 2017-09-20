module Data.Aeson.Utils
    ( removePrefix
    , decapitalize
    ) where

import Data.Char (toLower)
import Data.List (isPrefixOf)

decapitalize :: String -> String
decapitalize [] = []
decapitalize (c:cs) = toLower c : cs

removePrefix :: (Show a, Eq a) => [a] -> [a] -> [a]
removePrefix prefix str
    | prefix `isPrefixOf` str = drop (length prefix) str
    | otherwise = error ("removePrefix " ++ show prefix ++ " on " ++ show str)
