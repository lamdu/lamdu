-- | Verify config.json doesn't have any redundant fields (Aeson
-- parses successfully even with redundant fields!)
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Monad (unless)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as PP
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Monoid ((<>))
import           Lamdu.Config (Config)

main :: IO ()
main = do
    json <- either fail return . A.eitherDecode =<< BSL.readFile "config.json"
    case A.fromJSON json :: A.Result Config of
        A.Error msg -> fail msg
        A.Success config ->
            do
                unless (reencodedJson == json) $
                    do
                        putStrLn "Redundant fields detected!"
                        BSL.putStrLn $
                            "config.json:\n" <>
                            PP.encodePretty json
                        BSL.putStrLn $
                            "non-redundant json:\n" <>
                            PP.encodePretty reencodedJson
                        fail "Redundant fields exist!"
                putStrLn "Success, config.json has no redundant fields!"
            where
                reencodedJson = A.toJSON config
