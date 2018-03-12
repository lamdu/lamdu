{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Test.Framework
import Test.Framework.Providers.HUnit

import qualified Lamdu.Data.Export.JSON as JsonCodec

import Lamdu.Prelude

jsonCodecMigrationTest :: IO ()
jsonCodecMigrationTest = JsonCodec.fileImportAll "test/old-codec-factorial.json" & void

main :: IO ()
main = defaultMainWithOpts [testCase "json-codec-migration" jsonCodecMigrationTest] mempty
