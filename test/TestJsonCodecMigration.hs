{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Lamdu.Data.Export.JSON

import Lamdu.Prelude

migrationTest :: IO ()
migrationTest = fileImportAll "test/old-codec-factorial.json" & void

main :: IO ()
main = defaultMainWithOpts [testCase "migration" migrationTest] mempty
