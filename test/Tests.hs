module Main where

import qualified Lamdu.Data.Export.JSON as JsonFormat
import qualified TestColorSchemes
import qualified TestConfig
import qualified TestMomentu
import qualified TestStdlib
import           Test.Framework
import           Test.Framework.Providers.HUnit (testCase)

import           Lamdu.Prelude

jsonCodecMigrationTest :: IO ()
jsonCodecMigrationTest = JsonFormat.fileImportAll "test/old-codec-factorial.json" & void

main :: IO ()
main =
    defaultMainWithOpts tests mempty
    where
        tests =
            TestStdlib.tests ++
            TestMomentu.tests ++
            [ TestColorSchemes.test
            , TestConfig.test
            , testCase "json-codec-migration" jsonCodecMigrationTest
            ]
