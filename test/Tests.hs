module Main where

import qualified Lamdu.Data.Export.JSON as JsonFormat
import qualified TestAnimIdClash
import qualified TestColorSchemes
import qualified TestConfig
import qualified TestNames
import qualified TestJsRtsTags
import qualified TestMomentu
import qualified TestNix
import qualified TestStdlib
import qualified TestValUtils
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
            [ TestStdlib.test
            , TestNix.test
            , TestMomentu.test
            , TestAnimIdClash.test
            , TestColorSchemes.test
            , TestConfig.test
            , TestNames.test
            , TestJsRtsTags.test
            , TestValUtils.test
            , testCase "json-codec-migration" jsonCodecMigrationTest
            ]
