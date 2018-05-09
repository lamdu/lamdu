module Main where

import qualified Lamdu.Data.Export.JSON as JsonFormat
import qualified TestAnimIdClash
import qualified TestColorSchemes
import qualified TestConfig
import qualified TestJsRtsTags
import qualified TestMomentu
import qualified TestNames
import qualified TestNix
import qualified TestPrecedence
import qualified TestStdlib
import qualified TestSugar
import qualified TestValUtils

import           Test.Lamdu.Prelude

jsonCodecMigrationTest :: Test
jsonCodecMigrationTest =
    JsonFormat.fileImportAll "test/programs/old-codec-factorial.json" & void
    & testCase "json-codec-migration"

main :: IO ()
main =
    defaultMainWithOpts tests mempty
    where
        tests =
            [ TestSugar.test
            , TestPrecedence.test
            , TestStdlib.test
            , TestNix.test
            , TestMomentu.test
            , TestAnimIdClash.test
            , TestColorSchemes.test
            , TestConfig.test
            , TestNames.test
            , TestJsRtsTags.test
            , TestValUtils.test
            , jsonCodecMigrationTest
            ]
