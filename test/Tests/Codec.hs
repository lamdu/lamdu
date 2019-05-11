module Tests.Codec where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Diff as AesonDiff
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSChar
import           Lamdu.Data.Db.Layout (runDbTransaction)
import qualified Lamdu.Data.Export.JSON as JsonFormat
import           Lamdu.VersionControl (runAction)
import           Test.Lamdu.Db (withDB)

import           Test.Lamdu.Prelude

test :: Test
test =
    testGroup "json-codec"
    [ migrationTest
    , reExportTests
    ]

migrationTest :: Test
migrationTest =
    JsonFormat.fileImportAll "test/programs/old-codec-factorial.json" & void
    & testCase "migration"

reExportTest :: FilePath -> Test
reExportTest name =
    do
        src <- LBS.readFile path <&> Aeson.eitherDecode >>= either fail pure
        dst <- withDB path (runDbTransaction ?? runAction JsonFormat.jsonExportRepl)
        if src == dst
            then pure ()
            else
                "re-exported program mismatches:\n" <>
                LBSChar.unpack (AesonPretty.encodePretty (Aeson.toJSON (AesonDiff.diff dst src)))
                & assertString
        & testCase ("re-export " ++ name)
    where
        path = "test/programs/" <> name <> ".json"

reExportTests :: Test
reExportTests =
    [ "let-with-global-reference"
    , "lambda-in-fragment"
    , "applied-case"
    , "extract-with-skolems"
    , "relayed-arg"
    , "to-nom"
    , "foo"
    , "simple-lambda"
    ] <&> reExportTest & testGroup "program-tests"
