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
    , reExportTest
    ]

migrationTest :: Test
migrationTest =
    JsonFormat.fileImportAll "test/programs/old-codec-factorial.json" & void
    & testCase "migration"

reExportTest :: Test
reExportTest =
    do
        src <- LBS.readFile fooPath <&> Aeson.eitherDecode >>= either fail pure
        dst <- withDB fooPath (runDbTransaction ?? runAction JsonFormat.jsonExportRepl)
        if src == dst
            then pure ()
            else
                "re-exported program mismatches:\n" <>
                LBSChar.unpack (AesonPretty.encodePretty (Aeson.toJSON (AesonDiff.diff dst src)))
                & assertString
    & testCase "re-export"
    where
        fooPath = "test/programs/foo.json"
