module Tests.Codec where

import qualified Lamdu.Data.Export.JSON as JsonFormat

import           Test.Lamdu.Prelude

test :: Test
test =
    JsonFormat.fileImportAll "test/programs/old-codec-factorial.json" & void
    & testCase "json-codec-migration"
