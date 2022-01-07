-- | Test export of JS programs

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Tests.JsExport where

import           Control.Monad ((>=>))
import           Lamdu.Data.Db.Layout (runDbTransaction)
import qualified Lamdu.Data.Export.JS as ExportJS
import           Lamdu.VersionControl (runAction)
import           Revision.Deltum.Transaction (Transaction)
import           System.FilePath ((</>))
import           Test.Lamdu.Code (readRepl)
import           Test.Lamdu.Db (ramDB)
import           Test.Lamdu.Exec (runJS)

import           Test.Lamdu.Prelude

type T = Transaction

test :: Test
test =
    testGroup "js-export"
    [ testOnePlusOne
    , testFieldAndParamUseSameTag
    ]

compile :: FilePath -> IO String
compile program =
    do
        db <- ramDB ["test/programs" </> program]
        runDbTransaction db $ runAction $ readRepl >>= ExportJS.compile

run :: FilePath -> IO ByteString
run = compile >=> runJS

testProgram :: String -> ByteString -> Test
testProgram program expectedOutput =
    do
        result <- program <> ".json" & run
        assertEqual "Expected output" expectedOutput result
    & testCase program

testOnePlusOne :: Test
testOnePlusOne = testProgram "one-plus-one" "2\n"

testFieldAndParamUseSameTag :: Test
testFieldAndParamUseSameTag =
    testProgram "field-and-param-use-same-tag"
    "{ socket: [Function: socket] }\n"
