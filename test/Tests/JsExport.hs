-- | Test export of JS programs

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Tests.JsExport (test) where

import qualified Control.Lens as Lens
import           Control.Monad ((>=>))
import           Lamdu.Calc.Identifier (identFromHex)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Data.Db.Layout (runDbTransaction)
import qualified Lamdu.Data.Export.JS as ExportJS
import           Lamdu.Data.Export.JSON.Migration.ToVersion16 (replVar)
import           Lamdu.VersionControl (runAction)
import           System.FilePath ((</>))
import           Test.Lamdu.Db (ramDB)
import           Test.Lamdu.Exec (runJS)

import           Test.Lamdu.Prelude

test :: TestTree
test =
    testGroup "js-export"
    [ testOnePlusOne
    , testFieldAndParamUseSameTag
    ]

compile :: FilePath -> IO String
compile program =
    do
        db <- ramDB ["test/programs" </> program] & join
        identFromHex replVar ^?! Lens._Right & V.Var & ExportJS.compile & runAction & runDbTransaction db

run :: FilePath -> IO ByteString
run = compile >=> runJS

testProgram :: String -> ByteString -> TestTree
testProgram program expectedOutput =
    do
        result <- program <> ".json" & run
        assertEqual "Expected output" expectedOutput result
    & testCase program

testOnePlusOne :: TestTree
testOnePlusOne = testProgram "one-plus-one" "2\n"

testFieldAndParamUseSameTag :: TestTree
testFieldAndParamUseSameTag =
    testProgram "field-and-param-use-same-tag"
    "{ socket: [Function: socket] }\n"
