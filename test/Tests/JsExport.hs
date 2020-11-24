-- | Test export of JS programs

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Tests.JsExport where

import qualified Data.ByteString as BS
import           Lamdu.Calc.Term (Term)
import           Lamdu.Data.Db.Layout (ViewM, runDbTransaction)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Export.JS as ExportJS
import           Lamdu.Expr.IRef (HRef)
import qualified Lamdu.Expr.Load as ExprLoad
import qualified Lamdu.Paths as Paths
import           Lamdu.VersionControl (runAction)
import           Revision.Deltum.Transaction (Transaction)
import           System.FilePath ((</>), splitFileName)
import qualified System.IO as IO
import qualified System.NodeJS.Path as NodeJS
import qualified System.Process as Proc
import           System.Process.Utils (withProcess)
import           Test.Lamdu.Db (withDB)

import           Test.Lamdu.Prelude

type T = Transaction

test :: Test
test =
    testGroup "js-export"
    [ testOnePlusOne
    , testFieldAndParamUseSameTag
    ]

readRepl :: T ViewM (Def.Expr (Ann (HRef ViewM) # Term))
readRepl = ExprLoad.defExpr (DbLayout.repl DbLayout.codeAnchors)

nodeRepl :: IO Proc.CreateProcess
nodeRepl =
    do
        rtsPath <- Paths.getDataFileName "js/rts.js" <&> fst . splitFileName
        nodeExePath <- NodeJS.path
        pure (Proc.proc nodeExePath [])
            { Proc.std_in = Proc.CreatePipe
            , Proc.std_out = Proc.CreatePipe
            , Proc.env =
                Just [("NODE_PATH", (rtsPath </> "export") ++ ":" ++ rtsPath)]
            }

compile :: FilePath -> IO String
compile program =
    withDB ("test/programs" </> program) $
    \db -> runDbTransaction db $ runAction $ readRepl >>= ExportJS.compile

run :: FilePath -> IO ByteString
run program =
    do
        compiledCode <- compile program
        procParams <- nodeRepl
        withProcess procParams $
            \(Just stdin, Just stdout, Nothing, _procHandle) ->
            do
                IO.hPutStrLn stdin compiledCode
                IO.hClose stdin
                BS.hGetContents stdout

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
