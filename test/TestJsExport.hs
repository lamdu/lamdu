-- | Test export of JS programs

module TestJsExport where

import qualified Data.ByteString as BS
import           Lamdu.Calc.Val.Annotated (Val)
import           Lamdu.Data.Db.Layout (ViewM)
import           Lamdu.Data.Db.Layout (runDbTransaction)
import qualified Lamdu.Data.Db.Layout as DbLayout
import qualified Lamdu.Data.Definition as Def
import           Lamdu.Data.Export.JS (compile)
import           Lamdu.Expr.IRef (ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Paths as Paths
import           Lamdu.VersionControl (runAction)
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction
import           System.FilePath ((</>))
import           System.FilePath (splitFileName)
import qualified System.IO as IO
import qualified System.NodeJS.Path as NodeJS
import qualified System.Process as Proc
import           System.Process.Utils (withProcess)
import           Test.Lamdu.Db (withDB)

import           Test.Lamdu.Prelude

type T = Transaction

test :: Test
test = testGroup "js-export" [testOnePlusOne]

readRepl :: T ViewM (Def.Expr (Val (ValI ViewM)))
readRepl =
    DbLayout.repl DbLayout.codeIRefs & Transaction.readIRef
    >>= traverse ExprIRef.readVal

nodeRepl :: IO Proc.CreateProcess
nodeRepl =
    do
        rtsPath <- Paths.getDataFileName "js/rts.js" <&> fst . splitFileName
        nodeExePath <- NodeJS.path
        pure (Proc.proc nodeExePath ["--harmony-tailcalls"])
            { Proc.std_in = Proc.CreatePipe
            , Proc.std_out = Proc.CreatePipe
            , Proc.env =
                Just [("NODE_PATH", (rtsPath </> "export") ++ ":" ++ rtsPath)]
            }

testOnePlusOne :: Test
testOnePlusOne =
    testCase "one-plus-one" $
    do
        compiledCode <-
            withDB "test/programs/one-plus-one.json" $
            \db ->
                runDbTransaction db $ runAction $ readRepl >>= compile
        procParams <- nodeRepl
        stdout <-
            withProcess procParams $
            \(Just stdin, Just stdout, Nothing, _procHandle) ->
            do
                IO.hPutStrLn stdin compiledCode
                IO.hClose stdin
                BS.hGetContents stdout
        assertEqual "Nodejs output for 1+1=" "2\n" stdout
