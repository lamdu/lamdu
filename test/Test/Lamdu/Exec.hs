module Test.Lamdu.Exec (runJS) where

import qualified Data.ByteString as BS
import qualified Lamdu.Paths as Paths
import           System.FilePath ((</>), splitFileName)
import qualified System.IO as IO
import qualified System.NodeJS.Path as NodeJS
import qualified System.Process as Proc
import           System.Process.Utils (withProcess)

import           Test.Lamdu.Prelude

runJS :: String -> IO ByteString
runJS compiledCode =
    do
        procParams <- nodeRepl
        withProcess procParams $
            \(Just stdin, Just stdout, Nothing, _procHandle) ->
            do
                IO.hPutStrLn stdin compiledCode
                IO.hClose stdin
                BS.hGetContents stdout

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

