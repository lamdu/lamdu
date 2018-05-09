module System.Process.Utils
    ( withProcess
    ) where

import qualified Control.Exception as E
import           System.IO (Handle, hClose)
import           System.Process

import           Lamdu.Prelude

withProcess ::
    CreateProcess ->
    ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO a) ->
    IO a
withProcess createProc =
    E.bracket
    (createProcess createProc)
    (E.uninterruptibleMask_ . close)
    where
      close (_, mStdout, mStderr, handle) =
          do
              _ <- [mStdout, mStderr] & traverse . traverse %%~ hClose
              terminateProcess handle
              waitForProcess handle

