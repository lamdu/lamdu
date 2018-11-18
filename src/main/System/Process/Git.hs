-- | Run some git commands, even from another module's TH

-- TODO: Rename to Language.Haskell.TH.Git ?
module System.Process.Git
    ( hash
    , status
    , dirty
    ) where

import qualified Control.Exception as E
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS
import           System.Process (readProcess)

import           Lamdu.Prelude

runGit :: [String] -> TH.Q String
runGit args =
    readProcess "git" args ""
    & E.handle (\e@E.SomeException {} -> "git failed: " ++ show e & pure)
    & TH.runIO

hash :: TH.ExpQ
hash = runGit ["rev-parse", "HEAD"] <&> words <&> head >>= TH.stringE

status :: TH.ExpQ
status = runGit ["status", "--porcelain"] >>= TH.stringE

dirty :: TH.ExpQ
dirty = runGit ["status", "--porcelain"] <&> words <&> not . null >>= THS.lift
