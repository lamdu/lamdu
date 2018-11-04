-- | Run some git commands, even from another module's TH

module System.Process.Git
    ( hash
    , status
    , dirty
    ) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as THS
import           System.Process (readProcess)

import           Prelude

runGit :: [String] -> TH.Q String
runGit args = TH.runIO (readProcess "git" args "")

hash :: TH.ExpQ
hash = (head . words <$> runGit ["rev-parse", "HEAD"]) >>= TH.stringE

status :: TH.ExpQ
status = runGit ["status", "--porcelain"] >>= TH.stringE

dirty :: TH.ExpQ
dirty = (not . null . words <$> runGit ["status", "--porcelain"]) >>= THS.lift
