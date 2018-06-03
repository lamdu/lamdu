module Test.Lamdu.Sugar where

import           Control.DeepSeq (deepseq)
import           Lamdu.Debug (noopMonitors)
import           Lamdu.Data.Db.Layout (ViewM, codeAnchors, runDbTransaction)
import qualified Lamdu.Cache as Cache
import qualified Lamdu.Eval.Results as EvalResults
import           Lamdu.GUI.CodeEdit.Load (loadWorkArea)
import           Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.Name (Name)
import           Lamdu.Sugar.Types
import           Lamdu.VersionControl (runAction)
import           Revision.Deltum.Transaction (Transaction)
import           Test.Lamdu.Db (withDB)

import           Test.Lamdu.Prelude

type T = Transaction

convertWorkArea ::
    Cache.Functions ->
    T ViewM (WorkArea (Name (T ViewM)) (T ViewM) (T ViewM) ExprGui.Payload)
convertWorkArea cache =
    loadWorkArea cache noopMonitors (pure EvalResults.empty) codeAnchors
    >>= \x -> deepseq x (pure x)

testProgram :: FilePath -> (Cache.Functions -> T ViewM a) -> IO a
testProgram program action =
    do
        cache <- Cache.make <&> snd
        withDB ("test/programs/" <> program)
            (runDbTransaction ?? runAction (action cache))
