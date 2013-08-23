module Lamdu.Data.Infer.Monad.Run (run) where

import Control.Lens.Operators
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT(..), runWriterT)
import Lamdu.Data.Infer.Context (Context)
import Lamdu.Data.Infer.Monad (Infer, Error(..))
import qualified Control.Lens as Lens
import qualified Data.OpaqueRef as OR
import qualified Lamdu.Data.Infer.Monad as InferM
import qualified Lamdu.Data.Infer.Rule as Rule

run :: Ord def => Infer def a -> StateT (Context def) (Either (Error def)) a
run act = do
  (res, rulesTriggered) <- runInferWriter act
  go rulesTriggered
  return res
  where
    runInferWriter = runWriterT . (^. Lens.from InferM.infer)
    go (InferM.TriggeredRules oldRuleRefs) =
      case OR.refMapMinViewWithKey oldRuleRefs of
      Nothing -> return ()
      Just ((firstRuleRef, triggers), ruleIds) ->
        go . filterRemovedRule firstRuleRef . (Lens._2 <>~ InferM.TriggeredRules ruleIds) =<<
        runInferWriter (Rule.execute firstRuleRef triggers)
    filterRemovedRule _ (True, rules) = rules
    filterRemovedRule ruleId (False, InferM.TriggeredRules rules) =
      InferM.TriggeredRules $ rules & Lens.at ruleId .~ Nothing
