{-# LANGUAGE RankNTypes #-}
module Lamdu.Data.Infer.Rule
  ( execute
  ) where

import Control.Lens (Lens')
import Control.Lens.Operators
import Data.Map (Map)
import Data.Maybe.Utils (unsafeUnjust)
import Data.UnionFind (Ref)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Monad (Infer)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Lamdu.Data.Infer.Monad as InferM

ruleLens :: RuleId -> Lens' (Context def) (Maybe Rule)
ruleLens ruleId = ctxRuleMap . rmMap . Lens.at ruleId

verifyTag :: Map (Ref, Trigger) Bool -> Infer def ()
verifyTag triggers =
  case mViolation of
  Nothing -> return ()
  Just ref -> InferM.error $ InferM.CompositeTag ref
  where
    mViolation = Map.keys (Map.filter not triggers) ^? Lens.traverse . Lens._1

execute :: RuleId -> Map (Ref, Trigger) Bool -> Infer def ()
execute ruleId triggers = do
  mOldRule <- InferM.liftContext $ Lens.use (ruleLens ruleId)
  let oldRule = unsafeUnjust ("Execute called on bad rule id: " ++ show ruleId) mOldRule
  case oldRule of
    RuleVerifyTag -> verifyTag triggers
