{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Lamdu.Data.Infer.Monad
  ( Error(..)
  , TriggeredRules(..)
  , Infer, infer
  , liftContext, liftUFExprs, liftGuidAliases, liftRuleMap
  , liftError, error
  , ruleTrigger
  ) where

import Prelude hiding (error)

import Control.Applicative (Applicative)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.Trans.Writer (WriterT(..))
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Lamdu.Data.Expression.Utils () -- Expr.Body Show instance
import Lamdu.Data.Infer.Context (Context)
import Lamdu.Data.Infer.GuidAliases (GuidAliases)
import Lamdu.Data.Infer.RefData (UFExprs, LoadedDef)
import Lamdu.Data.Infer.RefTags (ExprRef, TagRule, TagExpr)
import Lamdu.Data.Infer.Rule.Types (RuleRef, RuleMap)
import Lamdu.Data.Infer.Trigger.Types (Fired)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.OpaqueRef as OR
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Infer.Context as Context

data Error def
  = VarEscapesScope Guid
  | VarNotInScope
  | InfiniteExpression (ExprRef def)
  | CompositeTag (ExprRef def)
  | GetMissingField
  | GetFieldRequiresRecord
  | Mismatch
    (Expr.Body (LoadedDef def) (ExprRef def))
    (Expr.Body (LoadedDef def) (ExprRef def))
  deriving (Show)

newtype TriggeredRules def = TriggeredRules
  { triggeredRules :: OR.RefMap (TagRule def) (OR.RefMap (TagExpr def) [Fired def])
  }
instance Monoid (TriggeredRules def) where
  mempty = TriggeredRules OR.refMapEmpty
  mappend (TriggeredRules x) (TriggeredRules y) =
    TriggeredRules $ (OR.refMapUnionWith . OR.refMapUnionWith) mappend x y

newtype Infer def a
  = Infer
    ( WriterT (TriggeredRules def)
      (StateT (Context def)
       (Either (Error def))) a )
  deriving (Functor, Applicative, Monad)
Lens.makeIso ''Infer

ruleTrigger :: RuleRef def -> ExprRef def -> Fired def -> Infer def ()
ruleTrigger ruleRef ref fired =
  Infer . Writer.tell . TriggeredRules .
  OR.refMapSingleton ruleRef $
  OR.refMapSingleton ref [fired]

liftContext ::
  StateT (Context def) (Either (Error def)) a -> Infer def a
liftContext = Infer . lift

liftUFExprs ::
  StateT (UFExprs def) (Either (Error def)) a ->
  Infer def a
liftUFExprs = liftContext . Lens.zoom Context.ufExprs

liftGuidAliases :: StateT (GuidAliases def) (Either (Error def)) a -> Infer def a
liftGuidAliases = liftContext . Lens.zoom Context.guidAliases

liftRuleMap :: StateT (RuleMap def) (Either (Error def)) a -> Infer def a
liftRuleMap = liftContext . Lens.zoom Context.ruleMap

liftError :: Either (Error def) a -> Infer def a
liftError = Infer . lift . lift

error :: Error def -> Infer def a
error = liftError . Left
