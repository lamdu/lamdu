module Lamdu.Data.Infer.Monad
  ( Error(..)
  , TriggeredRules(..)
  , Infer, run
  , liftContext, liftUFExprs, liftGuidAliases
  , liftError, error
  , ruleTrigger
  -- TODO: Delete these:
  , InferActions(..)
  , executeRelation, rerunRelations
  ) where

import Prelude hiding (error)

import Control.Lens.Operators
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.Trans.Writer (WriterT(..))
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Lamdu.Data.Expression.Utils () -- Expr.Body Show instance
import Lamdu.Data.Infer.GuidAliases (GuidAliases)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.RefTags (ExprRef, TagRule)
import Lamdu.Data.Infer.Rule.Internal
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Map as Map
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expression as Expr

data Error def
  = VarEscapesScope Guid
  | VarNotInScope
  | InfiniteExpression (ExprRef def)
  | CompositeTag (ExprRef def)
  | GetMissingField
  | GetFieldRequiresRecord
  | Mismatch (Expr.Body def (ExprRef def)) (Expr.Body def (ExprRef def))
  deriving (Show)

newtype InferActions def = InferActions
  { iaExecuteRelation :: Relation def -> ExprRef def -> Infer def ()
  }

newtype TriggeredRules def = TriggeredRules
  { triggeredRules :: OR.RefMap (TagRule def) (Map (ExprRef def, Trigger def) Bool)
  }
instance Monoid (TriggeredRules def) where
  mempty = TriggeredRules OR.refMapEmpty
  mappend (TriggeredRules x) (TriggeredRules y) =
    TriggeredRules $ OR.refMapUnionWith mappend x y

type Infer def =
  ReaderT (InferActions def)
  (WriterT (TriggeredRules def)
   (StateT (Context def)
    (Either (Error def))))

ruleTrigger :: RuleRef def -> ExprRef def -> Trigger def -> Bool -> Infer def ()
ruleTrigger ruleId ref trigger res =
  lift . Writer.tell . TriggeredRules $
  OR.refMapSingleton ruleId (Map.singleton (ref, trigger) res)

liftContext ::
  StateT (Context def) (Either (Error def)) a ->
  Infer def a
liftContext = lift . lift

liftUFExprs ::
  StateT (UFExprs def) (Either (Error def)) a ->
  Infer def a
liftUFExprs = liftContext . Lens.zoom ctxUFExprs

liftGuidAliases :: StateT (GuidAliases def) (Either (Error def)) a -> Infer def a
liftGuidAliases = liftContext . Lens.zoom ctxGuidAliases

liftError :: Either (Error def) a -> Infer def a
liftError = lift . lift . lift

error :: Error def -> Infer def a
error = liftError . Left

run ::
  InferActions def -> Infer def a ->
  WriterT (TriggeredRules def) (StateT (Context def) (Either (Error def))) a
run = flip runReaderT

executeRelation :: Relation def -> ExprRef def -> Infer def ()
executeRelation relation ref = do
  act <- Reader.asks iaExecuteRelation
  act relation ref

rerunRelations :: Eq def => ExprRef def -> Infer def ()
rerunRelations ref = do
  relations <- liftContext . Lens.zoom ctxUFExprs $ UFData.read ref <&> (^. rdRelations)
  traverse_ (`executeRelation` ref) relations
