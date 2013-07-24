module Lamdu.Data.Infer.Monad
  ( Error(..)
  , TriggeredRules(..)
  , Infer, run
  , liftContext, liftError, error
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
import Data.UnionFind (Ref)
import Lamdu.Data.Expression.Utils () -- Expr.Body Show instance
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.Rule.Internal
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.IntMap as IntMap
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Infer.ExprRefs as ExprRefs

data Error def
  = VarEscapesScope Guid
  | VarNotInScope
  | InfiniteExpression Ref
  | CompositeTag Ref
  | GetMissingField
  | GetFieldRequiresRecord
  | Mismatch (Expr.Body def Ref) (Expr.Body def Ref)
  deriving (Show)

newtype InferActions def = InferActions
  { iaExecuteRelation :: Relation -> Ref -> Infer def ()
  }

newtype TriggeredRules = TriggeredRules
  { triggeredRules :: RuleIdMap (Map (Ref, Trigger) Bool)
  }
instance Monoid TriggeredRules where
  mempty = TriggeredRules IntMap.empty
  mappend (TriggeredRules x) (TriggeredRules y) =
    TriggeredRules $ IntMap.unionWith mappend x y

type Infer def a =
  ReaderT (InferActions def)
  (WriterT TriggeredRules
   (StateT (Context def)
    (Either (Error def)))) a

ruleTrigger :: RuleId -> Ref -> Trigger -> Bool -> Infer def ()
ruleTrigger ruleId ref trigger res =
  lift . Writer.tell . TriggeredRules $
  IntMap.empty & Lens.at ruleId .~
  Just (mempty & Lens.at (ref, trigger) .~ Just res)

liftContext ::
  StateT (Context def) (Either (Error def)) a ->
  Infer def a
liftContext = lift . lift

liftError :: Either (Error def) a -> Infer def a
liftError = lift . lift . lift

error :: Error def -> Infer def a
error = liftError . Left

run ::
  InferActions def -> Infer def a ->
  WriterT TriggeredRules (StateT (Context def) (Either (Error def))) a
run = flip runReaderT

executeRelation :: Relation -> Ref -> Infer def ()
executeRelation relation ref = do
  act <- Reader.asks iaExecuteRelation
  act relation ref

rerunRelations :: Eq def => Ref -> Infer def ()
rerunRelations ref = do
  relations <- liftContext $ ExprRefs.read ref <&> (^. rdRelations)
  traverse_ (`executeRelation` ref) relations
