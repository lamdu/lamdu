module Lamdu.Data.Infer.Monad
  ( Error(..)
  , TriggeredRules(..)
  , Infer
  , liftContext, liftUFExprs, liftGuidAliases
  , liftError, error
  , ruleTrigger
  ) where

import Prelude hiding (error)

import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT(..))
import Control.Monad.Trans.Writer (WriterT(..))
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Lamdu.Data.Expression.Utils () -- Expr.Body Show instance
import Lamdu.Data.Infer.GuidAliases (GuidAliases)
import Lamdu.Data.Infer.Internal
import Lamdu.Data.Infer.RefTags (ExprRef, TagRule)
import Lamdu.Data.Infer.Rule.Internal
import Lamdu.Data.Infer.Trigger.Internal (Trigger)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Map as Map
import qualified Data.OpaqueRef as OR
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

newtype TriggeredRules def = TriggeredRules
  { triggeredRules :: OR.RefMap (TagRule def) (Map (ExprRef def, Trigger def) Bool)
  }
instance Monoid (TriggeredRules def) where
  mempty = TriggeredRules OR.refMapEmpty
  mappend (TriggeredRules x) (TriggeredRules y) =
    TriggeredRules $ OR.refMapUnionWith mappend x y

type Infer def =
  WriterT (TriggeredRules def)
  (StateT (Context def) (Either (Error def)))

ruleTrigger :: RuleRef def -> ExprRef def -> Trigger def -> Bool -> Infer def ()
ruleTrigger ruleId ref trigger res =
  Writer.tell . TriggeredRules $
  OR.refMapSingleton ruleId (Map.singleton (ref, trigger) res)

liftContext ::
  StateT (Context def) (Either (Error def)) a -> Infer def a
liftContext = lift

liftUFExprs ::
  StateT (UFExprs def) (Either (Error def)) a ->
  Infer def a
liftUFExprs = liftContext . Lens.zoom ctxUFExprs

liftGuidAliases :: StateT (GuidAliases def) (Either (Error def)) a -> Infer def a
liftGuidAliases = liftContext . Lens.zoom ctxGuidAliases

liftError :: Either (Error def) a -> Infer def a
liftError = lift . lift

error :: Error def -> Infer def a
error = liftError . Left
