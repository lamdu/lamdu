{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Lamdu.Data.Infer.Context
  ( Context(..), ufExprs, defTVs, defVisibility, ruleMap, randomGen, guidAliases, empty
  , addToVisibility, removeFromVisibility
  , freshData, fresh, freshHole
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Lens.Operators
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)
import Lamdu.Data.Infer.GuidAliases (GuidAliases)
import Lamdu.Data.Infer.RefData (RefData, UFExprs, LoadedDef)
import Lamdu.Data.Infer.RefTags (TagExpr, ExprRef)
import Lamdu.Data.Infer.Rule.Types (RuleMap, initialRuleMap)
import Lamdu.Data.Infer.TypedValue (TypedValue)
import qualified Control.Lens as Lens
import qualified Control.Lens.Utils as LensUtils
import qualified Data.Map as Map
import qualified Data.OpaqueRef as OR
import qualified Data.UnionFind.WithData as UFData
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expr.Lens as ExprLens
import qualified Lamdu.Data.Infer.GuidAliases as GuidAliases
import qualified Lamdu.Data.Infer.RefData as RefData
import qualified System.Random as Random
import System.Random.Utils () -- Binary StdGen

-- Context
data Context def = Context
  { _ufExprs :: UFExprs def
  , _ruleMap :: RuleMap def
  , -- NOTE: This Map is for 2 purposes: Sharing Refs of loaded Defs
    -- and allowing to specify recursive defs
    _defTVs :: Map def (TypedValue def)
  , _defVisibility :: Map def (OR.RefSet (TagExpr def))
  , _randomGen :: Random.StdGen -- for guids
  , _guidAliases :: GuidAliases def
  } deriving (Typeable)
Lens.makeLenses ''Context

instance (Ord def, Binary def) => Binary (Context def) where
  get = Context <$> get <*> get <*> get <*> get <*> get <*> get
  put (Context a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f

empty :: Random.StdGen -> Context def
empty gen =
  Context
  { _ufExprs = UFData.empty
  , _ruleMap = initialRuleMap
  , _defTVs = Map.empty
  , _defVisibility = Map.empty
  , _randomGen = gen
  , _guidAliases = GuidAliases.empty
  }

atVisibility ::
  (Ord def, Monad m) => RefData def ->
  (Maybe (OR.RefSet (TagExpr def)) ->
   Maybe (OR.RefSet (TagExpr def))) ->
  StateT (Context def) m ()
atVisibility refData f =
  case mDef of
  Nothing -> return ()
  Just def -> defVisibility . Lens.at def %= f
  where
    mDef = refData ^. RefData.rdScope . RefData.scopeMDef

removeFromVisibility ::
  (Ord def, Monad m) => (ExprRef def, RefData def) -> StateT (Context def) m ()
removeFromVisibility (rep, refData) =
  atVisibility refData $
  LensUtils._fromJust "removeFromVisibility" . Lens.contains rep %~ uncontain
  where
    uncontain False = error "Should have been visible!"
    uncontain True = False

addToVisibility ::
  (Ord def, Monad m) => (ExprRef def, RefData def) -> StateT (Context def) m ()
addToVisibility (rep, refData) =
  atVisibility refData . mappend . Just $ OR.refSetSingleton rep

freshData :: (Ord def, MonadA m) => RefData def -> StateT (Context def) m (ExprRef def)
freshData refData = do
  rep <- Lens.zoom ufExprs $ UFData.fresh refData
  addToVisibility (rep, refData)
  return rep

fresh ::
  (Ord def, MonadA m) => RefData.Scope def ->
  Expr.Body (LoadedDef def) (ExprRef def) ->
  StateT (Context def) m (ExprRef def)
fresh scop body = freshData $ RefData.defaultRefData scop body

freshHole :: (Ord def, MonadA m) => RefData.Scope def -> StateT (Context def) m (ExprRef def)
freshHole scop = fresh scop $ ExprLens.bodyHole # ()
