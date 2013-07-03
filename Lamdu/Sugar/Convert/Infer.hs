{-# LANGUAGE TemplateHaskell, DeriveFunctor, ConstraintKinds #-}
module Lamdu.Sugar.Convert.Infer
  ( Payload(..), plGuid, plInferred, plStored, plData
  , ExpressionSetter
  , NoInferred(..), InferredWC
  , NoStored(..), Stored
  , PayloadMM, ExprMM

  , isPolymorphicFunc
  , inferAddImplicits
  , InferredWithImplicits(..)

  , iwiSuccess
  , iwiBaseInferContext, iwiInferContext
  , iwiStructureInferContext
  , iwiExpr, iwiBaseExpr

  , mkExprPure

  -- TODO: These don't belong here:
  -- Type-check an expression into an ordinary Inferred Expression,
  -- short-circuit on error:
  , load, memoLoadInfer

  , exprInferred
  , exprStored
  , exprGuid
  , exprStoredGuid
  , exprData

  , plIRef
  , exprIRef
  , replaceWith
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad (void, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.Trans.State.Utils (toStateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Maybe (isJust)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Typeable (Typeable, Typeable1)
import Lamdu.Data.Expression.IRef (DefI, DefIM)
import Lamdu.Data.Expression.Infer.Conflicts (InferredWithConflicts(..), inferWithConflicts)
import Lamdu.Sugar.Types.Internal
import System.Random (RandomGen)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Cache as Cache
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Infer.ImplicitVariables as ImplicitVariables
import qualified Lamdu.Data.Expression.Infer.Structure as Structure
import qualified Lamdu.Data.Expression.Load as Load
import qualified Lamdu.Data.Expression.Utils as ExprUtil

data NoInferred = NoInferred
-- TODO: Replace (InferredWC t) with (InferredWC m)
type InferredWC t = InferredWithConflicts (DefI t)

data NoStored = NoStored
type Stored m = ExprIRef.ExpressionProperty m

type ExpressionSetter def = Expr.Expression def () -> Expr.Expression def ()

data Payload inferred stored a
  = Payload
    { _plGuid :: Guid
    , _plInferred :: inferred
    , _plStored :: stored
    , _plData :: a
    }
Lens.makeLenses ''Payload

type PayloadMM m a =
  Payload (Maybe (InferredWC (Tag m))) (Maybe (Stored m)) a
type ExprMM m a = ExprIRef.ExpressionM m (PayloadMM m a)

-- Not inferred, not stored
mkExprPure ::
  RandomGen g => g -> ExprIRef.ExpressionM m a -> ExprMM m a
mkExprPure g =
  ExprUtil.randomizeExpr g . fmap f
  where
    f a guid = Payload guid Nothing Nothing a

loader :: MonadA m => Infer.Loader (DefIM m) (T m)
loader =
  Infer.Loader
  (fmap void . ExprIRef.readExpression . (^. Definition.bodyType) <=<
   Transaction.readIRef)

load ::
  MonadA m => Maybe (DefIM m) ->
  ExprIRef.ExpressionM m a ->
  T m (Infer.Loaded (DefIM m) a)
load = Infer.load loader

pureMemoBy ::
  (Cache.Key k, Binary v, Typeable v, MonadA m) =>
  Cache.FuncId -> k -> v -> StateT Cache m v
pureMemoBy funcId k val = Cache.memoS funcId (const (return val)) k

-- memoLoadInfer does an infer "load" of the given expression (which
-- loads dependent expression types), and then memoizes the inference
-- result of that in a given context. It uses the "loaded" expression
-- (expr+dependent def types) and a given "inferStateKey" as the
-- memoization key. This is done because using the "inferState"
-- directly as a key could potentially be huge and
-- wasteful. Therefore, the caller is in charge of giving us a unique
-- identifier for the inferState that is preferably small.
memoLoadInfer ::
  (MonadA m, Typeable1 m, Cache.Key a, Binary a) =>
  Maybe (DefIM m) ->
  ExprIRef.ExpressionM m a ->
  Infer.Node (DefIM m) ->
  StateT (InferContext m) (MaybeT (CT m))
  (ExprIRef.ExpressionM m (Infer.Inferred (DefIM m), a))
memoLoadInfer mDefI expr node = do
  loaded <- lift . lift . lift $ load mDefI expr
  icHashKey %= Cache.bsOfKey . (,,) loaded node
  k <- Lens.use icHashKey
  Lens.zoom icContext $
    StateT . fmap (MaybeT . pureMemoBy "memoLoadInfer" k) . runStateT $
    Infer.inferLoaded (Infer.InferActions (const Nothing))
    loaded node

inferWithVariables ::
  (RandomGen g, MonadA m) => g ->
  Infer.Loaded (DefIM m) a -> Infer.Context (DefIM m) -> Infer.Node (DefIM m) ->
  T m
  ( ( Infer.Context (DefIM m)
    , ExprIRef.ExpressionM m (InferredWithConflicts (DefIM m), a)
    )
  , Maybe
    ( Infer.Context (DefIM m)
    , Infer.Context (DefIM m)
    , ExprIRef.ExpressionM m (InferredWithConflicts (DefIM m), ImplicitVariables.Payload a)
    )
  )
inferWithVariables gen loaded baseInferContext node =
  (`evalStateT` baseInferContext) $ do

    (success, expr) <- toStateT $ inferWithConflicts loaded node
    intermediateContext <- State.get

    mWithVariables <-
      if not success
      then return Nothing
      else Just <$> do
        -- success chceked above, guarantees no conflicts:
        let asIWC newInferred = InferredWithConflicts newInferred [] []

        withStructureExpr <- Structure.add loader (expr <&> Lens._1 %~ iwcInferred)
        withStructureContext <- State.get

        wvExpr <- ImplicitVariables.add gen loader withStructureExpr
        wvContext <- State.get

        return (withStructureContext, wvContext, wvExpr <&> Lens._1 %~ asIWC)

    return
      ( (intermediateContext, expr)
      , mWithVariables
      )

data InferredWithImplicits m a = InferredWithImplicits
  { _iwiSuccess :: Bool
  , _iwiInferContext :: Infer.Context (DefIM m)
  , _iwiStructureInferContext :: Infer.Context (DefIM m)
  , _iwiExpr :: ExprIRef.ExpressionM m (Payload (InferredWC (Tag m)) (Maybe (Stored m)) a)
  -- Prior to adding variables
  , _iwiBaseInferContext :: InferContext m
  , _iwiBaseExpr :: ExprIRef.ExpressionM m (Payload (InferredWC (Tag m)) (Stored m) a)
  }
Lens.makeLenses ''InferredWithImplicits

inferAddImplicits ::
  (RandomGen g, MonadA m, Typeable (m ())) => g ->
  Maybe (DefIM m) ->
  ExprIRef.ExpressionM m (Load.ExprPropertyClosure (Tag m)) ->
  Cache.KeyBS ->
  ( Infer.Context (DefIM m)
  , Infer.Node (DefIM m)
  ) -> CT m (InferredWithImplicits m ())
inferAddImplicits gen mDefI lExpr inferContextKey inferState = do
  loaded <- lift $ load mDefI lExpr
  ((baseContext, expr), mWithVariables) <-
    Cache.memoS "inferAddImplicits" uncurriedInfer (loaded, inferState)
  let baseExpr = mkStoredPayload <$> expr
  return InferredWithImplicits
    { _iwiSuccess = isJust mWithVariables
    , _iwiBaseInferContext = InferContext baseContext $ Cache.bsOfKey (loaded, inferContextKey)
    , _iwiBaseExpr = baseExpr

    , _iwiStructureInferContext = maybe baseContext (^. Lens._1) mWithVariables
    , _iwiInferContext = maybe baseContext (^. Lens._2) mWithVariables
    , _iwiExpr =
      maybe (baseExpr & Lens.mapped . plStored %~ Just)
      (fmap mkWVPayload . (^. Lens._3)) mWithVariables
    }
  where
    uncurriedInfer (loaded, (inferContext, inferNode)) =
      inferWithVariables gen loaded inferContext inferNode

    mkStoredPayload (iwc, propClosure) =
      Payload (ExprIRef.epGuid prop) iwc prop ()
      where
        prop = Load.exprPropertyOfClosure propClosure
    mkWVPayload (iwc, ImplicitVariables.AutoGen guid) =
      Payload guid iwc Nothing ()
    mkWVPayload (iwc, ImplicitVariables.Stored propClosure) =
      mkStoredPayload (iwc, propClosure)
      & plStored %~ Just

isPolymorphicFunc :: PayloadMM m a -> Bool
isPolymorphicFunc funcPl =
  maybe False
  (ExprUtil.isDependentPi . Infer.iType . iwcInferred) $
  funcPl ^. plInferred

exprGuid ::
  Lens' (Expr.Expression def (Payload inferred stored a)) Guid
exprGuid = Expr.ePayload . plGuid

exprStored ::
  Lens' (Expr.Expression def (Payload inferred stored a)) stored
exprStored = Expr.ePayload . plStored

exprInferred ::
  Lens' (Expr.Expression def (Payload inferred stored a)) inferred
exprInferred = Expr.ePayload . plInferred

exprData ::
  Lens' (Expr.Expression def (Payload inferred stored a)) a
exprData = Expr.ePayload . plData

plIRef ::
  Lens.Traversal' (Payload i (Maybe (Stored m)) a) (ExprIRef.ExpressionIM m)
plIRef = plStored . Lens._Just . Property.pVal

exprStoredGuid ::
  Lens.Fold
  (Expr.Expression def (Payload i (Maybe (Stored m)) a)) Guid
exprStoredGuid = exprIRef . Lens.to ExprIRef.exprGuid

replaceWith :: MonadA m => Stored m -> Stored m -> T m Guid
replaceWith parentP replacerP = do
  Property.set parentP replacerI
  return $ ExprIRef.exprGuid replacerI
  where
    replacerI = Property.value replacerP

exprIRef ::
  Lens.Traversal'
  (Expr.Expression def (Payload i (Maybe (Stored m)) a))
  (ExprIRef.ExpressionIM m)
exprIRef = exprStored . Lens._Just . Property.pVal
