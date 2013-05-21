module Lamdu.CodeEdit.Sugar.Expression
  ( make, mkGen
  , mkCallWithArg
  , removeSuccessfulType, removeInferredTypes, removeTypes
  , addApplyChildParens, addParens
  , setNextHole
  , subExpressions
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (zipWithM, mplus)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (mapStateT)
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Traversable (traverse)
import Data.Typeable (Typeable1)
import Lamdu.CodeEdit.Sugar.Infer (Stored)
import Lamdu.CodeEdit.Sugar.Monad (SugarM)
import Lamdu.CodeEdit.Sugar.Types
import Lamdu.Data.Expression.Infer.Conflicts (iwcInferredTypes)
import qualified Control.Lens as Lens
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.Store.Guid as Guid
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.CodeEdit.Sugar.Infer as SugarInfer
import qualified Lamdu.CodeEdit.Sugar.Monad as SugarM
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Ops as DataOps
import qualified System.Random as Random
import qualified System.Random.Utils as RandomUtils

addParens :: ExpressionP name m pl -> ExpressionP name m pl
addParens =
  Lens.over rBody addParensBody
  where
    addParensBody (BodyInferred (Inferred val hole)) =
      BodyInferred $ Inferred (addParens val) hole
    addParensBody (BodyCollapsed (Collapsed g compact full)) =
      BodyCollapsed . Collapsed g compact $
      addParens full
    addParensBody x = Lens.set eHasParens HaveParens x

addApplyChildParens :: ExpressionP name m pl -> ExpressionP name m pl
addApplyChildParens x =
  case x ^. rBody of
  BodyApply{} -> x
  BodyCollapsed{} -> x
  _ -> addParens x

removeSuccessfulType :: Expression name m -> Expression name m
removeSuccessfulType =
  rPayload . plInferredTypes %~ removeIfNoErrors
  where
    removeIfNoErrors [_] = []
    removeIfNoErrors xs = xs

removeInferredTypes :: Expression name m -> Expression name m
removeInferredTypes = rPayload . plInferredTypes .~ []

removeTypes :: MonadA m => Expression name m -> Expression name m
removeTypes = Lens.mapped . plInferredTypes .~ []

mkGen :: Int -> Int -> Guid -> Random.StdGen
mkGen select count =
  Random.mkStdGen . (+select) . (*count) . BinaryUtils.decodeS . Guid.bs

mkCutter :: MonadA m => Anchors.CodeProps m -> DataIRef.ExpressionI (Tag m) -> T m Guid -> T m Guid
mkCutter cp expr replaceWithHole = do
  _ <- DataOps.newClipboard cp expr
  replaceWithHole

checkReinferSuccess :: MonadA m => SugarM.Context m -> String -> T m a -> CT m Bool
checkReinferSuccess sugarContext key act =
  case sugarContext ^. SugarM.scMReinferRoot of
  Nothing -> pure False
  Just reinferRoot ->
    mapStateT Transaction.forkScratch $ do
      _ <- lift act
      reinferRoot key

guardReinferSuccess :: MonadA m => SugarM.Context m -> String -> T m a -> CT m (Maybe (T m a))
guardReinferSuccess sugarContext key act = do
  success <- checkReinferSuccess sugarContext key act
  pure $
    if success
    then Just act
    else Nothing

mkCallWithArg ::
  MonadA m => SugarM.Context m ->
  DataIRef.ExpressionM m (SugarInfer.PayloadM m i (Stored m)) ->
  PrefixAction m -> CT m (Maybe (T m Guid))
mkCallWithArg sugarContext exprS prefixAction =
  guardReinferSuccess sugarContext "callWithArg" $ do
    prefixAction
    fmap DataIRef.exprGuid . DataOps.callWithArg $ SugarInfer.resultStored exprS

mkActions ::
  MonadA m => SugarM.Context m ->
  DataIRef.ExpressionM m (SugarInfer.PayloadM m i (Stored m)) -> Actions m
mkActions sugarContext exprS =
  Actions
  { _giveAsArg = giveAsArgPrefix
  , _callWithArg = mkCallWithArg sugarContext exprS
  , _callWithNextArg = pure (pure Nothing)
  , _setToHole = doReplace DataOps.setToHole
  , _replaceWithNewHole = doReplace DataOps.replaceWithHole
  , _cut = mkCutter (sugarContext ^. SugarM.scCodeAnchors) (Property.value stored) $ doReplace DataOps.replaceWithHole
  , _giveAsArgToOperator = DataIRef.exprGuid <$> DataOps.giveAsArgToOperator stored
  }
  where
    giveAsArgPrefix prefix = DataIRef.exprGuid <$> (prefix *> DataOps.giveAsArg stored)
    stored = SugarInfer.resultStored exprS
    doReplace f = DataIRef.exprGuid <$> f stored

make ::
  (Typeable1 m, MonadA m) => SugarInfer.ExprMM m ->
  BodyU m -> SugarM m (ExpressionU m)
make exprI expr = do
  sugarContext <- SugarM.readContext
  inferredTypes <-
    zipWithM
    ( fmap (SugarM.convertSubexpression . fmap SugarInfer.toPayloadMM)
    . SugarInfer.resultFromPure
    ) seeds types
  return
    Expression
    { _rGuid = SugarInfer.resultGuid exprI
    , _rBody = expr
    , _rPayload = Payload
      { _plInferredTypes = inferredTypes
      , _plActions =
        mkActions sugarContext <$>
        traverse (SugarInfer.ntraversePayload pure id) exprI
      , _plNextHole = Nothing
      }
    , _rHiddenGuids = []
    , _rPresugaredExpression =
      fmap (StorePoint . Property.value) .
      Lens.view SugarInfer.plStored <$> exprI
    }
  where
    seeds = RandomUtils.splits . mkGen 0 3 $ SugarInfer.resultGuid exprI
    types = maybe [] iwcInferredTypes $ SugarInfer.resultInferred exprI

setNextHole :: MonadA m => ExpressionU m -> ExpressionU m -> ExpressionU m
setNextHole dest =
  case dest ^? subHoles of
  Just hole ->
    -- The mplus ignores holes that are already set:
    Lens.mapped . plNextHole %~ (`mplus` Just hole)
  Nothing -> id
  where
    subHoles =
      Lens.folding subExpressions .
      Lens.filtered (Lens.notNullOf (rBody . _BodyHole))

subExpressions :: ExpressionU m -> [ExpressionU m]
subExpressions x = x : x ^.. rBody . Lens.traversed . Lens.folding subExpressions
