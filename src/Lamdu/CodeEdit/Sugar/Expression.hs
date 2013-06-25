module Lamdu.CodeEdit.Sugar.Expression
  ( make, mkGen
  , mkReplaceWithNewHole
  , setNextHoleToFirstSubHole
  , setNextHole
  , subExpressions
  , getStoredName
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (zipWithM, mplus)
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Typeable (Typeable1)
import Lamdu.CodeEdit.Sugar.Infer (Stored)
import Lamdu.CodeEdit.Sugar.Internal
import Lamdu.CodeEdit.Sugar.Monad (SugarM)
import Lamdu.CodeEdit.Sugar.Types
import Lamdu.CodeEdit.Sugar.Types.Internal
import Lamdu.Data.Expression.Infer.Conflicts (iwcInferredTypes)
import qualified Control.Lens as Lens
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.Store.Guid as Guid
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.CodeEdit.Sugar.Infer as SugarInfer
import qualified Lamdu.CodeEdit.Sugar.Monad as SugarM
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Ops as DataOps
import qualified System.Random as Random
import qualified System.Random.Utils as RandomUtils

mkGen :: Int -> Int -> Guid -> Random.StdGen
mkGen select count =
  Random.mkStdGen . (+select) . (*count) . BinaryUtils.decodeS . Guid.bs

mkCutter :: MonadA m => Anchors.CodeProps m -> ExprIRef.ExpressionI (Tag m) -> T m Guid -> T m Guid
mkCutter cp expr replaceWithHole = do
  _ <- DataOps.newClipboard cp expr
  replaceWithHole

mkReplaceWithNewHole :: MonadA m => Stored m -> T m Guid
mkReplaceWithNewHole stored =
  ExprIRef.exprGuid <$> DataOps.replaceWithHole stored

mkActions :: MonadA m => SugarM.Context m -> Stored m -> Actions m
mkActions sugarContext stored =
  Actions
  { _wrap = WrapAction $ ExprIRef.exprGuid <$> DataOps.wrap stored
  , _mSetToHole = Just $ ExprIRef.exprGuid <$> DataOps.setToHole stored
  , _cut =
    mkCutter (sugarContext ^. SugarM.scCodeAnchors)
    (Property.value stored) $ mkReplaceWithNewHole stored
  }

make ::
  (Typeable1 m, MonadA m) => SugarInfer.PayloadMM m a ->
  BodyU m a -> SugarM m (ExpressionU m a)
make exprPl body = do
  sugarContext <- SugarM.readContext
  inferredTypes <-
    zipWithM
    ( fmap SugarM.convertSubexpression
    . SugarInfer.mkExprPure
    ) seeds types
  return $ Expression body Payload
    { _plGuid = exprPl ^. SugarInfer.plGuid
    , _plInferredTypes = inferredTypes
    , _plActions =
      mkActions sugarContext <$> exprPl ^. SugarInfer.plStored
    , _plMNextHoleGuid = Nothing
    , _plData = exprPl ^. SugarInfer.plData
    }
  where
    seeds = RandomUtils.splits . mkGen 0 3 $ exprPl ^. SugarInfer.plGuid
    types = maybe [] iwcInferredTypes $ exprPl ^. SugarInfer.plInferred

subHoles ::
  (Applicative f, Lens.Contravariant f) =>
  (ExpressionU m a -> f (ExpressionU m a)) ->
  ExpressionU m a -> f void
subHoles x =
  Lens.folding subExpressions . Lens.filtered cond $ x
  where
    cond expr =
      Lens.notNullOf (rBody . _BodyHole) expr ||
      Lens.notNullOf (rBody . _BodyInferred . iValue . subHoles) expr

setNextHole :: Guid -> ExpressionU m a -> ExpressionU m a
setNextHole destGuid =
  -- The mplus ignores holes that are already set:
  Lens.mapped . plMNextHoleGuid %~ (`mplus` Just destGuid)

setNextHoleToFirstSubHole :: MonadA m => ExpressionU m a -> ExpressionU m a -> ExpressionU m a
setNextHoleToFirstSubHole dest =
  maybe id (setNextHole . (^. rPayload . plGuid)) $ dest ^? subHoles

subExpressions :: ExpressionU m a -> [ExpressionU m a]
subExpressions x = x : x ^.. rBody . Lens.traversed . Lens.folding subExpressions

getStoredName :: MonadA m => Guid -> T m (Maybe String)
getStoredName guid = do
  name <- Transaction.getP $ Anchors.assocNameRef guid
  pure $
    if null name then Nothing else Just name
