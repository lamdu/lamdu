module Lamdu.Sugar.Convert.Expression
  ( make, mkGen
  , mkReplaceWithNewHole
  , getStoredName
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (zipWithM)
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Typeable (Typeable1)
import Lamdu.Data.Expression.Infer.Conflicts (iwcInferredTypes)
import Lamdu.Sugar.Convert.Infer (Stored)
import Lamdu.Sugar.Convert.Monad (SugarM)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import Lamdu.Sugar.Types.Internal
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.Store.Guid as Guid
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Sugar.Convert.Infer as SugarInfer
import qualified Lamdu.Sugar.Convert.Monad as SugarM
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
  { _storedGuid = ExprIRef.exprGuid $ Property.value stored
  , _wrap = WrapAction $ ExprIRef.exprGuid <$> DataOps.wrap stored
  , _mSetToHole = Just $ ExprIRef.exprGuid <$> DataOps.setToHole stored
  , _mSetToInnerExpr = Nothing
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
    , _plData = exprPl ^. SugarInfer.plData
    }
  where
    seeds = RandomUtils.splits . mkGen 0 3 $ exprPl ^. SugarInfer.plGuid
    types = maybe [] iwcInferredTypes $ exprPl ^. SugarInfer.plInferred

getStoredName :: MonadA m => Guid -> T m (Maybe String)
getStoredName guid = do
  name <- Transaction.getP $ Anchors.assocNameRef guid
  pure $
    if null name then Nothing else Just name
