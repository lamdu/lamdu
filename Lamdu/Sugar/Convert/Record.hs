module Lamdu.Sugar.Convert.Record
  ( convertEmpty, convertExtend
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad (void)
import Control.MonadA (MonadA)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Lamdu.Expr.Val (Val(..))
import Lamdu.Sugar.Convert.Expression.Actions (addActions)
import Lamdu.Sugar.Convert.Monad (ConvertM)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens
import qualified Data.Store.Property as Property
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

plValI :: Lens.Traversal' (Input.Payload m a) (ExprIRef.ValI m)
plValI = Input.mStored . Lens._Just . Property.pVal

convertTag :: EntityId -> T.Tag -> TagG Guid
convertTag inst tag = TagG inst tag $ UniqueId.toGuid tag

convertField ::
  (MonadA m, Monoid a) =>
  Maybe (ExprIRef.ValIProperty m) ->
  Maybe (ExprIRef.ValI m) -> Record name m (ExpressionU m a) ->
  EntityId -> T.Tag -> Val (Input.Payload m a) ->
  ConvertM m (RecordField Guid m (ExpressionU m a))
convertField mStored mRestI restS inst tag expr = do
  exprS <- ConvertM.convertSubexpression expr
  typeProtect <- ConvertM.typeProtectTransaction
  protectedSetToVal <- ConvertM.typeProtectedSetToVal
  return RecordField
    { _rfTag = convertTag inst tag
    , _rfExpr = exprS
    , _rfMDelete =
        do
          stored <- mStored
          restI <- mRestI
          exprI <- expr ^? V.payload . plValI
          return $
            if null (restS ^. rItems)
            then
              fmap EntityId.ofValI $ protectedSetToVal stored =<<
              case restS ^. rTail of
              ClosedRecord{}
                | Lens.has (rBody . _BodyHole) exprS ->
                    ExprIRef.newVal $ Val () $ V.BLeaf V.LRecEmpty
                | otherwise ->
                    -- When deleting closed one field record
                    -- we replace the record with the field value
                    -- (unless it is a hole)
                    return exprI
              RecordExtending{} -> return restI
            else do
              let delete = DataOps.replace stored restI
              mResult <- fmap EntityId.ofValI <$> typeProtect delete
              case mResult of
                Just result -> return result
                Nothing ->
                  fromMaybe (error "should have a way to fix type error") $
                  case restS ^. rTail of
                  RecordExtending ext ->
                    ext ^? rPayload . plActions . Lens._Just . wrap . _WrapAction
                    <&> fmap snd
                  ClosedRecord mOpen -> (delete >>) <$> mOpen
    }

makeAddField :: MonadA m =>
  Maybe (ExprIRef.ValIProperty m) ->
  ConvertM m (Maybe (Transaction m EntityId))
makeAddField Nothing = return Nothing
makeAddField (Just stored) =
  do
    typeProtect <- ConvertM.typeProtectTransaction
    return . Just . fmap (EntityId.ofRecExtendTag . EntityId.ofValI) $ do
      let extend = snd <$> DataOps.recExtend stored
      mResultI <- typeProtect extend
      case mResultI of
        Just resultI -> return resultI
        Nothing -> do
          resultI <- extend
          void $ DataOps.setToWrapper resultI stored
          return resultI

convertEmpty :: MonadA m => Input.Payload m a -> ConvertM m (ExpressionU m a)
convertEmpty exprPl = do
  mAddField <- makeAddField (exprPl ^. Input.mStored)
  BodyRecord Record
    { _rItems = []
    , _rTail =
        ClosedRecord $
        fmap EntityId.ofValI . DataOps.replaceWithHole <$> exprPl ^. Input.mStored
    , _rMAddField = mAddField
    }
    & addActions exprPl

convertExtend ::
  (MonadA m, Monoid a) => V.RecExtend (Val (Input.Payload m a)) ->
  Input.Payload m a -> ConvertM m (ExpressionU m a)
convertExtend (V.RecExtend tag val rest) exprPl = do
  restS <- ConvertM.convertSubexpression rest
  (restRecord, hiddenEntities) <-
    case restS ^. rBody of
    BodyRecord r -> return (r, restS ^. rPayload . plData)
    _ -> do
      mAddField <- makeAddField (rest ^. V.payload . Input.mStored)
      return
        ( Record [] (RecordExtending restS) mAddField
        , mempty
        )
  fieldS <-
    convertField
    (exprPl ^. Input.mStored) (rest ^? V.payload . plValI) restRecord
    (EntityId.ofRecExtendTag (exprPl ^. Input.entityId)) tag val
  restRecord
    & rItems %~ (fieldS:)
    & BodyRecord
    & addActions exprPl
    <&> rPayload . plData <>~ hiddenEntities
