{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving #-}
module Lamdu.Sugar.Internal.EntityId
    ( EntityId
    , bs
    , ofValI, ofIRef
    , ofBinder
    , ofTag
    , ofTaggedEntity
    , ofTId
    , randomizeExprAndParams
    ) where

import           Data.Binary.Utils (encodeS)
import           Data.Hashable (Hashable)
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Utils as UUIDUtils
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Expr.GenIds as GenIds
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Revision.Deltum.IRef (IRef)

import           System.Random (RandomGen)

import           Lamdu.Prelude

newtype EntityId = EntityId UUID
    deriving (Eq, Ord, Hashable, Show)

bs :: EntityId -> ByteString
bs (EntityId uuid) = UUIDUtils.toSBS16 uuid

randomizeExprAndParams ::
    RandomGen gen => gen -> Val (EntityId -> a) -> Val a
randomizeExprAndParams gen =
    GenIds.randomizeExprAndParams gen . fmap (. EntityId)

augment :: ByteString -> EntityId -> EntityId
augment str (EntityId x) = EntityId $ UUIDUtils.augment str x

ofIRef :: IRef m a -> EntityId
ofIRef = EntityId . UniqueId.toUUID

ofValI :: ExprIRef.ValI m -> EntityId
ofValI = ofIRef . ExprIRef.unValI

ofTId :: T.NominalId -> EntityId
ofTId = EntityId . UniqueId.toUUID

ofBinder :: V.Var -> EntityId
ofBinder = EntityId . UniqueId.toUUID

-- For tag instance entity id
ofTaggedEntity :: UniqueId.ToUUID a => a -> T.Tag -> EntityId
ofTaggedEntity v p =
    EntityId $ UUIDUtils.combine (UniqueId.toUUID v) (UniqueId.toUUID p)

-- For tag instance entity id
ofTag :: EntityId -> T.Tag -> EntityId
ofTag entityId tag = augment (encodeS tag) entityId
