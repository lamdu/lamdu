{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Lamdu.Sugar.Internal.EntityId
    ( EntityId
    , bs
    , ofValI, ofIRef
    , ofLambdaParam
    , ofLambdaTagParam
    , ofInjectTag
    , ofGetFieldTag
    , ofRecExtendTag
    , ofCaseTag
    , ofTId
    , randomizeExprAndParams
    ) where

import           Data.Hashable (Hashable)
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Utils as UUIDUtils
import           Data.Store.IRef (IRef)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Expr.GenIds as GenIds
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.UniqueId as UniqueId
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

ofLambdaParam :: V.Var -> EntityId
ofLambdaParam = EntityId . UniqueId.toUUID

ofLambdaTagParam :: V.Var -> T.Tag -> EntityId
ofLambdaTagParam v p =
    EntityId $ UUIDUtils.combine (UniqueId.toUUID v) (UniqueId.toUUID p)

ofInjectTag :: EntityId -> EntityId
ofInjectTag = augment "tag"

ofGetFieldTag :: EntityId -> EntityId
ofGetFieldTag = augment "tag"

ofRecExtendTag :: EntityId -> EntityId
ofRecExtendTag = augment "tag"

ofCaseTag :: EntityId -> EntityId
ofCaseTag = augment "tag"
