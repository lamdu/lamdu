{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lamdu.Sugar.Internal.EntityId
    ( EntityId(..)
    , bs
    , ofValI, ofIRef
    , ofBinder
    , ofTag
    , ofTaggedEntity
    , ofTId
    , ofFragmentUnder
    , randomizeExprAndParams
    , ofEvalOf, ofEvalField, ofEvalArrayIdx
    , ofTypeOf, ofRestOfComposite, ofFunParam, ofFunResult, ofTInstParam
    , usedTypeOf, currentTypeOf
    ) where

import           Data.Binary.Extended (encodeS)
import qualified Data.ByteString as BS
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

fromUniqueId :: UniqueId.ToUUID a => a -> EntityId
fromUniqueId = EntityId . UniqueId.toUUID

ofIRef :: IRef m a -> EntityId
ofIRef = fromUniqueId

ofValI :: ExprIRef.ValI m -> EntityId
ofValI = fromUniqueId

ofTId :: T.NominalId -> EntityId
ofTId = fromUniqueId

ofBinder :: V.Var -> EntityId
ofBinder = fromUniqueId

-- For tag instance entity id
ofTaggedEntity :: UniqueId.ToUUID a => a -> T.Tag -> EntityId
ofTaggedEntity v p =
    EntityId $ UUIDUtils.combine (UniqueId.toUUID v) (UniqueId.toUUID p)

-- For tag instance entity id
ofTag :: EntityId -> T.Tag -> EntityId
ofTag entityId tag = augment (encodeS tag) entityId

ofEvalField :: T.Tag -> EntityId -> EntityId
ofEvalField p (EntityId uuid) =
    EntityId $ UUIDUtils.combine (UniqueId.toUUID p) uuid

ofEvalArrayIdx :: Int -> EntityId -> EntityId
ofEvalArrayIdx idx = augment (BS.pack [fromIntegral idx])

ofEvalOf :: EntityId -> EntityId
ofEvalOf = augment "evalOf"

ofTypeOf :: EntityId -> EntityId
ofTypeOf = augment "typeOf"

ofRestOfComposite :: EntityId -> EntityId
ofRestOfComposite = augment "restOfComposite"

ofFunParam :: EntityId -> EntityId
ofFunParam = augment "TFunParam"

ofFunResult :: EntityId -> EntityId
ofFunResult = augment "TFunResult"

ofFragmentUnder :: Int -> EntityId -> EntityId
ofFragmentUnder idx = augment (BS.pack [fromIntegral idx]) . augment "Fragment"

ofTInstParam :: T.ParamId -> EntityId -> EntityId
ofTInstParam p (EntityId uuid) = EntityId $ UUIDUtils.combine (UniqueId.toUUID p) uuid

currentTypeOf :: EntityId -> EntityId
currentTypeOf = ofTypeOf

usedTypeOf :: EntityId -> EntityId
usedTypeOf = augment "usedTypeOf"
