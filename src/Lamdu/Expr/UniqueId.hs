{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Expr.UniqueId
    ( ToUUID(..), UniqueId(..), identifierOfUUID, varOfUUID
    ) where

import           Data.Store.IRef (IRef)
import qualified Data.Store.IRef as IRef
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.UUID.Types (UUID)
import qualified Data.UUID.Utils as UUIDUtils
import           Lamdu.Calc.Identifier (Identifier(..))
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Val as V
import           Lamdu.Expr.IRef (ValI(..))
import qualified Lamdu.Expr.IRef as ExprIRef

import           Lamdu.Prelude

uuidOfIdentifier :: Identifier -> UUID
uuidOfIdentifier (Identifier bs) = UUIDUtils.fromSBS16 bs

identifierOfUUID :: UUID -> Identifier
identifierOfUUID = Identifier . UUIDUtils.toSBS16

varOfUUID :: UUID -> V.Var
varOfUUID = V.Var . identifierOfUUID

class    ToUUID a           where toUUID :: a -> UUID
instance ToUUID V.Var       where toUUID = uuidOfIdentifier . V.vvName
instance ToUUID T.Tag       where toUUID = uuidOfIdentifier . T.tagName
instance ToUUID T.NominalId where toUUID = uuidOfIdentifier . T.nomId
instance ToUUID T.ParamId   where toUUID = uuidOfIdentifier . T.typeParamId
instance ToUUID (IRef m a)  where toUUID = IRef.uuid
instance ToUUID (ValI m)    where toUUID = toUUID . ExprIRef.unValI

-- TODO: Remove this when all code uses more descritive types than UUID
instance ToUUID UUID  where toUUID = id

mkNew :: Monad m => (Identifier -> a) -> Transaction m a
mkNew f = f . identifierOfUUID <$> Transaction.newKey

-- NOTE: No other code in Lamdu should be creating var or tag ids!
class ToUUID a => UniqueId a     where new :: Monad m => Transaction m a
instance          UniqueId V.Var where new = mkNew V.Var
instance          UniqueId T.Tag where new = mkNew T.Tag
instance          UniqueId T.NominalId  where new = mkNew T.NominalId
