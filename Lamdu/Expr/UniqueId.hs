module Lamdu.Expr.UniqueId
  ( ToGuid(..), UniqueId(..)
  ) where

import Control.Applicative ((<$>))
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Lamdu.Expr.Identifier (Identifier(..))
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

guidOfIdentifier :: Identifier -> Guid
guidOfIdentifier (Identifier bs) = Guid.make bs

identifierOfGuid :: Guid -> Identifier
identifierOfGuid = Identifier . Guid.bs

class    ToGuid a          where toGuid :: a -> Guid
instance ToGuid V.Var      where toGuid = guidOfIdentifier . V.vvName
instance ToGuid T.Tag      where toGuid = guidOfIdentifier . T.tagName
instance ToGuid T.Id       where toGuid = guidOfIdentifier . T.typeId
instance ToGuid T.ParamId  where toGuid = guidOfIdentifier . T.typeParamId
instance ToGuid (IRef t a) where toGuid = IRef.guid

-- TODO: Remove this when all code uses more descritive types than Guid
instance ToGuid Guid  where toGuid = id

mkNew :: MonadA m => (Identifier -> a) -> Transaction m a
mkNew f = f . identifierOfGuid <$> Transaction.newKey

-- NOTE: No other code in Lamdu should be creating var or tag ids!
class ToGuid a => UniqueId a     where new :: MonadA m => Transaction m a
instance          UniqueId V.Var where new = mkNew V.Var
instance          UniqueId T.Tag where new = mkNew T.Tag
instance          UniqueId T.Id  where new = mkNew T.Id
