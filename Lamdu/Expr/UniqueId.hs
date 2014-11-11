module Lamdu.Expr.UniqueId
  ( ToGuid(..), UniqueId(..)
  ) where

import Control.Applicative ((<$>))
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Lamdu.Expr.Identifier (Identifier(..))
import qualified Data.Store.Guid as Guid
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V

guidOfIdentifier :: Identifier -> Guid
guidOfIdentifier (Identifier bs) = Guid.make bs

identifierOfGuid :: Guid -> Identifier
identifierOfGuid = Identifier . Guid.bs

class    ToGuid a     where toGuid :: a -> Guid
instance ToGuid V.Var where toGuid = guidOfIdentifier . V.vvName
instance ToGuid T.Tag where toGuid = guidOfIdentifier . T.tagName

-- TODO: Remove this when all code uses more descritive types than Guid
instance ToGuid Guid  where toGuid = id

-- NOTE: No other code in Lamdu should be creating var or tag ids!
class ToGuid a => UniqueId a     where new :: MonadA m => Transaction m a
instance          UniqueId V.Var where new = V.Var . identifierOfGuid <$> Transaction.newKey
instance          UniqueId T.Tag where new = T.Tag . identifierOfGuid <$> Transaction.newKey
