module Trash where

import Data.Store.Guid (Guid)
import Lamdu.Expr.Identifier (Identifier(..))
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Expr.Val as V

guidOfIdentifier :: Identifier -> Guid
guidOfIdentifier (Identifier bs) = Guid.make bs

identifierOfGuid :: Guid -> Identifier
identifierOfGuid = Identifier . Guid.bs

varOfGuid :: Guid -> V.Var
varOfGuid = V.Var . identifierOfGuid

guidOfVar :: V.Var -> Guid
guidOfVar = guidOfIdentifier . V.vvName
