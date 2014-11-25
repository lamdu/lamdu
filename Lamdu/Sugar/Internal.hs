module Lamdu.Sugar.Internal
  ( BodyU, ExpressionU
  , replaceWith
  ) where

import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Lamdu.Sugar.Types
import qualified Data.Store.Property as Property
import qualified Lamdu.Expr.IRef as ExprIRef

type T = Transaction

type BodyU m a = Body MStoredName m (ExpressionU m a)
type ExpressionU m a = Expression MStoredName m a

replaceWith ::
    MonadA m => ExprIRef.ValIProperty m -> ExprIRef.ValIProperty m -> T m Guid
replaceWith parentP replacerP = do
  Property.set parentP replacerI
  return $ ExprIRef.valIGuid replacerI
  where
    replacerI = Property.value replacerP
