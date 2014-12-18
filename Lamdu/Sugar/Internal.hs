module Lamdu.Sugar.Internal
  ( BodyU, ExpressionU
  , replaceWith
  , recurseGetVar
  ) where

import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.String (IsString(..))
import Lamdu.Sugar.Types
import qualified Data.Store.Property as Property
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

type T = Transaction

type BodyU m a = Body Guid m (ExpressionU m a)
type ExpressionU m a = Expression Guid m a

replaceWith ::
    MonadA m => ExprIRef.ValIProperty m -> ExprIRef.ValIProperty m ->
    T m EntityId
replaceWith parentP replacerP = do
  Property.set parentP replacerI
  return $ EntityId.ofValI replacerI
  where
    replacerI = Property.value replacerP

recurseGetVar :: V.Var
recurseGetVar = fromString "RECURSE"
