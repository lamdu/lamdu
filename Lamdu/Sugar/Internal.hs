module Lamdu.Sugar.Internal
    ( BodyU, ExpressionU
    , replaceWith
    ) where

import           Control.MonadA (MonadA)
import           Data.Store.Guid (Guid)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

type T = Transaction

type BodyU m a = Body Guid m (ExpressionU m a)
type ExpressionU m a = Expression Guid m a

replaceWith ::
    MonadA m => ExprIRef.ValIProperty m -> ExprIRef.ValIProperty m ->
    T m EntityId
replaceWith parentP replacerP =
    do
        Property.set parentP replacerI
        return $ EntityId.ofValI replacerI
    where
        replacerI = Property.value replacerP
