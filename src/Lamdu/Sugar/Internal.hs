module Lamdu.Sugar.Internal
    ( ExpressionU
    , replaceWith
    ) where

import           Data.UUID.Types (UUID)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

type T = Transaction

type ExpressionU m a = Expression UUID m a

replaceWith ::
    Monad m => ExprIRef.ValIProperty m -> ExprIRef.ValIProperty m ->
    T m EntityId
replaceWith parentP replacerP =
    do
        Property.set parentP replacerI
        return $ EntityId.ofValI replacerI
    where
        replacerI = Property.value replacerP
