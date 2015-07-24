{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Nominal
    ( convertFromNom, convertToNom
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import           Control.MonadA (MonadA)
import           Data.Store.Guid (Guid)
import qualified Data.Store.Property as Property
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.List as ConvertList
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.TIdG as ConvertTIdG
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

convertFromNom ::
    (MonadA m, Monoid a) => V.Nom (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convertFromNom = convert BodyFromNom

convert ::
    (MonadA m, Monoid a) =>
    (Nominal Guid m (ExpressionU m a) -> BodyU m b) ->
    V.Nom (Val (Input.Payload m a)) ->
    Input.Payload m b -> ConvertM m (ExpressionU m b)
convert f (V.Nom tid val) exprPl =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        Nominal
            { _nTId = ConvertTIdG.convert tid
            , _nVal = val
            , _nMDeleteNom =
                protectedSetToVal
                <$> exprPl ^. Input.mStored
                <*> ( val ^. V.payload . Input.mStored
                    <&> Property.value
                    )
                <&> Lens.mapped %~ EntityId.ofValI
            }
            & traverse ConvertM.convertSubexpression
            <&> f
            >>= addActions exprPl

convertToNom ::
    (MonadA m, Monoid a) =>
    V.Nom (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertToNom nom exprPl =
    do
        ConvertList.nil nom exprPl & justToLeft
        ConvertList.cons nom exprPl & justToLeft
        convert BodyToNom nom exprPl & lift
    & runMatcherT
