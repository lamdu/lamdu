{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.Convert.Inject
    ( convert
    ) where

import qualified Control.Lens as Lens
import qualified Data.Store.Property as Property
import qualified Lamdu.Calc.Val as V
import qualified Lamdu.Calc.Val.Annotated as Val
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.Tag (convertTag)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convert :: (Monad m, Monoid a) => V.Inject (Val (Input.Payload m a)) -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convert (V.Inject tag injected) exprPl =
    do
        mInjectedS <-
            if Lens.has ExprLens.valRecEmpty injected
            then pure Nothing
            else ConvertM.convertSubexpression injected <&> Just
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let setTag newTag =
                do
                    V.Inject newTag injectedI & V.BInject & ExprIRef.writeValBody valI
                    _ <- protectedSetToVal (exprPl ^. Input.stored) valI
                    TagInfo inst newTag & pure
        convertTag (TagInfo inst tag) mempty setTag
            <&> (`Inject` mInjectedS) <&> BodyInject
    >>= addActions exprPl
    where
        entityId = exprPl ^. Input.entityId
        inst = EntityId.ofInjectTag entityId
        valI = exprPl ^. Input.stored . Property.pVal
        injectedI = injected ^. Val.payload . Input.stored . Property.pVal
