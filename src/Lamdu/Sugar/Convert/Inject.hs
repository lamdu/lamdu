module Lamdu.Sugar.Convert.Inject
    ( convert
    ) where

import qualified Control.Lens as Lens
import qualified Data.Property as Property
import           Hyper (Tree)
import           Hyper.Type.Ann (Ann(..), ann, val)
import qualified Lamdu.Calc.Term as V
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Config as Config
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.Tag as ConvertTag
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

convert ::
    (Monad m, Monoid a) =>
    Tree (Ann (Input.Payload m a)) V.Inject ->
    ConvertM m (ExpressionU m a)
convert (Ann exprPl (V.Inject tag injected)) =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let typeProtect = protectedSetToVal (exprPl ^. Input.stored) valI
        injectedS <- ConvertM.convertSubexpression injected
        let toNullary =
                do
                    V.BLeaf V.LRecEmpty & ExprIRef.newValI
                        <&> V.Inject tag <&> V.BInject
                        >>= ExprIRef.writeValI valI
                    typeProtect <&> EntityId.ofValI
        nullSugar <-
            Lens.view (ConvertM.scConfig . Config.sugarsEnabled . Config.nullaryInject)
        let inj =
                case injectedS of
                Ann pl
                    (BodyRecord
                     (Composite [] []
                      (ClosedComposite closedCompositeActions) addItem))
                    | nullSugar->
                        NullaryVal closedCompositeActions addItem
                        & Const
                        & Ann pl
                        & InjectNullary
                _ ->
                    injectedS
                    & val . _BodyHole . holeMDelete ?~ toNullary
                    & InjectVal
        let setTag newTag =
                do
                    V.Inject newTag injectedI & V.BInject & ExprIRef.writeValI valI
                    void typeProtect
        ConvertTag.ref tag nameWithoutContext mempty (EntityId.ofTag entityId) setTag
            <&> (`Inject` inj) <&> BodyInject
            >>= addActions [] exprPl
    where
        entityId = exprPl ^. Input.entityId
        valI = exprPl ^. Input.stored . Property.pVal
        injectedI = injected ^. ann . Input.stored . Property.pVal
