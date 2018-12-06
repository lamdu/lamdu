module Lamdu.Sugar.Convert.Inject
    ( convert
    ) where

import           AST.Functor.Ann (Ann(..), ann, val)
import           Data.Functor.Const (Const(..))
import qualified Data.Property as Property
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.Tag (convertTag)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import qualified Revision.Deltum.Transaction as Transaction

import           Lamdu.Prelude

convert ::
    (Monad m, Monoid a) =>
    V.Inject (Val (Input.Payload m a)) -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convert (V.Inject tag injected) exprPl =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let typeProtect = protectedSetToVal (exprPl ^. Input.stored) valI
        injectedS <- ConvertM.convertSubexpression injected
        let toNullary =
                do
                    V.BLeaf V.LRecEmpty & Transaction.newIRef
                        <&> V.Inject tag <&> V.BInject
                        >>= Transaction.writeIRef valI
                    typeProtect <&> EntityId.ofValI
        let inj =
                case injectedS of
                Ann pl
                    (BodyRecord
                     (Composite []
                      (ClosedComposite closedCompositeActions) addItem)) ->
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
                    V.Inject newTag injectedI & V.BInject & Transaction.writeIRef valI
                    void typeProtect
        convertTag tag nameWithoutContext mempty (EntityId.ofTag entityId) setTag
            <&> (`Inject` inj) <&> BodyInject
            >>= addActions [] exprPl
    where
        entityId = exprPl ^. Input.entityId
        valI = exprPl ^. Input.stored . Property.pVal
        injectedI = injected ^. ann . Input.stored . Property.pVal
