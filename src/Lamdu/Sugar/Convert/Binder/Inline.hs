{-# LANGUAGE TupleSections #-}
module Lamdu.Sugar.Convert.Binder.Inline
    ( inlineLet
    ) where

import           AST (monoChildren)
import           AST.Functor.Ann (Ann(..), ann, val, annotations)
import qualified Control.Lens as Lens
import qualified Data.Property as Property
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Expr.IRef (ValP, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Binder.Redex (Redex(..))
import qualified Lamdu.Sugar.Convert.Binder.Redex as Redex
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

redexes :: Val a -> ([(V.Var, Val a)], Val a)
redexes (Ann _ (V.BApp (V.Apply (Ann _ (V.BLam lam)) arg))) =
    redexes (lam ^. V.lamResult)
    & _1 %~ (:) (lam ^. V.lamParamId, arg)
redexes v = ([], v)

wrapWithRedexes :: [(V.Var, Val (Maybe a))] -> Val (Maybe a) -> Val (Maybe a)
wrapWithRedexes rs x =
    foldr wrapWithRedex x rs
    where
        wrapWithRedex (v, a) b =
            V.Apply (Ann Nothing (V.BLam (V.Lam v b))) a
            & V.BApp
            & Ann Nothing

inlineLetH :: V.Var -> Val (Maybe a) -> Val (Maybe a) -> Val (Maybe a)
inlineLetH var arg bod =
    go bod & uncurry wrapWithRedexes
    where
        go (Ann stored b) =
            case (b, arg ^. val) of
            (V.BLeaf (V.LVar v), _) | v == var -> redexes arg
            (V.BApp (V.Apply (Ann _ (V.BLeaf (V.LVar v))) a)
              , V.BLam (V.Lam param lamBody))
              | v == var ->
                redexes lamBody
                & _1 %~ (:) (param, a)
            (V.BLam (V.Lam param lamBody), _) ->
                ( []
                , go lamBody & uncurry wrapWithRedexes
                  & V.Lam param & V.BLam & Ann stored
                )
            _ ->
                ( r ^.. monoChildren . Lens._Wrapped . _1 . traverse
                , r & monoChildren %~ (^. Lens._Wrapped . _2) & Ann stored
                )
                where
                    r = b & monoChildren %~ Lens.Const . go

cursorDest :: Val a -> a
cursorDest x =
    case x ^. val of
    V.BLam lam -> lam ^. V.lamResult
    _ -> x
    & redexes
    & (^. _2 . ann)

inlineLet ::
    Monad m => ValP m -> Redex (ValI m) -> T m EntityId
inlineLet topLevelProp redex =
    Property.value topLevelProp & ExprIRef.readVal
    <&> (^? val . V._BApp . V.applyFunc . val . V._BLam . V.lamResult . ann)
    <&> fromMaybe (error "malformed redex")
    >>= ExprIRef.readVal
    <&> annotations %~ Just
    <&> inlineLetH
        (redex ^. Redex.lam . V.lamParamId)
        (redex ^. Redex.arg & annotations %~ Just)
    <&> annotations %~ (, ())
    >>= ExprIRef.writeValWithStoredSubexpressions
    <&> (^. ann . _1)
    >>= Property.set topLevelProp
    & (cursorDest (redex ^. Redex.arg & annotations %~ EntityId.ofValI) <$)
