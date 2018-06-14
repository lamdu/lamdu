{-# LANGUAGE TupleSections #-}
module Lamdu.Sugar.Convert.Binder.Inline
    ( inlineLet
    ) where

import qualified Control.Lens as Lens
import qualified Data.Property as Property
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
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
redexes (Val _ (V.BApp (V.Apply (Val _ (V.BLam lam)) arg))) =
    redexes (lam ^. V.lamResult)
    & _1 %~ (:) (lam ^. V.lamParamId, arg)
redexes v = ([], v)

wrapWithRedexes :: [(V.Var, Val (Maybe a))] -> Val (Maybe a) -> Val (Maybe a)
wrapWithRedexes rs x =
    foldr wrapWithRedex x rs
    where
        wrapWithRedex (v, val) b =
            V.Apply (Val Nothing (V.BLam (V.Lam v b))) val
            & V.BApp
            & Val Nothing

inlineLetH :: V.Var -> Val (Maybe a) -> Val (Maybe a) -> Val (Maybe a)
inlineLetH var arg bod =
    go bod & uncurry wrapWithRedexes
    where
        go (Val stored b) =
            case (b, arg ^. Val.body) of
            (V.BLeaf (V.LVar v), _) | v == var -> redexes arg
            (V.BApp (V.Apply (Val _ (V.BLeaf (V.LVar v))) a)
              , V.BLam (V.Lam param lamBody))
              | v == var ->
                redexes lamBody
                & _1 %~ (:) (param, a)
            (V.BLam (V.Lam param lamBody), _) ->
                ( []
                , go lamBody & uncurry wrapWithRedexes
                  & V.Lam param & V.BLam & Val stored
                )
            _ ->
                ( r ^.. Lens.traverse . _1 . Lens.traverse
                , r <&> (^. _2) & Val stored
                )
                where
                    r = b <&> go

cursorDest :: Val a -> a
cursorDest val =
    case val ^. Val.body of
    V.BLam lam -> lam ^. V.lamResult
    _ -> val
    & redexes
    & (^. _2 . Val.payload)

inlineLet ::
    Monad m => ValP m -> Redex (ValI m) -> T m EntityId
inlineLet topLevelProp redex =
    Property.value topLevelProp & ExprIRef.readVal
    <&> (^? Val.body . V._BApp . V.applyFunc . Val.body . V._BLam . V.lamResult . Val.payload)
    <&> fromMaybe (error "malformed redex")
    >>= ExprIRef.readVal
    <&> Lens.mapped %~ Just
    <&> inlineLetH
        (redex ^. Redex.lam . V.lamParamId)
        (redex ^. Redex.arg <&> Just)
    <&> Lens.mapped %~ (, ())
    >>= ExprIRef.writeValWithStoredSubexpressions
    <&> (^. Val.payload . _1)
    >>= Property.set topLevelProp
    <&> const (cursorDest (redex ^. Redex.arg <&> EntityId.ofValI))
