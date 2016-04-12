{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Binder.Inline
    ( inlineLet
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import           Lamdu.Expr.IRef (ValIProperty, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Val as V
import           Lamdu.Expr.Val.Annotated (Val(..))
import qualified Lamdu.Expr.Val.Annotated as Val
import           Lamdu.Sugar.Convert.Binder.Redex (Redex(..))
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Prelude.Compat

redexes :: Val a -> ([(V.Var, Val a)], Val a)
redexes (Val _ (V.BApp (V.Apply (Val _ (V.BLam lam)) arg))) =
    redexes (lam ^. V.lamResult)
    & _1 %~ (:) (lam ^. V.lamParamId, arg)
redexes v = ([], v)

wrapWithRedexes :: [(V.Var, Val (Maybe a))] -> Val (Maybe a) -> Val (Maybe a)
wrapWithRedexes rs body =
    foldr wrapWithRedex body rs
    where
        wrapWithRedex (v, val) b =
            V.Apply (Val Nothing (V.BLam (V.Lam v b))) val
            & V.BApp
            & Val Nothing

inlineLetH :: V.Var -> Val (Maybe a) -> Val (Maybe a) -> Val (Maybe a)
inlineLetH var arg body =
    go body & uncurry wrapWithRedexes
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
    Monad m => ValIProperty m -> Redex (ValI m) -> Transaction m EntityId
inlineLet topLevelProp redex =
    redexLam redex ^. V.lamResult
    <&> Just
    & inlineLetH (redexLam redex ^. V.lamParamId) (redexArg redex <&> Just)
    <&> flip (,) ()
    & ExprIRef.writeValWithStoredSubexpressions
    <&> (^. Val.payload . _1)
    >>= Property.set topLevelProp
    <&> const (cursorDest (redexArg redex <&> EntityId.ofValI))
