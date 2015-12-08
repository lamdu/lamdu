{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Binder.Inline
    ( inlineLet
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import           Lamdu.Expr.IRef (ValIProperty, ValI)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Prelude.Compat

insideRedexes :: (Val a -> Val a) -> Val a -> Val a
insideRedexes f (Val a (V.BApp (V.Apply (V.Val l (V.BAbs lam)) arg))) =
    lam
    & V.lamResult %~ insideRedexes f
    & V.BAbs & Val l
    & flip V.Apply arg & V.BApp & Val a
insideRedexes f expr = f expr

redexes :: Val a -> ([(V.Var, Val a)], Val a)
redexes (Val _ (V.BApp (V.Apply (V.Val _ (V.BAbs lam)) arg))) =
    redexes (lam ^. V.lamResult)
    & _1 %~ (:) (lam ^. V.lamParamId, arg)
redexes v = ([], v)

inlineLetH :: V.Var -> Val (Maybe a) -> Val (Maybe a) -> Val (Maybe a)
inlineLetH var redexArg redexBody =
    foldr wrapWithRedex newBody innerRedexes
    where
        (innerRedexes, newBody) = go redexBody
        go (Val stored body) =
            case (body, redexArg ^. V.body) of
            (V.BLeaf (V.LVar v), _) | v == var -> redexes redexArg
            (V.BApp (V.Apply (Val _ (V.BLeaf (V.LVar v))) arg)
              , V.BAbs (V.Lam param lamBody))
              | v == var ->
                redexes lamBody
                & _1 %~ (:) (param, arg)
            _ ->
                ( r ^.. Lens.traverse . _1 . Lens.traverse
                , r <&> (^. _2) & Val stored
                )
                where
                    r = body <&> go
        wrapWithRedex (v, val) body =
            V.Apply (Val Nothing (V.BAbs (V.Lam v body))) val
            & V.BApp
            & Val Nothing

cursorDest :: Val a -> a
cursorDest val =
    case val ^. V.body of
    V.BAbs lam -> lam ^. V.lamResult
    _ -> val
    & redexes
    & (^. _2 . V.payload)

inlineLet ::
    MonadA m =>
    V.Var -> ValIProperty m -> Val (ValI m) -> Val (ValI m) ->
    Transaction m EntityId
inlineLet var topLevelProp redexBody redexArg =
    redexBody <&> Just
    & insideRedexes (inlineLetH var (redexArg <&> Just))
    <&> flip (,) ()
    & ExprIRef.newValWithStoredSubexpressions
    <&> (^. V.payload . _1)
    >>= Property.set topLevelProp
    <&> const (cursorDest (redexArg <&> EntityId.ofValI))
