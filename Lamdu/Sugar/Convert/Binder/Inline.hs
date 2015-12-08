{-# LANGUAGE NoImplicitPrelude #-}

module Lamdu.Sugar.Convert.Binder.Inline
    ( inlineLet
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (foldM, foldM_)
import           Control.MonadA (MonadA)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.IRef (ValIProperty)
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

import           Prelude.Compat

redexes :: Val a -> ([(V.Var, Val a)], Val a)
redexes (Val _ (V.BApp (V.Apply (V.Val _ (V.BAbs lam)) arg))) =
    redexes (lam ^. V.lamResult)
    & _1 %~ (:) (lam ^. V.lamParamId, arg)
redexes v = ([], v)

inlineLet ::
    MonadA m =>
    V.Var -> ValIProperty m -> Val (ValIProperty m) -> Val (ValIProperty m) ->
    Transaction m EntityId
inlineLet var topLevelProp redexBodyStored redexArgStored =
    do
        (newBodyI, newLets) <- go redexBodyStored
        case redexes redexBodyStored of
            ([], _) ->
                foldM addLet topLevelProp newLets
                >>= (`Property.set` newBodyI)
            (_, letPos) ->
                do
                    Property.set topLevelProp newBodyI
                    foldM_ addLet (letPos ^. V.payload) newLets
        EntityId.ofValI newBodyI & return
    where
        redexArgI = redexArgStored ^. V.payload & Property.value
        go (Val stored body) =
            case (body, redexArgStored ^. V.body) of
            (V.BLeaf (V.LVar v), _) | v == var ->
                setToBody stored redexArgStored
                <&> (,) redexArgI
            (V.BApp (V.Apply (Val _ (V.BLeaf (V.LVar v))) arg)
              , V.BAbs (V.Lam param lamBody))
              | v == var ->
                setToBody stored lamBody
                <&> (:) (param, Property.value (arg ^. V.payload))
                <&> (,) (lamBody ^. V.payload & Property.value)
            _ ->
                traverse go body
                <&> (^.. Lens.traverse . _2 . Lens.traverse)
                <&> (,) (Property.value stored)
        addLet letPoint (param, val) =
            DataOps.redexWrapWithGivenParam param val letPoint
        setToBody stored expr =
            case expr ^. V.body of
            V.BApp (V.Apply (Val _ (V.BAbs lam)) arg) ->
                setToBody stored (lam ^. V.lamResult)
                <&> (:) (lam ^. V.lamParamId, Property.value (arg ^. V.payload))
            _ ->
                do
                    expr ^. V.payload & Property.value & Property.set stored
                    return []
