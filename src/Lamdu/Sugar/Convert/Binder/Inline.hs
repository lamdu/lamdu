{-# LANGUAGE TupleSections, TypeFamilies #-}
module Lamdu.Sugar.Convert.Binder.Inline
    ( inlineLet
    ) where

import qualified Control.Lens as Lens
import qualified Data.Property as Property
import           Hyper
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
redexes (Ann _ (V.BApp (V.App (Ann _ (V.BLam lam)) arg))) =
    redexes (lam ^. V.lamOut)
    & _1 %~ (:) (lam ^. V.lamIn, arg)
redexes v = ([], v)

wrapWithRedexes :: [(V.Var, Val (Maybe a))] -> Val (Maybe a) -> Val (Maybe a)
wrapWithRedexes rs x =
    foldr wrapWithRedex x rs
    where
        wrapWithRedex (v, a) b =
            V.App (Ann (Const Nothing) (V.BLam (V.Lam v b))) a
            & V.BApp
            & Ann (Const Nothing)

inlineLetH :: V.Var -> Val (Maybe a) -> Val (Maybe a) -> Val (Maybe a)
inlineLetH var arg bod =
    go bod & uncurry wrapWithRedexes
    where
        go (Ann stored b) =
            case (b, arg ^. hVal) of
            (V.BLeaf (V.LVar v), _) | v == var -> redexes arg
            (V.BApp (V.App (Ann _ (V.BLeaf (V.LVar v))) a)
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
                ( r ^.. htraverse1 . Lens._Wrapped . _1 . traverse
                , r & htraverse1 %~ (^. Lens._Wrapped . _2) & Ann stored
                )
                where
                    r = b & htraverse1 %~ Lens.Const . go

cursorDest :: Val a -> a
cursorDest x =
    case x ^. hVal of
    V.BLam lam -> lam ^. V.lamOut
    _ -> x
    & redexes
    & (^. _2 . annotation)

inlineLet ::
    Monad m => ValP m -> Redex (ValI m) -> T m EntityId
inlineLet topLevelProp redex =
    Property.value topLevelProp & ExprIRef.readVal
    <&> (^? hVal . V._BApp . V.appFunc . hVal . V._BLam . V.lamOut . hAnn)
    <&> fromMaybe (error "malformed redex")
    >>= ExprIRef.readVal
    <&> Lens.from _HFlip . hmapped1 %~ Const . Just
    <&> inlineLetH
        (redex ^. Redex.lam . V.lamIn)
        (redex ^. Redex.arg & Lens.from _HFlip . hmapped1 . Lens._Wrapped %~ Just)
    <&> Lens.from _HFlip . hmapped1 . Lens._Wrapped %~ (, ())
    >>= ExprIRef.writeValWithStoredSubexpressions
    <&> (^. annotation . _1)
    >>= Property.set topLevelProp
    & (cursorDest (redex ^. Redex.arg & Lens.from _HFlip . hmapped1 . Lens._Wrapped %~ EntityId.ofValI) <$)
