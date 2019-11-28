{-# LANGUAGE TypeFamilies, PolyKinds, TypeOperators, ScopedTypeVariables #-}
module Lamdu.Sugar.Convert.Binder.Inline
    ( inlineLet
    ) where

import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Type.Functor (F, _F)
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Expr.IRef (HRef)
import qualified Lamdu.Expr.IRef as ExprIRef
import           Lamdu.Sugar.Convert.Binder.Redex (Redex(..))
import qualified Lamdu.Sugar.Convert.Binder.Redex as Redex
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Hyper (Write(..))
import           Revision.Deltum.IRef (IRef, uuid)
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

redexes ::
    Ann a # V.Term ->
    ([(V.Var, Ann a # V.Term)], Ann a # V.Term)
redexes (Ann _ (V.BApp (V.App (Ann _ (V.BLam lam)) arg))) =
    redexes (lam ^. V.lamOut)
    & _1 %~ (:) (lam ^. V.lamIn, arg)
redexes v = ([], v)

wrapWithRedexes ::
    [(V.Var, Ann (Write m) # V.Term)] ->
    Ann (Write m) # V.Term ->
    Ann (Write m) # V.Term
wrapWithRedexes rs x =
    foldr wrapWithRedex x rs
    where
        wrapWithRedex (v, a) b =
            V.App (Ann WriteNew (V.BLam (V.Lam v b))) a
            & V.BApp
            & Ann WriteNew

inlineLetH ::
    forall m.
    V.Var ->
    Ann (Write m) # V.Term ->
    Ann (Write m) # V.Term ->
    Ann (Write m) # V.Term
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
                ( hfoldMap (const (^.. _1 . Lens._Wrapped . traverse)) r
                , hmap (const (^. _2)) r & Ann stored
                )
                where
                    r :: V.Term # (Const [(V.Var, Ann (Write m) # V.Term)] :*: Ann (Write m))
                    r =
                        hmap
                        ( \case
                            HWitness V.W_Term_Term ->
                                go <&> \(res, body) -> Const res :*: body
                        ) b

cursorDest :: Val a -> a
cursorDest x =
    case x ^. hVal of
    V.BLam lam -> lam ^. V.lamOut
    _ -> x
    & redexes
    & (^. _2 . annotation)

inlineLet ::
    Monad m =>
    HRef m # V.Term -> Redex # F (IRef m) -> T m EntityId
inlineLet topLevelProp redex =
    topLevelProp ^. ExprIRef.iref & ExprIRef.readRecursively
    <&> (^? hVal . V._BApp . V.appFunc . hVal . V._BLam . V.lamOut . hAnn)
    <&> fromMaybe (error "malformed redex")
    >>= ExprIRef.readRecursively
    <&> hflipped %~ hmap (const ExistingRef)
    <&> inlineLetH
        (redex ^. Redex.lam . V.lamIn)
        (redex ^. Redex.arg & hflipped %~ hmap (const ExistingRef))
    <&> hflipped %~ hmap (const (:*: Const ()))
    >>= ExprIRef.writeRecursively
    <&> (^. hAnn . _1)
    >>= topLevelProp ^. ExprIRef.setIref
    & (dst <$)
    where
        dst =
            redex ^. Redex.arg
            & hflipped %~ hmap (const (Const . EntityId.EntityId . uuid . (^. _F)))
            & cursorDest
