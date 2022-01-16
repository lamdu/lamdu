{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module Lamdu.Sugar.Convert.Binder.Inline
    ( inlineVar
    ) where

import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Type.Functor (_F)
import           Hyper.Type.Prune (Prune(..))
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Expr.IRef (iref)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Hyper (Write(..))
import           Revision.Deltum.IRef (uuid)
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

redexes ::
    Ann a # V.Term ->
    ([(V.Var, Ann a # V.Term)], Ann a # V.Term)
redexes (Ann _ (V.BApp (V.App (Ann _ (V.BLam lam)) arg))) =
    redexes (lam ^. V.tlOut)
    & _1 %~ (:) (lam ^. V.tlIn, arg)
redexes v = ([], v)

wrapWithRedexes ::
    [(V.Var, Ann (Write m) # V.Term)] ->
    Ann (Write m) # V.Term ->
    Ann (Write m) # V.Term
wrapWithRedexes rs x =
    foldr wrapWithRedex x rs
    where
        wrapWithRedex (v, a) b =
            V.TypedLam v (Ann WriteNew (_HCompose # Pruned)) b
            & V.BLam
            & Ann WriteNew
            & (`V.App` a)
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
              , V.BLam (V.TypedLam param _paramTyp lamBody))
              | v == var ->
                redexes lamBody
                & _1 %~ (:) (param, a)
            (V.BLam (V.TypedLam param paramType lamBody), _) ->
                ( []
                , go lamBody & uncurry wrapWithRedexes
                  & V.TypedLam param paramType & V.BLam & Ann stored
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
                            _ -> \(Ann a body) -> Const [] :*: Ann a body
                        ) b

cursorDest :: Val a -> a
cursorDest x =
    case x ^. hVal of
    V.BLam lam -> lam ^. V.tlOut
    _ -> x
    & redexes
    & (^. _2 . annotation)

inlineVar ::
    Monad m =>
    Ann (Input.Payload m) # V.Term -> Maybe (V.Var, EntityId -> VarInline (T m))
inlineVar (Ann topPl (V.BApp (V.App (Ann lamPl (V.BLam lam)) arg))) =
    Just
    ( lam ^. V.tlIn
    , \useId ->
        if all (== useId) uses
        then
            do
                -- Need to re-read body to support inlining hole results!
                bod <-
                    topPl ^. Input.stored . iref & ExprIRef.readRecursively
                    <&> fromMaybe (error "Not a lambda?") . (^? hVal . V._BApp . V.appFunc . hVal . V._BLam . V.tlOut)
                inlineLetH
                    (lam ^. V.tlIn)
                    (arg & hflipped %~ hmap (const (ExistingRef . (^. Input.stored . iref))))
                    (bod & hflipped %~ hmap (const ExistingRef))
                    & hflipped %~ hmap (const (:*: Const ()))
                    & ExprIRef.writeRecursively
                    <&> (^. hAnn . _1)
                    >>= topPl ^. Input.stored . ExprIRef.setIref
                arg
                    & hflipped %~ hmap (const (Const . EntityId.EntityId . uuid . (^. Input.stored . iref . _F)))
                    & cursorDest
                    & pure
            & InlineVar
        else
            let (before, after) = break (== useId) uses
            in CannotInlineDueToUses (drop 1 after <> before)
    )
    where
        uses = lamPl ^. Input.varRefsOfLambda
inlineVar _ = Nothing
