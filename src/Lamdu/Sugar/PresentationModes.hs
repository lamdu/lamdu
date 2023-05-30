{-# LANGUAGE TypeFamilies #-}
module Lamdu.Sugar.PresentationModes
    ( makeLabeledApply
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT)
import           Control.Monad.Trans.Except.Extended (justToLeft, runMatcherT)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Transaction (getP)
import qualified Data.Map as Map
import           Lamdu.Calc.Term (Term)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)
import           Revision.Deltum.Hyper (iref)

import           Lamdu.Prelude

type T = Transaction

makeLabeledApply ::
    Monad m =>
    Annotated (ConvertPayload m) # Const (Sugar.GetVar InternalName (T m)) ->
    [ Sugar.AnnotatedArg v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m)
    ] ->
    [Sugar.PunnedVar InternalName (T m) # Annotated (ConvertPayload m)] ->
    Input.Payload m # Term ->
    ConvertM m
    (Sugar.LabeledApply v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m))
makeLabeledApply func args punnedArgs exprPl =
    do
        presentationMode <- func ^. hVal . Lens._Wrapped . Sugar.vVar & Anchors.assocPresentationMode & getP
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let mkOperatorArg arg other =
                arg & annotation . pActions . Sugar.delete . Lens.filteredBy Sugar._CannotDelete .~
                    Sugar.Delete
                    ( protectedSetToVal
                        (exprPl ^. Input.stored)
                        (other ^. annotation . pStored . iref)
                        <&> EntityId.ofValI
                    )
        checkOk <- Lens.view ConvertM.scPostProcessRoot
        let swapAction l r =
                do
                    do
                        _ <- DataOps.replace ls ru
                        (c0 || c1) <$ DataOps.replace rs lu
                        & ConvertM.typeProtect checkOk & MaybeT & justToLeft
                    do
                        _ <- DataOps.replace ls (rs ^. iref)
                        False <$ DataOps.replace rs (ls ^. iref)
                        & ConvertM.typeProtect checkOk & MaybeT & justToLeft
                    do
                        maybeWrap ls r
                        maybeWrap rs l
                        pure True
                        & ConvertM.typeProtect checkOk & MaybeT & justToLeft
                    error "no swap action successfully type checked" & lift
                & runMatcherT
                -- Work-around red-cursor in some cases when cursor is on record params.
                <&> (if hasRecordParams then const True else id)
                where
                    (c0, ru) = unwrap r
                    (c1, lu) = unwrap l
                    maybeWrap d s =
                        case s ^. hVal of
                        Sugar.BodyLeaf Sugar.LeafHole{} -> DataOps.replace d i
                        Sugar.BodyFragment{} -> DataOps.replace d i
                        _ -> DataOps.setToAppliedHole i d
                        & void
                        where
                            i = s ^. annotation . pStored . iref
                    unwrap x =
                        case x ^? hVal . Sugar._BodyFragment . Sugar.fExpr of
                        Nothing -> (False, x)
                        Just i -> (True, i)
                        & _2 %~ (^. annotation . pStored . iref)
                    ls = l ^. annotation . pStored
                    rs = r ^. annotation . pStored
                    hasRecordParams =
                        Lens.has (hVal . Sugar._BodyLam . Sugar.lamFunc . Sugar.fParams . Sugar._LhsRecord) r
        let (specialArgs, removedKeys) =
                case traverse argExpr presentationMode of
                Just (Sugar.Operator (l, la) (r, ra)) ->
                    ( Sugar.OperatorArgs (mkOperatorArg la ra) (mkOperatorArg ra la)
                        (swapAction la ra) & Just
                    , [l, r]
                    )
                _ -> (Nothing, [])
        pure Sugar.LabeledApply
            { Sugar._aFunc = func
            , Sugar._aMOpArgs = specialArgs
            , Sugar._aAnnotatedArgs =
                filter ((`notElem` removedKeys) . (^. Sugar.aaTag . Sugar.tagVal)) args
            , Sugar._aPunnedArgs =
                filter
                ((`notElem` removedKeys) . (^?! Sugar.pvVar . hVal . Lens._Wrapped . Sugar.vName . inTag))
                punnedArgs
            }
    where
        argsMap =
            (args <&> \x -> (x ^. Sugar.aaTag . Sugar.tagVal, x ^. Sugar.aaExpr)) <>
            (punnedArgs <&>
                \x ->
                ( x ^?! Sugar.pvVar . hVal . Lens._Wrapped . Sugar.vName . inTag
                , x ^. Sugar.pvVar . hVal . Lens._Wrapped & Sugar.LeafGetVar & Sugar.BodyLeaf
                    & Ann (Const (x ^. Sugar.pvVar . annotation))
                ))
            & Map.fromList
        argExpr t = Map.lookup t argsMap <&> (,) t
