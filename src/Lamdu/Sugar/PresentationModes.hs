{-# LANGUAGE TypeFamilies #-}
module Lamdu.Sugar.PresentationModes
    ( makeLabeledApply
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Once (OnceT)
import Control.Monad.Trans.Except.Extended (justToLeft, runMatcherT)
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
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)
import           Revision.Deltum.Hyper (iref)

import           Lamdu.Prelude

type T = Transaction

makeLabeledApply ::
    Monad m =>
    Annotated (ConvertPayload m a) # Const (Sugar.BinderVarRef InternalName (T m)) ->
    [ Sugar.AnnotatedArg v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m a)
    ] ->
    [Sugar.PunnedVar InternalName (T m) # Annotated (ConvertPayload m a)] ->
    Input.Payload m a # Term ->
    ConvertM m
    (Sugar.LabeledApply v InternalName (OnceT (T m)) (T m) # Annotated (ConvertPayload m a))
makeLabeledApply func args punnedArgs exprPl =
    do
        presentationMode <- func ^. hVal . Lens._Wrapped . Sugar.bvVar & Anchors.assocPresentationMode & getP
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let mkOperatorArg arg other =
                arg & annotation . pActions . Sugar.delete . Lens.filteredBy Sugar._CannotDelete .~
                    Sugar.Delete
                    ( protectedSetToVal
                        (exprPl ^. Input.stored)
                        (other ^. annotation . pInput . Input.stored . iref)
                        <&> EntityId.ofValI
                    )
        checkOk <- Lens.view ConvertM.scPostProcessRoot
        let swapAction l r =
                do
                    do
                        _ <- DataOps.replace ls (unwrap r)
                        () <$ DataOps.replace rs (unwrap l)
                        & ConvertM.typeProtect checkOk & MaybeT & justToLeft
                    do
                        _ <- DataOps.replace ls (rs ^. iref)
                        () <$ DataOps.replace rs (ls ^. iref)
                        & ConvertM.typeProtect checkOk & MaybeT & justToLeft
                    do
                        maybeWrap ls r
                        maybeWrap rs l
                        & lift
                & runMatcherT
                where
                    maybeWrap d s =
                        case s ^. hVal of
                        Sugar.BodyLeaf Sugar.LeafHole{} -> DataOps.replace d i
                        _ -> DataOps.setToAppliedHole i d
                        & void
                        where
                            i = s ^. annotation . pInput . Input.stored . iref
                    unwrap x =
                        fromMaybe x (x ^? hVal . Sugar._BodyFragment . Sugar.fExpr)
                        ^. annotation . pInput . Input.stored . iref
                    ls = l ^. annotation . pInput . Input.stored
                    rs = r ^. annotation . pInput . Input.stored
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
                ((`notElem` removedKeys) . (^?! Sugar.pvVar . hVal . Lens._Wrapped . SugarLens.getVarName . inTag))
                punnedArgs
            }
    where
        argsMap =
            (args <&> \x -> (x ^. Sugar.aaTag . Sugar.tagVal, x ^. Sugar.aaExpr)) <>
            (punnedArgs <&>
                \x ->
                ( x ^?! Sugar.pvVar . hVal . Lens._Wrapped . SugarLens.getVarName . inTag
                , x ^. Sugar.pvVar . hVal . Lens._Wrapped & Sugar.LeafGetVar & Sugar.BodyLeaf
                    & Ann (Const (x ^. Sugar.pvVar . annotation))
                ))
            & Map.fromList
        argExpr t = Map.lookup t argsMap <&> (,) t
