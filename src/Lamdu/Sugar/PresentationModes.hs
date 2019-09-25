{-# LANGUAGE TypeFamilies #-}
module Lamdu.Sugar.PresentationModes
    ( makeLabeledApply
    ) where

import           Hyper (Tree)
import           Hyper.Type.Ann (Ann(..), ann, val)
import           Control.Lens (Const)
import qualified Control.Lens as Lens
import           Control.Monad.Transaction (getP)
import qualified Data.Map as Map
import qualified Data.Property as Property
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

makeLabeledApply ::
    Monad m =>
    Tree (Ann (ConvertPayload m a)) (Const (Sugar.BinderVarRef InternalName (T m))) ->
    [ Sugar.AnnotatedArg InternalName
        (Sugar.Expression InternalName (T m) (T m) (ConvertPayload m a))
    ] ->
    [Tree (Ann (ConvertPayload m a)) (Const (Sugar.GetVar InternalName (T m)))] ->
    Input.Payload m a ->
    ConvertM m
    (Tree (Sugar.LabeledApply InternalName (T m) (T m)) (Ann (ConvertPayload m a)))
makeLabeledApply func args punnedArgs exprPl =
    do
        presentationMode <- func ^. val . Lens._Wrapped . Sugar.bvVar & Anchors.assocPresentationMode & getP
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let mkInfixArg arg other =
                arg
                & val . Sugar._BodyHole . Sugar.holeMDelete ?~
                    (protectedSetToVal
                        (exprPl ^. Input.stored)
                        (other ^. ann . pInput . Input.stored . Property.pVal)
                        <&> EntityId.ofValI
                    )
        let (specialArgs, removedKeys) =
                case traverse argExpr presentationMode of
                Just (Sugar.Infix (l, la) (r, ra)) ->
                    ( Sugar.Infix (mkInfixArg la ra) (mkInfixArg ra la)
                    , [l, r]
                    )
                Just (Sugar.Object (o, oa)) ->
                    ( Sugar.Object oa
                    , [o]
                    )
                _ -> (Sugar.Verbose, [])
        pure Sugar.LabeledApply
            { Sugar._aFunc = func
            , Sugar._aSpecialArgs = specialArgs
            , Sugar._aAnnotatedArgs =
                filter ((`notElem` removedKeys) . (^. Sugar.aaTag . Sugar.tagVal)) args
            , Sugar._aPunnedArgs =
                filter
                ((`notElem` removedKeys) . (^?! val . Lens._Wrapped . SugarLens.getVarName . inTag))
                punnedArgs
            }
    where
        argsMap =
            args
            <&> (\x -> (x ^. Sugar.aaTag . Sugar.tagVal, x ^. Sugar.aaExpr))
            & Map.fromList
        argExpr t = Map.lookup t argsMap <&> (,) t
