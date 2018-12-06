module Lamdu.Sugar.PresentationModes
    ( makeLabeledApply
    ) where

import           AST (LeafNode)
import           AST.Functor.Ann (Ann(..), ann, val)
import qualified Control.Lens as Lens
import           Control.Monad.Transaction (getP)
import           Data.Either (partitionEithers)
import           Data.Functor.Const (Const(..))
import qualified Data.Map as Map
import qualified Data.Property as Property
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

makeLabeledApply ::
    Monad m =>
    LeafNode (Ann (ConvertPayload m a)) (Sugar.BinderVarRef InternalName (T m)) ->
    [Sugar.AnnotatedArg InternalName (Sugar.Expression InternalName (T m) (T m) (ConvertPayload m a))] ->
    Input.Payload m a ->
    ConvertM m (Sugar.LabeledApply InternalName (T m) (T m) (Ann (ConvertPayload m a)))
makeLabeledApply func args exprPl =
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
        let (specialArgs, otherArgs) =
                case traverse argExpr presentationMode of
                Just (Sugar.Infix (l, la) (r, ra)) ->
                    ( Sugar.Infix (mkInfixArg la ra) (mkInfixArg ra la)
                    , argsMap
                        & Map.delete l
                        & Map.delete r
                        & Map.elems
                    )
                Just (Sugar.Object (o, oa)) ->
                    ( Sugar.Object oa
                    , Map.delete o argsMap & Map.elems
                    )
                _ -> (Sugar.Verbose, args)
            (annotatedArgs, relayedArgs) = otherArgs <&> processArg & partitionEithers
        pure Sugar.LabeledApply
            { Sugar._aFunc = func
            , Sugar._aSpecialArgs = specialArgs
            , Sugar._aAnnotatedArgs = annotatedArgs
            , Sugar._aRelayedArgs = relayedArgs <&> val %~ Const
            }
    where
        argsMap =
            args
            <&> (\x -> (x ^. Sugar.aaTag . Sugar.tagVal, x))
            & Map.fromList
        argExpr t = Map.lookup t argsMap <&> (^. Sugar.aaExpr) <&> (,) t
        processArg arg =
            do
                getVar <- arg ^? Sugar.aaExpr . val . Sugar._BodyGetVar
                name <-
                    case getVar of
                    Sugar.GetParam x -> x ^. Sugar.pNameRef . Sugar.nrName & Just
                    Sugar.GetBinder x -> x ^. Sugar.bvNameRef . Sugar.nrName & Just
                    Sugar.GetParamsRecord _ -> Nothing
                _ <- internalNameMatch (arg ^. Sugar.aaTag . Sugar.tagName) name
                Right Ann
                    { _val = getVar
                    , _ann = arg ^. Sugar.aaExpr . ann
                    } & Just
            & fromMaybe (Left arg)
