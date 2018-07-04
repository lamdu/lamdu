module Lamdu.Sugar.PresentationModes
    ( makeLabeledApply
    ) where

import           Data.Either (partitionEithers)
import qualified Data.Map as Map
import qualified Data.Property as Property
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

makeLabeledApply ::
    Monad m =>
    Sugar.Node (Sugar.BinderVarRef InternalName (T m)) (ConvertPayload m a) ->
    [Sugar.AnnotatedArg InternalName (Sugar.Expression InternalName (T m) (T m) (ConvertPayload m a))] ->
    T m (Sugar.LabeledApply InternalName (T m) (T m) (ConvertPayload m a))
makeLabeledApply func args =
    func ^. Sugar.val . Sugar.bvVar
    & Anchors.assocPresentationMode & Property.getP
    <&> \presentationMode ->
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
    in  Sugar.LabeledApply
        { Sugar._aFunc = func
        , Sugar._aSpecialArgs = specialArgs
        , Sugar._aAnnotatedArgs = annotatedArgs
        , Sugar._aRelayedArgs = relayedArgs
        }
    where
        argsMap =
            args
            <&> (\x -> (x ^. Sugar.aaTag . Sugar.tagVal, x))
            & Map.fromList
        argExpr t = Map.lookup t argsMap <&> (^. Sugar.aaExpr) <&> (,) t
        mkInfixArg arg other =
            arg
            & Sugar.body . Sugar._BodyHole . Sugar.holeMDelete .~
                other ^. Sugar.annotation . pActions . Sugar.mReplaceParent
        processArg arg =
            do
                getVar <- arg ^? Sugar.aaExpr . Sugar.body . Sugar._BodyGetVar
                name <-
                    case getVar of
                    Sugar.GetParam x -> x ^. Sugar.pNameRef . Sugar.nrName & Just
                    Sugar.GetBinder x -> x ^. Sugar.bvNameRef . Sugar.nrName & Just
                    Sugar.GetParamsRecord _ -> Nothing
                _ <- internalNameMatch (arg ^. Sugar.aaTag . Sugar.tagName) name
                Right Sugar.Node
                    { Sugar._val = getVar
                    , Sugar._ann = arg ^. Sugar.aaExpr . Sugar.annotation
                    } & Just
            & fromMaybe (Left arg)
