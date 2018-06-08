module Lamdu.Sugar.PresentationModes
    ( addToDef, addToExpr
    ) where

import qualified Control.Lens as Lens
import           Data.Either (partitionEithers)
import qualified Data.Map as Map
import qualified Data.Property as Property
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

addToLabeledApply ::
    Monad m =>
    Sugar.LabeledApply InternalName i o (Sugar.Expression InternalName i o a) ->
    T m (Sugar.LabeledApply InternalName i o (Sugar.Expression InternalName i o a))
addToLabeledApply a =
    case a ^. Sugar.aSpecialArgs of
    Sugar.Verbose ->
        a ^. Sugar.aFunc . Sugar.afVar . Sugar.bvVar
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
                _ -> (Sugar.Verbose, a ^. Sugar.aAnnotatedArgs)
            (annotatedArgs, relayedArgs) = otherArgs <&> processArg & partitionEithers
        in  a
            & Sugar.aSpecialArgs .~ specialArgs
            & Sugar.aAnnotatedArgs .~ annotatedArgs
            & Sugar.aRelayedArgs .~ relayedArgs
    _ -> pure a
    where
        argsMap =
            a ^. Sugar.aAnnotatedArgs
            <&> (\x -> (x ^. Sugar.aaTag . Sugar.tagVal, x))
            & Map.fromList
        argExpr t = Map.lookup t argsMap <&> (^. Sugar.aaExpr) <&> (,) t
        mkInfixArg arg other =
            arg
            & Sugar.body . Sugar._BodyHole . Sugar.holeMDelete .~
                other ^. Sugar.annotation . Sugar.plActions . Sugar.mReplaceParent
        processArg arg =
            do
                getVar <- arg ^? Sugar.aaExpr . Sugar.body . Sugar._BodyGetVar
                name <-
                    case getVar of
                    Sugar.GetParam x -> x ^. Sugar.pNameRef . Sugar.nrName & Just
                    Sugar.GetBinder x -> x ^. Sugar.bvNameRef . Sugar.nrName & Just
                    Sugar.GetParamsRecord _ -> Nothing
                _ <- internalNameMatch (arg ^. Sugar.aaTag . Sugar.tagName) name
                Right Sugar.RelayedArg
                    { Sugar._raValue = getVar
                    , Sugar._raId = arg ^. Sugar.aaExpr . Sugar.annotation . Sugar.plEntityId
                    , Sugar._raActions = arg ^. Sugar.aaExpr . Sugar.annotation . Sugar.plActions
                    } & Just
            & fromMaybe (Left arg)

addToBody ::
    Monad m =>
    Sugar.Body InternalName (T m) (T m) (Sugar.Expression InternalName (T m) (T m) a) ->
    T m (Sugar.Body InternalName (T m) (T m) (Sugar.Expression InternalName (T m) (T m) a))
addToBody (Sugar.BodyLabeledApply a) = addToLabeledApply a <&> Sugar.BodyLabeledApply
addToBody (Sugar.BodyHole h) = SugarLens.holeTransformExprs addToExpr h & Sugar.BodyHole & pure
addToBody (Sugar.BodyFragment w) =
    w
    & Sugar.fOptions . Lens.mapped . Lens.mapped %~ SugarLens.holeOptionTransformExprs addToExpr
    & Sugar.BodyFragment & pure
addToBody b = pure b

addToExpr ::
    Monad m => Sugar.Expression InternalName (T m) (T m) pl ->
    T m (Sugar.Expression InternalName (T m) (T m) pl)
addToExpr e =
    e
    & Sugar.body %%~ addToBody
    >>= Sugar.body . Lens.traversed %%~ addToExpr

addToBinder ::
    Monad m =>
    Sugar.Assignment InternalName (T m) (T m) (Sugar.Expression InternalName (T m) (T m) pl) ->
    T m (Sugar.Assignment InternalName (T m) (T m) (Sugar.Expression InternalName (T m) (T m) pl))
addToBinder = SugarLens.assignmentBody %%~ addToBinderBody

addToBinderBody ::
    Monad m =>
    Sugar.BinderBody InternalName (T m) (T m) (Sugar.Expression InternalName (T m) (T m) pl) ->
    T m (Sugar.BinderBody InternalName (T m) (T m) (Sugar.Expression InternalName (T m) (T m) pl))
addToBinderBody = Sugar.bbContent %%~ addToBinderContent

addToBinderContent ::
    Monad m =>
    Sugar.BinderContent InternalName (T m) (T m) (Sugar.Expression InternalName (T m) (T m) pl) ->
    T m (Sugar.BinderContent InternalName (T m) (T m) (Sugar.Expression InternalName (T m) (T m) pl))
addToBinderContent (Sugar.BinderExpr e) = addToExpr e <&> Sugar.BinderExpr
addToBinderContent (Sugar.BinderLet l) = addToLet l <&> Sugar.BinderLet

addToLet ::
    Monad m =>
    Sugar.Let InternalName (T m) (T m) (Sugar.Expression InternalName (T m) (T m) pl) ->
    T m (Sugar.Let InternalName (T m) (T m) (Sugar.Expression InternalName (T m) (T m) pl))
addToLet letItem =
    letItem
    & Sugar.lValue %%~ addToBinder
    >>= Sugar.lBody %%~ addToBinderBody

addToDef ::
    Monad m =>
    Sugar.Definition InternalName (T m) (T m) (Sugar.Expression InternalName (T m) (T m) a) ->
    T m (Sugar.Definition InternalName (T m) (T m) (Sugar.Expression InternalName (T m) (T m) a))
addToDef def =
    def
    & Sugar.drBody . Sugar._DefinitionBodyExpression .
      Sugar.deContent %%~ addToBinder
