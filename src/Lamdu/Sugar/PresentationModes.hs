{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.PresentationModes
    ( addToDef, addToExpr
    ) where

import qualified Control.Lens as Lens
import           Data.Either (partitionEithers)
import qualified Data.Map as Map
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.UUID.Types (UUID)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

addToLabeledApply ::
    Monad m =>
    Sugar.LabeledApply UUID f (Sugar.Expression UUID f a) ->
    T m (Sugar.LabeledApply UUID f (Sugar.Expression UUID f a))
addToLabeledApply a =
    case a ^. Sugar.aSpecialArgs of
    Sugar.Verbose ->
        do
            presentationMode <-
                a ^. Sugar.aFunc . Sugar.bvNameRef . Sugar.nrName
                & Anchors.assocPresentationMode & Transaction.getP
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
            let (annotatedArgs, relayedArgs) = otherArgs <&> processArg & partitionEithers
            a
                & Sugar.aSpecialArgs .~ specialArgs
                & Sugar.aAnnotatedArgs .~ annotatedArgs
                & Sugar.aRelayedArgs .~ relayedArgs
                & return
    _ -> return a
    where
        argsMap =
            a ^. Sugar.aAnnotatedArgs
            <&> (\x -> (x ^. Sugar.aaTag . Sugar.tagVal, x))
            & Map.fromList
        argExpr t = Map.lookup t argsMap <&> (^. Sugar.aaExpr) <&> (,) t
        mkInfixArg arg other =
            arg
            & Sugar.rPayload . Sugar.plActions . Sugar.delete %~ addDel
            where
                addDel Sugar.CannotDelete =
                    other ^. Sugar.rPayload . Sugar.plActions . Sugar.mReplaceParent &
                    maybe Sugar.CannotDelete Sugar.Delete
                addDel x = x
        processArg arg =
            do
                param <- arg ^? Sugar.aaExpr . Sugar.rBody . Sugar._BodyGetVar . Sugar._GetParam
                param ^. Sugar.pNameRef . Sugar.nrName == arg ^. Sugar.aaName & guard
                Right Sugar.RelayedArg
                    { Sugar._raValue = param
                    , Sugar._raId = arg ^. Sugar.aaExpr . Sugar.rPayload . Sugar.plEntityId
                    , Sugar._raActions = arg ^. Sugar.aaExpr . Sugar.rPayload . Sugar.plActions
                    } & return
            & fromMaybe (Left arg)

addToHoleResult ::
    Monad m =>
    Sugar.HoleResult (T m) (Sugar.Expression UUID (T m) ()) ->
    T m (Sugar.HoleResult (T m) (Sugar.Expression UUID (T m) ()))
addToHoleResult = Sugar.holeResultConverted %%~ addToExpr

addToOptions ::
    Monad m =>
    T m [Sugar.HoleOption (T m) (Sugar.Expression UUID (T m) ())] ->
    T m [Sugar.HoleOption (T m) (Sugar.Expression UUID (T m) ())]
addToOptions = Lens.mapped . Lens.mapped . Sugar.hoResults . Lens.mapped . _2 %~ (>>= addToHoleResult)

addToBody ::
    Monad m =>
    Sugar.Body UUID (T m) (Sugar.Expression UUID (T m) a) ->
    T m (Sugar.Body UUID (T m) (Sugar.Expression UUID (T m) a))
addToBody (Sugar.BodyLabeledApply a) = addToLabeledApply a <&> Sugar.BodyLabeledApply
addToBody (Sugar.BodyHole h) = h & Sugar.holeOptions %~ addToOptions & Sugar.BodyHole & pure
addToBody (Sugar.BodyFragment w) = w & Sugar.fOptions %~ addToOptions & Sugar.BodyFragment & pure
addToBody b = pure b

addToExpr ::
    Monad m => Sugar.Expression UUID (T m) pl ->
    T m (Sugar.Expression UUID (T m) pl)
addToExpr e =
    e
    & Sugar.rBody %%~ addToBody
    >>= Sugar.rBody . Lens.traversed %%~ addToExpr

addToBinder ::
    Monad m =>
    Sugar.Binder UUID (T m) (Sugar.Expression UUID (T m) pl) ->
    T m (Sugar.Binder UUID (T m) (Sugar.Expression UUID (T m) pl))
addToBinder = Sugar.bBody %%~ addToBinderBody

addToBinderBody ::
    Monad m =>
    Sugar.BinderBody UUID (T m) (Sugar.Expression UUID (T m) pl) ->
    T m (Sugar.BinderBody UUID (T m) (Sugar.Expression UUID (T m) pl))
addToBinderBody = Sugar.bbContent %%~ addToBinderContent

addToBinderContent ::
    Monad m =>
    Sugar.BinderContent UUID (T m) (Sugar.Expression UUID (T m) pl) ->
    T m (Sugar.BinderContent UUID (T m) (Sugar.Expression UUID (T m) pl))
addToBinderContent (Sugar.BinderExpr e) = addToExpr e <&> Sugar.BinderExpr
addToBinderContent (Sugar.BinderLet l) = addToLet l <&> Sugar.BinderLet

addToLet ::
    Monad m =>
    Sugar.Let UUID (T m) (Sugar.Expression UUID (T m) pl) ->
    T m (Sugar.Let UUID (T m) (Sugar.Expression UUID (T m) pl))
addToLet letItem =
    letItem
    & Sugar.lValue %%~ addToBinder
    >>= Sugar.lBody %%~ addToBinderBody

addToDef ::
    Monad m =>
    Sugar.Definition UUID (T m) (Sugar.Expression UUID (T m) a) ->
    T m (Sugar.Definition UUID (T m) (Sugar.Expression UUID (T m) a))
addToDef def =
    def
    & Sugar.drBody . Sugar._DefinitionBodyExpression .
      Sugar.deContent %%~ addToBinder
