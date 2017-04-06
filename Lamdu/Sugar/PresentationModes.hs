{-# LANGUAGE NoImplicitPrelude #-}
module Lamdu.Sugar.PresentationModes
    ( addToDef, addToExpr
    ) where

import qualified Control.Lens as Lens
import           Data.UUID.Types (UUID)
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

addToLabeledApply ::
    Monad m =>
    Sugar.LabeledApply UUID (Sugar.BinderVar UUID f) expr ->
    T m (Sugar.LabeledApply UUID (Sugar.BinderVar UUID f) expr)
addToLabeledApply a =
    case a ^. Sugar.aSpecialArgs of
    Sugar.NoSpecialArgs ->
        do
            presentationMode <-
                a ^. Sugar.aFunc . Sugar.bvNameRef . Sugar.nrName
                & Anchors.assocPresentationMode & Transaction.getP
            let (specialArgs, annotatedArgs) =
                    case (presentationMode, a ^. Sugar.aAnnotatedArgs) of
                    (Sugar.Infix, a0:a1:as) ->
                        ( Sugar.InfixArgs
                          (a0 ^. Sugar.aaExpr) (a1 ^. Sugar.aaExpr)
                        , as
                        )
                    (Sugar.OO, a0:as) ->
                        (Sugar.ObjectArg (a0 ^. Sugar.aaExpr), as)
                    (_, args) -> (Sugar.NoSpecialArgs, args)
            a
                & Sugar.aAnnotatedArgs .~ annotatedArgs
                & Sugar.aSpecialArgs .~ specialArgs
                & return
    _ -> return a

addToHoleResult ::
    Monad m => Sugar.HoleResult UUID m -> T m (Sugar.HoleResult UUID m)
addToHoleResult = Sugar.holeResultConverted %%~ addToExpr

addToHole :: Monad m => Sugar.Hole UUID m a -> Sugar.Hole UUID m a
addToHole =
    Sugar.holeActions . Sugar.holeOptions .
    Lens.mapped . Lens.mapped . Sugar.hoResults . Lens.mapped .
    _2 %~ (>>= addToHoleResult)

addToBody :: Monad m => BodyU m pl -> T m (BodyU m pl)
addToBody (Sugar.BodyLabeledApply a) = addToLabeledApply a <&> Sugar.BodyLabeledApply
addToBody (Sugar.BodyHole a) = addToHole a & Sugar.BodyHole & return
addToBody b = return b

addToExpr :: Monad m => ExpressionU m pl -> T m (ExpressionU m pl)
addToExpr e =
    e
    & Sugar.rBody %%~ addToBody
    >>= Sugar.rBody . Lens.traversed %%~ addToExpr

addToBinder ::
    Monad m =>
    Sugar.Binder UUID m (ExpressionU m pl) ->
    T m (Sugar.Binder UUID m (ExpressionU m pl))
addToBinder = Sugar.bBody %%~ addToBinderBody

addToBinderBody ::
    Monad m =>
    Sugar.BinderBody UUID m (ExpressionU m pl) ->
    T m (Sugar.BinderBody UUID m (ExpressionU m pl))
addToBinderBody = Sugar.bbContent %%~ addToBinderContent

addToBinderContent ::
    Monad m =>
    Sugar.BinderContent UUID m (ExpressionU m pl) ->
    T m (Sugar.BinderContent UUID m (ExpressionU m pl))
addToBinderContent (Sugar.BinderExpr e) = addToExpr e <&> Sugar.BinderExpr
addToBinderContent (Sugar.BinderLet l) = addToLet l <&> Sugar.BinderLet

addToLet ::
    Monad m =>
    Sugar.Let UUID m (ExpressionU m pl) ->
    T m (Sugar.Let UUID m (ExpressionU m pl))
addToLet letItem =
    letItem
    & Sugar.lValue %%~ addToBinder
    >>= Sugar.lBody %%~ addToBinderBody

addToDef ::
    Monad m =>
    Sugar.Definition UUID m (ExpressionU m a) ->
    T m (Sugar.Definition UUID m (ExpressionU m a))
addToDef def =
    def
    & Sugar.drBody . Sugar._DefinitionBodyExpression .
      Sugar.deContent %%~ addToBinder
