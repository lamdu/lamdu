{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}
module Lamdu.GUI.RedundantAnnotations
    ( markAnnotationsToDisplay
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.GUI.ExpressionGui.Types as T
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

showAnn :: Lens' (Payload m0 T.Payload) T.ShowAnnotation
showAnn = plData . T.plShowAnnotation

dontShowEval :: Expression name m T.Payload -> Expression name m T.Payload
dontShowEval = rPayload . showAnn . T.showInEvalMode .~ T.EvalModeShowNothing

dontShowAnnotation :: Expression name m T.Payload -> Expression name m T.Payload
dontShowAnnotation expr =
    case expr ^. rBody of
    BodyHole{} -> expr
    _ -> expr & rPayload . showAnn .~ T.neverShowAnnotations

forceShowType :: Expression name m T.Payload -> Expression name m T.Payload
forceShowType =
    rPayload . showAnn %~
    (T.showExpanded .~ True) .
    (T.showInEvalMode .~ T.EvalModeShowType) .
    (T.showInTypeMode .~ True)

forceShowTypeOrEval :: Expression name m T.Payload -> Expression name m T.Payload
forceShowTypeOrEval =
    rPayload . showAnn %~
    (T.showExpanded .~ True) .
    (T.showInEvalMode .~ T.EvalModeShowEval) .
    (T.showInTypeMode .~ True)

markAnnotationsToDisplay ::
    Expression name m T.Payload ->
    Expression name m T.Payload
markAnnotationsToDisplay (Expression oldBody pl) =
    case newBody of
    BodyInjectedExpression ->
        Expression newBody pl & dontShowAnnotation
    BodyLiteral LiteralNum {} ->
        Expression newBody pl & dontShowAnnotation
    BodyLiteral LiteralText {}->
        Expression newBody pl & dontShowAnnotation
    BodyLiteral LiteralBytes {} ->
        Expression newBody pl & dontShowEval
    BodyRecord _ ->
        Expression newBody pl & dontShowAnnotation
    BodyLam _ ->
        Expression newBody pl & dontShowAnnotation
    BodyGetVar (GetParam Param { _pBinderMode = LightLambda }) ->
        Expression newBody pl
    BodyGetVar (GetParam Param { _pBinderMode = NormalBinder }) ->
        Expression newBody pl & dontShowAnnotation
    BodyGetVar (GetBinder BinderVar { _bvForm = GetDefinition _ }) ->
        Expression newBody pl
    BodyGetVar (GetBinder BinderVar { _bvForm = GetLet }) ->
        Expression newBody pl & dontShowAnnotation
    BodyFromNom _ ->
        Expression (newBody <&> dontShowEval) pl
    BodyToNom (Nominal _ binder) ->
        pl
        & showAnn . T.showInEvalMode .~
            binder ^. bbContent . SugarLens.binderContentExpr . rPayload . showAnn . T.showInEvalMode
        & Expression (newBody <&> dontShowEval)
    BodyInject _ ->
        Expression newBody pl & dontShowEval
    BodyGetVar (GetParamsRecord _) ->
        Expression newBody pl
    BodyGetField _ ->
        Expression newBody pl
    BodySimpleApply app ->
        Expression (BodySimpleApply (app & applyFunc %~ dontShowAnnotation)) pl
    BodyLabeledApply _ ->
        Expression newBody pl
    BodyGuard g ->
        Expression (BodyGuard g') pl
        where
            g' =
                g
                & gThen %~ onCaseAlt
                & gElseIfs . traverse . geThen %~ onCaseAlt
                & gElse %~ onCaseAlt
    BodyHole hole ->
        Expression (BodyHole hole') pl & forceShowType
        where
            hole' = hole & holeMArg . Lens._Just . haExpr %~ forceShowTypeOrEval
    BodyCase cas ->
        Expression (BodyCase cas') pl
        where
            cas' =
                cas
                -- cKind contains the scrutinee which is not always
                -- visible (for case alts that aren't lambdas), so
                -- maybe we do want to show the annotation
                & cKind . Lens.mapped %~ dontShowAnnotation
                & cAlts . Lens.mapped . Lens.mapped %~ onCaseAlt
    where
        newBody = oldBody <&> markAnnotationsToDisplay
        onCaseAlt a =
            a
            & rBody . _BodyLam . lamBinder . bBody . bbContent . SugarLens.binderContentExpr %~ dontShowAnnotation
            & rPayload . showAnn . T.funcApplyLimit .~ T.AtMostOneFuncApply
