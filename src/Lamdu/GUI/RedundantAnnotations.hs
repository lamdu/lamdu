{-# LANGUAGE NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}
module Lamdu.GUI.RedundantAnnotations
    ( markAnnotationsToDisplay
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.GUI.ExpressionGui.Types as T
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

dontShowEval :: T.ShowAnnotation
dontShowEval =
    T.showAnnotationWhenVerbose & T.showInEvalMode .~ T.EvalModeShowNothing

forceShowType :: T.ShowAnnotation
forceShowType =
    T.showAnnotationWhenVerbose
    & T.showExpanded .~ True
    & T.showInEvalMode .~ T.EvalModeShowType
    & T.showInTypeMode .~ True

forceShowTypeOrEval :: T.ShowAnnotation
forceShowTypeOrEval =
    T.showAnnotationWhenVerbose
    & T.showExpanded .~ True
    & T.showInEvalMode .~ T.EvalModeShowEval
    & T.showInTypeMode .~ True

topLevelAnn :: Lens' (Expression name m (T.ShowAnnotation, a)) T.ShowAnnotation
topLevelAnn = rPayload . plData . _1

markAnnotationsToDisplay ::
    forall name m a.
    Expression name m a ->
    Expression name m (T.ShowAnnotation, a)
markAnnotationsToDisplay (Expression oldBody pl) =
    case newBody of
    BodyInjectedExpression -> set T.neverShowAnnotations
    BodyLiteral LiteralNum {} -> set T.neverShowAnnotations
    BodyLiteral LiteralText {}-> set T.neverShowAnnotations
    BodyLiteral LiteralBytes {} -> set dontShowEval
    BodyRecord _ -> set T.neverShowAnnotations
    BodyLam _ -> set T.neverShowAnnotations
    BodyGetVar (GetParam Param { _pBinderMode = LightLambda }) ->
        set T.showAnnotationWhenVerbose
    BodyGetVar (GetParam Param { _pBinderMode = NormalBinder }) ->
        set T.neverShowAnnotations
    BodyGetVar (GetBinder BinderVar { _bvForm = GetDefinition _ }) ->
        set T.showAnnotationWhenVerbose
    BodyGetVar (GetBinder BinderVar { _bvForm = GetLet }) ->
        set T.neverShowAnnotations
    BodyFromNom _ -> Expression (newBodyWith dontShowEval) defPl
    BodyToNom (Nominal _ binder) ->
        defPl
        & plData . _1 . T.showInEvalMode .~
        binder ^. bbContent . SugarLens.binderContentExpr .
        topLevelAnn . T.showInEvalMode
        & Expression (newBodyWith dontShowEval)
    BodyInject _ -> set dontShowEval
    BodyGetVar (GetParamsRecord _) -> set T.showAnnotationWhenVerbose
    BodyGetField _ -> set T.showAnnotationWhenVerbose
    BodySimpleApply app ->
        app
        & applyFunc . nonHoleAnn .~ T.neverShowAnnotations
        & BodySimpleApply
        & (`Expression` defPl)
    BodyLabeledApply _ -> set T.showAnnotationWhenVerbose
    BodyGuard g ->
        g
        & gThen %~ onCaseAlt
        & gElseIfs . Lens.mapped . geThen %~ onCaseAlt
        & gElse %~ onCaseAlt
        & BodyGuard
        & (`Expression` defPl)
    BodyHole hole ->
        hole
        & holeKind . _WrapperHole . haExpr . topLevelAnn .~ forceShowTypeOrEval
        & BodyHole
        & (`Expression` plWith forceShowType)
    BodyCase cas ->
        cas
        -- cKind contains the scrutinee which is not always
        -- visible (for case alts that aren't lambdas), so
        -- maybe we do want to show the annotation
        & cKind . Lens.mapped . topLevelAnn .~ T.neverShowAnnotations
        & cBody . cItems . Lens.mapped . Lens.mapped %~ onCaseAlt
        & BodyCase
        & (`Expression` defPl)
    where
        newBodyWith f = newBody <&> topLevelAnn .~ f
        plWith ann = pl & plData %~ (,) ann
        defPl = plWith T.showAnnotationWhenVerbose
        set ann = Expression newBody (plWith ann)
        newBody = oldBody <&> markAnnotationsToDisplay
        nonHoleAnn =
            Lens.filtered (Lens.nullOf (rBody . _BodyHole)) . topLevelAnn
        onCaseAlt a =
            a
            & rBody . _BodyLam . lamBinder . bBody . bbContent .
              SugarLens.binderContentExpr . nonHoleAnn .~ T.neverShowAnnotations
            & topLevelAnn . T.funcApplyLimit .~ T.AtMostOneFuncApply
