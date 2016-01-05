{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}
module Lamdu.GUI.RedundantAnnotations
    ( markAnnotationsToDisplay
    ) where

import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import qualified Lamdu.GUI.ExpressionGui.Types as T
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types

import           Prelude.Compat

showAnn :: Lens' (Payload m0 T.Payload) T.ShowAnnotation
showAnn = plData . T.plShowAnnotation

dontShowEval :: Expression name m T.Payload -> Expression name m T.Payload
dontShowEval = rPayload . showAnn . T.showInEvalMode .~ T.EvalModeShowNothing

dontShowType :: Expression name m T.Payload -> Expression name m T.Payload
dontShowType = rPayload . showAnn . T.showInTypeMode .~ False

dontShowAnnotation :: Expression name m T.Payload -> Expression name m T.Payload
dontShowAnnotation = rPayload . showAnn .~ T.neverShowAnnotations

forceShowType :: Expression name m T.Payload -> Expression name m T.Payload
forceShowType =
    rPayload . showAnn %~
    (T.showExpanded .~ True) .
    (T.showInEvalMode .~ T.EvalModeShowType)

forceShowTypeOrEval :: Expression name m T.Payload -> Expression name m T.Payload
forceShowTypeOrEval =
    rPayload . showAnn %~
    (T.showExpanded .~ True) .
    (T.showInEvalMode .~ T.EvalModeShowEval)

markAnnotationsToDisplay ::
    Expression name m T.Payload ->
    Expression name m T.Payload
markAnnotationsToDisplay (Expression oldBody pl) =
    case newBody of
    BodyLiteralNum _ ->
        Expression newBody pl & dontShowAnnotation
    BodyLiteralText _ ->
        Expression newBody pl & dontShowAnnotation
    BodyLiteralBytes _ ->
        Expression newBody pl & dontShowEval
    BodyRecord _ ->
        Expression newBody pl & dontShowAnnotation
    BodyLam _ ->
        Expression newBody pl & dontShowAnnotation
    BodyGetVar (GetParam Param { _pBinderMode = LightLambda }) ->
        Expression newBody pl
    BodyGetVar (GetParam Param { _pBinderMode = NormalBinder }) ->
        Expression newBody pl & dontShowAnnotation
    BodyGetVar (GetBinder BinderVar { _bvForm = GetDefinition }) ->
        Expression newBody pl
    BodyGetVar (GetBinder BinderVar { _bvForm = GetLet }) ->
        Expression newBody pl & dontShowAnnotation
    BodyFromNom _ ->
        Expression (newBody <&> dontShowEval) pl
    BodyToNom _ ->
        Expression (newBody <&> dontShowEval) pl
    BodyInject _ ->
        Expression newBody pl & dontShowEval
    BodyGetVar (GetParamsRecord _) ->
        Expression newBody pl
    BodyGetField _ ->
        Expression newBody pl
    BodyApply app ->
        Expression (BodyApply (app & aFunc %~ dontShowAnnotation)) pl
    BodyList l ->
        Expression (BodyList l') pl & dontShowEval
        where
            l' = l & lValues . Lens.mapped . liExpr %~ dontShowType
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
                & cAlts . Lens.mapped . Lens.mapped %~
                  (rBody . _BodyLam . lamBinder . bBody . bbContent .
                   SugarLens.binderContentExpr %~ dontShowAnnotation) .
                  (rPayload . showAnn . T.funcApplyLimit .~ T.AtMostOneFuncApply)
    where
        newBody = oldBody <&> markAnnotationsToDisplay
