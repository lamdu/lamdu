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
showAnn x = (plData . T.plShowAnnotation) x

don'tShowEval :: Expression name m T.Payload -> Expression name m T.Payload
don'tShowEval = rPayload . showAnn . T.showInEvalMode .~ T.EvalModeShowNothing

don'tShowType :: Expression name m T.Payload -> Expression name m T.Payload
don'tShowType = rPayload . showAnn . T.showInTypeMode .~ False

don'tShowAnnotation :: Expression name m T.Payload -> Expression name m T.Payload
don'tShowAnnotation = rPayload . showAnn .~ T.neverShowAnnotations

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
        Expression newBody pl & don'tShowAnnotation
    BodyRecord _ ->
        Expression newBody pl & don'tShowAnnotation
    BodyLam _ ->
        Expression newBody pl & don'tShowAnnotation
    BodyGetVar (GetVarNamed NamedVar { _nvMode = LightLambda }) ->
        Expression newBody pl
    BodyGetVar (GetVarNamed NamedVar { _nvVarType = GetFieldParameter }) ->
        Expression newBody pl & don'tShowAnnotation
    BodyGetVar (GetVarNamed NamedVar { _nvVarType = GetParameter }) ->
        Expression newBody pl & don'tShowAnnotation
    BodyFromNom _ ->
        Expression (newBody <&> don'tShowEval) pl
    BodyToNom _ ->
        Expression (newBody <&> don'tShowEval) pl
    BodyInject _ ->
        Expression newBody pl & don'tShowEval
    BodyGetVar (GetVarParamsRecord _) ->
        Expression newBody pl
    BodyGetField _ ->
        Expression newBody pl
    BodyGetVar (GetVarNamed NamedVar { _nvVarType = GetDefinition }) ->
        Expression newBody pl
    BodyApply app ->
        Expression (BodyApply (app & aFunc %~ don'tShowAnnotation)) pl
    BodyList l ->
        Expression (BodyList l') pl & don'tShowEval
        where
            l' = l & lValues . Lens.mapped . liExpr %~ don'tShowType
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
                & cKind . Lens.mapped %~ don'tShowAnnotation
                & cAlts . Lens.mapped . Lens.mapped %~
                  (rBody . _BodyLam . lamBinder . bBody .
                   SugarLens.binderBodyExpr %~ don'tShowAnnotation) .
                  (rPayload . showAnn . T.funcApplyLimit .~ T.AtMostOneFuncApply)
    where
        newBody = oldBody <&> markAnnotationsToDisplay
