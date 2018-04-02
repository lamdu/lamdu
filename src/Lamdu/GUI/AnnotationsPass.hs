module Lamdu.GUI.AnnotationsPass
    ( markAnnotationsToDisplay
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.GUI.ExpressionGui as T
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

topLevelAnn :: Lens' (Expression name im am (T.ShowAnnotation, a)) T.ShowAnnotation
topLevelAnn = rPayload . plData . _1

markAnnotationsToDisplay ::
    Expression name im am a ->
    Expression name im am (T.ShowAnnotation, a)
markAnnotationsToDisplay (Expression oldBody pl) =
    case newBody of
    BodyPlaceHolder -> set T.neverShowAnnotations
    BodyLiteral LiteralNum {} -> set T.neverShowAnnotations
    BodyLiteral LiteralText {} -> set T.neverShowAnnotations
    BodyLiteral LiteralBytes {} -> set dontShowEval
    BodyRecord _ -> set T.neverShowAnnotations
    BodyLam _ -> set T.neverShowAnnotations
    BodyGetVar (GetParam ParamRef { _pBinderMode = LightLambda }) ->
        set T.showAnnotationWhenVerbose
    BodyGetVar (GetParam ParamRef { _pBinderMode = NormalBinder }) ->
        set T.neverShowAnnotations
    BodyGetVar (GetBinder BinderVarRef { _bvForm = GetDefinition _ }) ->
        set T.showAnnotationWhenVerbose
    BodyGetVar (GetBinder BinderVarRef { _bvForm = GetLet }) ->
        set T.neverShowAnnotations
    BodyFromNom _ -> set dontShowEval
    BodyToNom (Nominal tid binder) ->
        defPl
        & plData . _1 . T.showInEvalMode .~
            ( if tid ^. tidTId == Builtins.textTid
                then T.EvalModeShowEval
                else binder ^. bbContent . SugarLens.binderContentExpr .
                        topLevelAnn . T.showInEvalMode
            )
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
    BodyIfElse i ->
        i
        & iIfThen . itThen %~ onCaseAlt
        & iElse %~ onElse
        & BodyIfElse
        & (`Expression` defPl)
    BodyHole hole ->
        hole
        & BodyHole
        & (`Expression` plWith forceShowType)
    BodyFragment fragment ->
        fragment
        & fExpr . nonHoleAnn .~ forceShowTypeOrEval
        & BodyFragment
        & (`Expression` plWith forceShowType)
    BodyCase cas ->
        cas
        -- cKind contains the scrutinee which is not always
        -- visible (for case alts that aren't lambdas), so
        -- maybe we do want to show the annotation
        & cKind . Lens.mapped . nonHoleAnn .~ T.neverShowAnnotations
        & cBody . cItems . Lens.mapped . Lens.mapped %~ onCaseAlt
        & BodyCase
        & (`Expression` defPl)
    where
        newBodyWith f = newBody <&> nonHoleAnn .~ f
        plWith ann = pl & plData %~ (,) ann
        defPl = plWith T.showAnnotationWhenVerbose
        set ann = Expression newBody (plWith ann)
        newBody = oldBody <&> markAnnotationsToDisplay
        nonHoleAnn = Lens.filtered (Lens.nullOf (rBody . SugarLens.bodyUnfinished)) . topLevelAnn
        onCaseAlt a =
            a
            & rBody . _BodyLam . lamBinder . bBody . bbContent .
              SugarLens.binderContentExpr . nonHoleAnn .~ T.neverShowAnnotations
            & topLevelAnn . T.funcApplyLimit .~ T.AtMostOneFuncApply
        onElse (SimpleElse x) = onCaseAlt x & SimpleElse
        onElse (ElseIf elseIf) =
            elseIf
            & eiContent . iIfThen . itThen %~ onCaseAlt
            & eiContent . iElse %~ onElse
            & ElseIf
