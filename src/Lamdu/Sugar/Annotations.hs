{-# LANGUAGE TemplateHaskell #-}

module Lamdu.Sugar.Annotations
    ( ShowAnnotation(..), showExpanded, showInTypeMode, showInEvalMode , funcApplyLimit
    , markAnnotationsToDisplay
    , neverShowAnnotations, alwaysShowAnnotations, showAnnotationWhenVerbose
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

data ShowAnnotation = ShowAnnotation
    { -- showExpanded means we:
      -- A) Show even in concise-mode & eval-mode without val
      -- B) Do not shrink the annotation to fit
      _showExpanded :: Bool
    , _showInTypeMode :: Bool
    , _showInEvalMode :: Bool
    , _funcApplyLimit :: FuncApplyLimit
    } deriving (Eq, Ord, Show, Generic)
Lens.makeLenses ''ShowAnnotation

showAnnotationWhenVerbose :: ShowAnnotation
showAnnotationWhenVerbose =
    ShowAnnotation
    { _showExpanded = False
    , _showInTypeMode = True
    , _showInEvalMode = True
    , _funcApplyLimit = UnlimitedFuncApply
    }

neverShowAnnotations :: ShowAnnotation
neverShowAnnotations = ShowAnnotation False False False UnlimitedFuncApply

alwaysShowAnnotations :: ShowAnnotation
alwaysShowAnnotations = ShowAnnotation True True True UnlimitedFuncApply

dontShowEval :: ShowAnnotation
dontShowEval = showAnnotationWhenVerbose & showInEvalMode .~ False

forceShowTypeOrEval :: ShowAnnotation
forceShowTypeOrEval = showAnnotationWhenVerbose & showExpanded .~ True

topLevelAnn ::
    Lens' (Expression name i o (ShowAnnotation, a))
    ShowAnnotation
topLevelAnn = annotation . _1

markAnnotationsToDisplay ::
    Expression name i o a ->
    Expression name i o (ShowAnnotation, a)
markAnnotationsToDisplay (Expression pl oldBody) =
    case newBody of
    BodyPlaceHolder -> set neverShowAnnotations
    BodyLiteral LiteralNum {} -> set neverShowAnnotations
    BodyLiteral LiteralText {} -> set neverShowAnnotations
    BodyLiteral LiteralBytes {} -> set dontShowEval
    BodyRecord _ -> set neverShowAnnotations
    BodyLam _ -> set neverShowAnnotations
    BodyGetVar (GetParam ParamRef { _pBinderMode = LightLambda }) ->
        set showAnnotationWhenVerbose
    BodyGetVar (GetParam ParamRef { _pBinderMode = NormalBinder }) ->
        set neverShowAnnotations
    BodyGetVar (GetBinder BinderVarRef { _bvForm = GetDefinition _ }) ->
        set showAnnotationWhenVerbose
    BodyGetVar (GetBinder BinderVarRef { _bvForm = GetLet }) ->
        set neverShowAnnotations
    BodyFromNom _ -> set dontShowEval
    BodyToNom (Nominal tid binder) ->
        defPl
        & _1 . showInEvalMode .~
            ( tid ^. tidTId == Builtins.textTid
                || binder ^. bbContent . SugarLens.binderContentResultExpr .
                    topLevelAnn . showInEvalMode
            )
        & (`Expression` newBodyWith dontShowEval)
    BodyInject _ -> set dontShowEval
    BodyGetVar (GetParamsRecord _) -> set showAnnotationWhenVerbose
    BodyGetField _ -> set showAnnotationWhenVerbose
    BodySimpleApply app ->
        app
        & applyFunc . nonHoleAnn .~ neverShowAnnotations
        & BodySimpleApply
        & Expression defPl
    BodyLabeledApply _ -> set showAnnotationWhenVerbose
    BodyIfElse i ->
        i
        & iIfThen . itThen %~ onCaseAlt
        & iElse %~ onElse
        & BodyIfElse
        & Expression defPl
    BodyHole hole ->
        hole
        & BodyHole
        & Expression (plWith forceShowTypeOrEval)
    BodyFragment fragment ->
        fragment
        & fExpr . nonHoleAnn .~ forceShowTypeOrEval
        & BodyFragment
        & Expression (plWith forceShowTypeOrEval)
    BodyCase cas ->
        cas
        -- cKind contains the scrutinee which is not always
        -- visible (for case alts that aren't lambdas), so
        -- maybe we do want to show the annotation
        & cKind . Lens.mapped . nonHoleAnn .~ neverShowAnnotations
        & cBody . cItems . Lens.mapped . Lens.mapped %~ onCaseAlt
        & BodyCase
        & Expression defPl
    where
        newBodyWith f =
            SugarLens.overBodyChildren
            (nullaryPayload . _1 .~ f)
            (afPayload . _1 .~ f)
            (nonHoleAnn .~ f)
            newBody
        plWith ann = (ann, pl)
        defPl = plWith showAnnotationWhenVerbose
        set ann = Expression (plWith ann) newBody
        newBody =
            SugarLens.overBodyChildren
            (nullaryPayload %~ (,) neverShowAnnotations)
            (afPayload %~ (,) neverShowAnnotations)
            markAnnotationsToDisplay
            oldBody
        nonHoleAnn = Lens.filtered (Lens.nullOf (body . SugarLens.bodyUnfinished)) . topLevelAnn
        onCaseAlt a =
            a
            & body . _BodyLam . lamFunc . fBody . bbContent .
              SugarLens.binderContentResultExpr . nonHoleAnn .~ neverShowAnnotations
            & topLevelAnn . funcApplyLimit .~ AtMostOneFuncApply
        onElse (SimpleElse x) = onCaseAlt x & SimpleElse
        onElse (ElseIf elseIf) =
            elseIf
            & eiContent . iIfThen . itThen %~ onCaseAlt
            & eiContent . iElse %~ onElse
            & ElseIf
