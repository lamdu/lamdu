{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Lamdu.Sugar.Annotations
    ( ShowAnnotation(..), showExpanded, showInTypeMode, showInEvalMode
    , markAnnotationsToDisplay
    , neverShowAnnotations
    ) where

import qualified Control.Lens as Lens
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

data ShowAnnotation = ShowAnnotation
    { -- showExpanded means we:
      -- A) Show type even in concise-mode
      -- B) Do not shrink the annotation to fit
      _showExpanded :: Bool
    , _showInTypeMode :: Bool
    , _showInEvalMode :: Bool
    } deriving (Eq, Ord, Show, Generic)
Lens.makeLenses ''ShowAnnotation

showAnnotationWhenVerbose :: ShowAnnotation
showAnnotationWhenVerbose =
    ShowAnnotation
    { _showExpanded = False
    , _showInTypeMode = True
    , _showInEvalMode = True
    }

neverShowAnnotations :: ShowAnnotation
neverShowAnnotations = ShowAnnotation False False False

dontShowEval :: ShowAnnotation
dontShowEval = showAnnotationWhenVerbose & showInEvalMode .~ False

forceShowTypeOrEval :: ShowAnnotation
forceShowTypeOrEval = showAnnotationWhenVerbose & showExpanded .~ True

topLevelAnn ::
    Lens' (Expression name i o (ShowAnnotation, a))
    ShowAnnotation
topLevelAnn = _PNode . ann . _1

markAnnotationsToDisplay ::
    Expression name i o a ->
    Expression name i o (ShowAnnotation, a)
markAnnotationsToDisplay (PNode (Node pl oldBody)) =
    Node (showAnn, pl) newBody & PNode
    where
        (showAnn, newBody) = markBodyAnnotations oldBody

markBodyAnnotations ::
    Body name i o a ->
    (ShowAnnotation, Body name i o (ShowAnnotation, a))
markBodyAnnotations oldBody =
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
        ( showAnnotationWhenVerbose
            & showInEvalMode .~
                ( tid ^. tidTId == Builtins.textTid
                    || binder ^. SugarLens.binderResultExpr . _1 . showInEvalMode
                )
        , newBodyWith dontShowEval
        )
    BodyInject _ -> set dontShowEval
    BodyGetVar (GetParamsRecord _) -> set showAnnotationWhenVerbose
    BodyGetField _ -> set showAnnotationWhenVerbose
    BodySimpleApply app ->
        ( showAnnotationWhenVerbose
        , app
            & applyFunc . nonHoleAnn .~ neverShowAnnotations
            & BodySimpleApply
        )
    BodyLabeledApply _ -> set showAnnotationWhenVerbose
    BodyIfElse i ->
        ( showAnnotationWhenVerbose
        , i
            & iIfThen . itThen %~ onCaseAlt
            & iElse %~ onElse
            & BodyIfElse
        )
    BodyHole hole -> (forceShowTypeOrEval, BodyHole hole)
    BodyFragment fragment ->
        ( forceShowTypeOrEval
        , fragment
            & fExpr . nonHoleAnn .~ forceShowTypeOrEval
            & BodyFragment
        )
    BodyCase cas ->
        ( showAnnotationWhenVerbose
        , cas
            -- cKind contains the scrutinee which is not always
            -- visible (for case alts that aren't lambdas), so
            -- maybe we do want to show the annotation
            & cKind . Lens.mapped . nonHoleAnn .~ neverShowAnnotations
            & cBody . cItems . Lens.mapped . Lens.mapped %~ onCaseAlt
            & BodyCase
        )
    where
        newBodyWith f = newBody & SugarLens.bodyChildPayloads . nonHoleIndex . _1 .~ f
        nonHoleIndex = Lens.ifiltered (const . Lens.nullOf (SugarLens._OfExpr . SugarLens.bodyUnfinished))
        set x = (x, newBody)
        newBody =
            SugarLens.overBodyChildren
            (ann %~ (,) neverShowAnnotations)
            (ann %~ (,) neverShowAnnotations)
            (ann %~ (,) neverShowAnnotations)
            markAnnotationsToDisplay
            oldBody
        nonHoleAnn = Lens.filtered (Lens.nullOf (_PNode . val . SugarLens.bodyUnfinished)) . topLevelAnn
        onCaseAlt a =
            a
            & _PNode . val . _BodyLam . lamFunc . fBody .
              SugarLens.binderResultExpr . nonHoleIndex . _1 .~ neverShowAnnotations
        onElse (SimpleElse x) = onCaseAlt x & SimpleElse
        onElse (ElseIf elseIf) =
            elseIf
            & eiContent . iIfThen . itThen %~ onCaseAlt
            & eiContent . iElse %~ onElse
            & ElseIf
