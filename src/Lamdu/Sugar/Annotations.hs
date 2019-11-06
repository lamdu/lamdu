{-# LANGUAGE TemplateHaskell, TypeApplications, DataKinds, TypeFamilies #-}

module Lamdu.Sugar.Annotations
    ( ShowAnnotation(..), showExpanded, showInTypeMode, showInEvalMode
    , MarkAnnotations
    , markNodeAnnotations
    , neverShowAnnotations, alwaysShowAnnotations
    ) where

import qualified Control.Lens as Lens
import           Hyper
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

alwaysShowAnnotations :: ShowAnnotation
alwaysShowAnnotations = ShowAnnotation True True True

neverShowAnnotations :: ShowAnnotation
neverShowAnnotations = ShowAnnotation False False False

dontShowEval :: ShowAnnotation
dontShowEval = showAnnotationWhenVerbose & showInEvalMode .~ False

forceShowTypeOrEval :: ShowAnnotation
forceShowTypeOrEval = showAnnotationWhenVerbose & showExpanded .~ True

topLevelAnn ::
    Lens' (Expression name i o (ShowAnnotation, a))
    ShowAnnotation
topLevelAnn = annotation . _1

markNodeAnnotations ::
    MarkAnnotations t =>
    Annotated a t ->
    Annotated (ShowAnnotation, a) t
markNodeAnnotations (Ann (Const pl) x) =
    Ann (Const (showAnn, pl)) newBody
    where
        (showAnn, newBody) = markAnnotations x

class MarkAnnotations (t :: AHyperType -> *) where
    markAnnotations :: Tree t (Ann (Const a)) -> (ShowAnnotation, Tree t (Ann (Const (ShowAnnotation, a))))

instance MarkAnnotations (Binder name i o) where
    markAnnotations (BinderExpr body) =
        markBodyAnnotations body & _2 %~ BinderExpr
    markAnnotations (BinderLet let_) =
        ( neverShowAnnotations
        , hmap (Proxy @MarkAnnotations #> markNodeAnnotations) let_
            & BinderLet
        )

instance MarkAnnotations (Assignment name i o) where
    markAnnotations (BodyPlain (AssignPlain a b)) =
        markAnnotations b
        & _2 %~ BodyPlain . AssignPlain a
    markAnnotations (BodyFunction function) =
        ( neverShowAnnotations
        , function & fBody %~ markNodeAnnotations & BodyFunction
        )

instance MarkAnnotations (Else name i o) where
    markAnnotations (SimpleElse body) =
        markBodyAnnotations body & _2 %~ SimpleElse
    markAnnotations (ElseIf elseIf) =
        ( neverShowAnnotations
        , elseIf
            & eiContent %~
                hmap (Proxy @MarkAnnotations #> markNodeAnnotations)
            & ElseIf
        )

instance MarkAnnotations (Const a) where
    markAnnotations (Const x) = (neverShowAnnotations, Const x)

instance MarkAnnotations (Body name i o) where
    markAnnotations = markBodyAnnotations

markBodyAnnotations ::
    Tree (Body name i o) (Ann (Const a)) ->
    (ShowAnnotation, Tree (Body name i o) (Ann (Const (ShowAnnotation, a))))
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
            & appFunc . nonHoleAnn .~ neverShowAnnotations
            & BodySimpleApply
        )
    BodyLabeledApply _ -> set showAnnotationWhenVerbose
    BodyIfElse i ->
       ( showAnnotationWhenVerbose
       , onIfElse i & BodyIfElse
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
            & cBody . cItems . Lens.mapped . Lens.mapped . hVal %~ onHandler
            & BodyCase
        )
    where
        newBodyWith f =
            hmap
            ( Proxy @SugarLens.SugarExpr #>
                Lens.filtered (not . SugarLens.isUnfinished . (^. hVal)) . annotation . _1 .~ f
            ) newBody
        nonHoleIndex = Lens.ifiltered (const . Lens.nullOf SugarLens.bodyUnfinished)
        set x = (x, newBody)
        newBody =
            hmap (Proxy @MarkAnnotations #> markNodeAnnotations) oldBody
        nonHoleAnn =
            Lens.filtered (Lens.nullOf (hVal . SugarLens.bodyUnfinished)) .
            topLevelAnn
        onHandler a =
            a
            & _BodyLam . lamFunc . fBody .
              SugarLens.binderResultExpr . nonHoleIndex . _1 .~ neverShowAnnotations
        onElse (SimpleElse x) = onHandler x & SimpleElse
        onElse (ElseIf elseIf) = elseIf & eiContent %~ onIfElse & ElseIf
        onIfElse x =
            x
            & iThen . hVal %~ onHandler
            & iElse . hVal %~ onElse
