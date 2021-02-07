{-# LANGUAGE TemplateHaskell, TypeApplications, DataKinds, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}

module Lamdu.Sugar.Annotations
    ( ShowAnnotation(..), showTypeAlways, showInTypeMode, showInEvalMode
    , MarkAnnotations(..)
    , neverShowAnnotations, alwaysShowAnnotations
    ) where

import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Class.Morph
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

data ShowAnnotation = ShowAnnotation
    { -- For holes and fragments we always show types
      _showTypeAlways :: Bool
    , _showInTypeMode :: Bool
    , _showInEvalMode :: Bool
    } deriving (Eq, Ord, Show, Generic)
Lens.makeLenses ''ShowAnnotation

showAnnotationWhenVerbose :: ShowAnnotation
showAnnotationWhenVerbose =
    ShowAnnotation
    { _showTypeAlways = False
    , _showInTypeMode = True
    , _showInEvalMode = True
    }

alwaysShowAnnotations :: ShowAnnotation
alwaysShowAnnotations = ShowAnnotation True True True

neverShowAnnotations :: ShowAnnotation
neverShowAnnotations = ShowAnnotation False False False

dontShowEval :: ShowAnnotation
dontShowEval =
    ShowAnnotation
    { _showTypeAlways = False
    , _showInTypeMode = True
    , _showInEvalMode = False
    }

dontShowType :: ShowAnnotation
dontShowType =
    ShowAnnotation
    { _showTypeAlways = False
    , _showInTypeMode = False
    , _showInEvalMode = True
    }

class MarkBodyAnnotations v n i o e where
    markBodyAnnotations ::
        Body e v n i o a -> (ShowAnnotation, Body e (ShowAnnotation, v) n i o a)

class MarkAnnotations v n i o t0 t1 where
    markNodeAnnotations ::
        Annotated (Payload v n i o, a) # t0 ->
        Annotated (Payload (ShowAnnotation, v) n i o, a) # t1

instance MarkAnnotations v n i o (Const a) (Const a) where
    markNodeAnnotations (Ann a (Const b)) = Ann (a & Lens._Wrapped . _1 . plAnnotation %~ (,) neverShowAnnotations) (Const b)

instance MarkBodyAnnotations v n i o e => MarkAnnotations v n i o (e v n i o) (e (ShowAnnotation, v) n i o) where
    markNodeAnnotations (Ann (Const pl) x) =
        Ann (Const (pl & _1 . plAnnotation %~ (,) showAnn)) newBody
        where
            (showAnn, newBody) = markBodyAnnotations x

instance Functor i => MarkBodyAnnotations v n i o Binder where
    markBodyAnnotations (BinderTerm body) =
        markBodyAnnotations body & _2 %~ BinderTerm
    markBodyAnnotations (BinderLet let_) =
        ( neverShowAnnotations
        , morphMap (Proxy @(MarkAnnotations v n i o) #?> markNodeAnnotations) let_
            & BinderLet
        )

instance Functor i => MarkBodyAnnotations v n i o Assignment where
    markBodyAnnotations (BodyPlain (AssignPlain a b)) =
        markBodyAnnotations b
        & _2 %~ BodyPlain . AssignPlain a
    markBodyAnnotations (BodyFunction f) = markBodyAnnotations f & _2 %~ BodyFunction

instance Functor i => MarkBodyAnnotations v n i o Else where
    markBodyAnnotations (SimpleElse body) = markBodyAnnotations body & _2 %~ SimpleElse . markCaseHandler
    markBodyAnnotations (ElseIf x) =
        ( neverShowAnnotations
        , morphMap (Proxy @(MarkAnnotations v n i o) #?> markNodeAnnotations) x & ElseIf
        )

instance Functor i => MarkBodyAnnotations v n i o Function where
    markBodyAnnotations func =
        ( neverShowAnnotations
        , func
            { _fBody = func ^. fBody & markNodeAnnotations
            , _fParams = func ^. fParams & SugarLens.binderParamsAnnotations %~ (,) showAnnotationWhenVerbose
            }
        )

instance Functor i => MarkBodyAnnotations v n i o IfElse where
    markBodyAnnotations (IfElse i t e) =
        ( showAnnotationWhenVerbose
        , IfElse (markNodeAnnotations i) (markNodeAnnotations t & hVal %~ markCaseHandler) (markNodeAnnotations e)
        )

instance Functor i => MarkBodyAnnotations v n i o Composite where
    markBodyAnnotations x =
        ( neverShowAnnotations
        , morphMap (Proxy @(MarkAnnotations v n i o) #?> markNodeAnnotations) x
        )

instance Functor i => MarkBodyAnnotations v n i o PostfixFunc where
    markBodyAnnotations x =
        ( neverShowAnnotations
        , morphMap (Proxy @(MarkAnnotations v n i o) #?> markNodeAnnotations) x
        )

instance Functor i => MarkBodyAnnotations v n i o Term where
    markBodyAnnotations BodyPlaceHolder = (neverShowAnnotations, BodyPlaceHolder)
    markBodyAnnotations (BodyLiteral x@LiteralBytes{}) = (dontShowEval, BodyLiteral x)
    markBodyAnnotations (BodyLiteral x) = (neverShowAnnotations, BodyLiteral x)
    markBodyAnnotations (BodyRecord x) = markBodyAnnotations x & _2 %~ BodyRecord
    markBodyAnnotations (BodyLam x) = lamFunc markBodyAnnotations x & _2 %~ BodyLam
    markBodyAnnotations (BodyGetVar x) =
        ( case x of
            GetParamsRecord{} -> showAnnotationWhenVerbose
            GetParam ParamRef{ _pBinderMode = LightLambda } -> showAnnotationWhenVerbose
            GetBinder BinderVarRef { _bvForm = GetDefinition{} } -> showAnnotationWhenVerbose
            _ -> neverShowAnnotations
        , BodyGetVar x
        )
    markBodyAnnotations (BodyPostfixFunc x) = markBodyAnnotations x & _2 %~ BodyPostfixFunc
    markBodyAnnotations (BodyToNom (Nominal tid binder)) =
        ( showAnnotationWhenVerbose
            & showInEvalMode .~
                ( tid ^. tidTId == Builtins.textTid
                    || newBinder ^. SugarLens.binderResultExpr . _1 . plAnnotation . _1 . showInEvalMode
                )
        , newBinder
            & Lens.filtered (not . SugarLens.isUnfinished . (^. hVal)) .
                annotation . _1 . plAnnotation . _1 .~ dontShowEval
            & Nominal tid & BodyToNom
        )
        where
            newBinder = markNodeAnnotations binder
    markBodyAnnotations (BodyInject x) = (dontShowEval, BodyInject x)
    markBodyAnnotations (BodyEmptyInject x) = (dontShowEval, BodyEmptyInject x)
    markBodyAnnotations (BodySimpleApply x) =
        ( showAnnotationWhenVerbose
        , morphMap (Proxy @(MarkAnnotations v n i o) #?> markNodeAnnotations) x
            & appFunc . nonHoleAnn .~ neverShowAnnotations
            & BodySimpleApply
        )
    markBodyAnnotations (BodyPostfixApply x) =
        ( neverShowAnnotations -- No need to see result of from-nom/get-field
        , morphMap (Proxy @(MarkAnnotations v n i o) #?> markNodeAnnotations) x & BodyPostfixApply
        )
    markBodyAnnotations (BodyLabeledApply x) =
        ( showAnnotationWhenVerbose
        , morphMap (Proxy @(MarkAnnotations v n i o) #?> markNodeAnnotations) x & BodyLabeledApply
        )
    markBodyAnnotations (BodyIfElse x) = markBodyAnnotations x & _2 %~ BodyIfElse
    markBodyAnnotations (BodyHole x) =
        ( alwaysShowAnnotations
        , x & BodyHole
        )
    markBodyAnnotations (BodyFragment (Fragment e h t o)) =
        ( alwaysShowAnnotations
        , Fragment
            ( markNodeAnnotations e
                & if Lens.has Lens._Just t
                    then nonHoleAnn .~ dontShowType
                    else id
            ) h t o
            & BodyFragment
        )

nonHoleAnn ::
    Lens.Traversal' (Annotated (Payload (ShowAnnotation, v0) n i o, a) # Term v1 n i o) ShowAnnotation
nonHoleAnn =
    Lens.filtered (Lens.nullOf (hVal . SugarLens.bodyUnfinished)) .
    annotation . _1 . plAnnotation . _1

markCaseHandler ::
    Term v n i o # Annotated (Payload (ShowAnnotation, v0) n i o, a) ->
    Term v n i o # Annotated (Payload (ShowAnnotation, v0) n i o, a)
markCaseHandler =
    _BodyLam . lamFunc . fBody .
    SugarLens.binderResultExpr . Lens.ifiltered (const . Lens.nullOf SugarLens.bodyUnfinished) .
    _1 . plAnnotation . _1
    .~ neverShowAnnotations
