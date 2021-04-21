{-# LANGUAGE TemplateHaskell, TypeApplications, DataKinds, PolyKinds, UndecidableInstances #-}
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
import           Lamdu.Sugar.Convert.Input (userData)
import           Lamdu.Sugar.Internal (ConvertPayload, pInput)
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

class MarkBodyAnnotations v m e where
    markBodyAnnotations ::
        e v n i o # Annotated (ConvertPayload m a) ->
        (ShowAnnotation, e (ShowAnnotation, v) n i o # Annotated (ConvertPayload m (ShowAnnotation, a)))

class MarkAnnotations m t0 t1 where
    markNodeAnnotations ::
        Annotated (ConvertPayload m a) # t0 ->
        Annotated (ConvertPayload m (ShowAnnotation, a)) # t1

instance MarkAnnotations m (Const a) (Const a) where
    markNodeAnnotations (Ann a (Const b)) = Ann (a & Lens._Wrapped . pInput . userData %~ (,) neverShowAnnotations) (Const b)

instance MarkBodyAnnotations v m e => MarkAnnotations m (e v n i o) (e (ShowAnnotation, v) n i o) where
    markNodeAnnotations (Ann (Const pl) x) =
        Ann (Const (pl & pInput . userData %~ (,) showAnn)) newBody
        where
            (showAnn, newBody) = markBodyAnnotations x

instance Functor m => MarkBodyAnnotations v m Binder where
    markBodyAnnotations (BinderTerm body) =
        markBodyAnnotations body & _2 %~ BinderTerm
    markBodyAnnotations (BinderLet let_) =
        ( neverShowAnnotations
        , morphMap (Proxy @(MarkAnnotations m) #?> markNodeAnnotations) let_
            & BinderLet
        )

instance Functor m => MarkBodyAnnotations v m Assignment where
    markBodyAnnotations (BodyPlain (AssignPlain a b)) =
        markBodyAnnotations b
        & _2 %~ BodyPlain . AssignPlain a
    markBodyAnnotations (BodyFunction f) = markBodyAnnotations f & _2 %~ BodyFunction

instance Functor m => MarkBodyAnnotations v m Else where
    markBodyAnnotations (SimpleElse body) = markBodyAnnotations body & _2 %~ SimpleElse . markCaseHandler
    markBodyAnnotations (ElseIf x) =
        ( neverShowAnnotations
        , morphMap (Proxy @(MarkAnnotations m) #?> markNodeAnnotations) x & ElseIf
        )

instance Functor m => MarkBodyAnnotations v m Function where
    markBodyAnnotations func =
        ( neverShowAnnotations
        , func
            { _fBody = func ^. fBody & markNodeAnnotations
            , _fParams = func ^. fParams & SugarLens.annotations @v %~ (,) showAnnotationWhenVerbose
            }
        )

instance Functor m => MarkBodyAnnotations v m IfElse where
    markBodyAnnotations (IfElse i t e) =
        ( showAnnotationWhenVerbose
        , IfElse (markNodeAnnotations i) (markNodeAnnotations t & hVal %~ markCaseHandler) (markNodeAnnotations e)
        )

instance Functor m => MarkBodyAnnotations v m Composite where
    markBodyAnnotations x =
        ( neverShowAnnotations
        , morphMap (Proxy @(MarkAnnotations m) #?> markNodeAnnotations) x
        )

instance Functor m => MarkBodyAnnotations v m PostfixFunc where
    markBodyAnnotations x =
        ( neverShowAnnotations
        , morphMap (Proxy @(MarkAnnotations m) #?> markNodeAnnotations) x
        )

instance Functor m => MarkBodyAnnotations v m Term where
    markBodyAnnotations (BodyLeaf LeafPlaceHolder) = (neverShowAnnotations, BodyLeaf LeafPlaceHolder)
    markBodyAnnotations (BodyLeaf (LeafLiteral x@LiteralBytes{})) = (dontShowEval, BodyLeaf (LeafLiteral x))
    markBodyAnnotations (BodyLeaf (LeafLiteral x)) = (neverShowAnnotations, BodyLeaf (LeafLiteral x))
    markBodyAnnotations (BodyRecord x) = markBodyAnnotations x & _2 %~ BodyRecord
    markBodyAnnotations (BodyLam x) = lamFunc markBodyAnnotations x & _2 %~ BodyLam
    markBodyAnnotations (BodyLeaf (LeafGetVar x)) =
        ( case x of
            GetParamsRecord{} -> showAnnotationWhenVerbose
            GetParam ParamRef{ _pBinderMode = LightLambda } -> showAnnotationWhenVerbose
            GetBinder BinderVarRef { _bvForm = GetDefinition{} } -> showAnnotationWhenVerbose
            _ -> neverShowAnnotations
        , LeafGetVar x & BodyLeaf
        )
    markBodyAnnotations (BodyPostfixFunc x) = markBodyAnnotations x & _2 %~ BodyPostfixFunc
    markBodyAnnotations (BodyToNom (Nominal tid binder)) =
        ( showAnnotationWhenVerbose
            & showInEvalMode .~
                ( tid ^. tidTId == Builtins.textTid
                    || newBinder ^. SugarLens.binderResultExpr . pInput . userData . _1 . showInEvalMode
                )
        , newBinder
            & Lens.filtered (not . SugarLens.isUnfinished . (^. hVal)) .
                annotation . pInput . userData . _1 .~ dontShowEval
            & Nominal tid & BodyToNom
        )
        where
            newBinder = markNodeAnnotations binder
    markBodyAnnotations (BodyLeaf (LeafInject x)) = (dontShowEval, BodyLeaf (LeafInject x))
    markBodyAnnotations (BodyNullaryInject x) =
        ( dontShowEval
        , x & morphMapped1 %~
                (\(Ann a (Const b)) ->
                    Ann (a & Lens._Wrapped . pInput . userData %~ (,) neverShowAnnotations) (Const b))
            & BodyNullaryInject
        )
    markBodyAnnotations (BodySimpleApply x) =
        ( showAnnotationWhenVerbose
        , morphMap (Proxy @(MarkAnnotations m) #?> markNodeAnnotations) x
            & appFunc . nonHoleAnn .~ neverShowAnnotations
            & BodySimpleApply
        )
    markBodyAnnotations (BodyPostfixApply x) =
        ( neverShowAnnotations -- No need to see result of from-nom/get-field
        , morphMap (Proxy @(MarkAnnotations m) #?> markNodeAnnotations) x & BodyPostfixApply
        )
    markBodyAnnotations (BodyLabeledApply x) =
        ( showAnnotationWhenVerbose
        , morphMap (Proxy @(MarkAnnotations m) #?> markNodeAnnotations) x & BodyLabeledApply
        )
    markBodyAnnotations (BodyIfElse x) = markBodyAnnotations x & _2 %~ BodyIfElse
    markBodyAnnotations (BodyLeaf (LeafHole x)) =
        ( alwaysShowAnnotations
        , LeafHole x & BodyLeaf
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
    Lens.Traversal' (Annotated (ConvertPayload m (ShowAnnotation, a)) # Term v1 n i o) ShowAnnotation
nonHoleAnn =
    Lens.filtered (Lens.nullOf (hVal . SugarLens.bodyUnfinished)) .
    annotation . pInput . userData . _1

markCaseHandler ::
    Term v n i o # Annotated (ConvertPayload m (ShowAnnotation, a)) ->
    Term v n i o # Annotated (ConvertPayload m (ShowAnnotation, a))
markCaseHandler =
    _BodyLam . lamFunc . fBody .
    SugarLens.binderResultExpr . Lens.ifiltered (const . Lens.nullOf SugarLens.bodyUnfinished) .
    pInput . userData . _1
    .~ neverShowAnnotations
