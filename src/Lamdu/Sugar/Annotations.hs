{-# LANGUAGE TemplateHaskell, TypeApplications, PolyKinds, UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Lamdu.Sugar.Annotations
    ( ShowAnnotation(..), showTypeAlways, showInTypeMode, showInEvalMode
    , MarkAnnotations(..), alwaysShowAnnotations
    ) where

import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Class.Morph
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Props as SugarProps
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

class MarkBodyAnnotations v e where
    markBodyAnnotations ::
        e v n i o # Annotated a ->
        (ShowAnnotation, e (ShowAnnotation, v) n i o # Annotated (ShowAnnotation, a))

class MarkAnnotations t0 t1 where
    markNodeAnnotations ::
        Annotated a # t0 ->
        Annotated (ShowAnnotation, a) # t1

instance MarkAnnotations (Const a) (Const a) where
    markNodeAnnotations (Ann a (Const b)) = Ann (a & Lens._Wrapped %~ (,) neverShowAnnotations) (Const b)

instance MarkBodyAnnotations v e => MarkAnnotations (e v n i o) (e (ShowAnnotation, v) n i o) where
    markNodeAnnotations (Ann (Const pl) x) =
        Ann (Const (showAnn, pl)) newBody
        where
            (showAnn, newBody) = markBodyAnnotations x

instance MarkBodyAnnotations v Binder where
    markBodyAnnotations = bBody markBodyAnnotations

instance MarkBodyAnnotations v BinderBody where
    markBodyAnnotations (BinderTerm body) = markBodyAnnotations body & _2 %~ BinderTerm
    markBodyAnnotations (BinderLet let_) = markBodyAnnotations let_ & _2 %~ BinderLet

instance  MarkBodyAnnotations v Let where
    markBodyAnnotations l =
        ( neverShowAnnotations
        , l { _lValue = l ^. lValue & markNodeAnnotations
            , _lNames = l ^. lNames & markParamAnnotations
            , _lBody = l ^. lBody & markNodeAnnotations
            }
        )

instance MarkBodyAnnotations v Assignment where
    markBodyAnnotations (BodyPlain (AssignPlain a b)) =
        markBodyAnnotations b
        & _2 %~ BodyPlain . AssignPlain a
    markBodyAnnotations (BodyFunction f) = markBodyAnnotations f & _2 %~ BodyFunction

instance MarkBodyAnnotations v Else where
    markBodyAnnotations (SimpleElse body) = markBodyAnnotations body & _2 %~ SimpleElse . markCaseHandler
    markBodyAnnotations (ElseIf x) =
        ( neverShowAnnotations
        , x & eIfElse %~ morphMap (Proxy @MarkAnnotations #?> markNodeAnnotations) & ElseIf
        )

instance MarkBodyAnnotations v Function where
    markBodyAnnotations func =
        ( neverShowAnnotations
        , func
            { _fBody = func ^. fBody & markNodeAnnotations
            , _fParams = func ^. fParams & markParamAnnotations
            }
        )

markParamAnnotations :: LhsNames n i o v -> LhsNames n i o (ShowAnnotation, v)
markParamAnnotations (LhsVar v) = v <&> (,) showAnnotationWhenVerbose & LhsVar
markParamAnnotations (LhsRecord r) = r <&> markLhsFieldAnnotations & LhsRecord

markLhsFieldAnnotations :: LhsField n v -> LhsField n (ShowAnnotation, v)
markLhsFieldAnnotations (LhsField f s) =
    LhsField (f <&> (,) r) (s <&> traverse . _2 %~ markLhsFieldAnnotations)
    where
        r = case s of
            Nothing -> showAnnotationWhenVerbose
            Just{} -> neverShowAnnotations

instance MarkBodyAnnotations v HoleOpt where
    markBodyAnnotations (HoleBinder t) = markBodyAnnotations t & _2 %~ HoleBinder
    markBodyAnnotations (HoleVarsRecord x) = (neverShowAnnotations, HoleVarsRecord x)

instance MarkBodyAnnotations v IfElse where
    markBodyAnnotations (IfElse i t e) =
        ( showAnnotationWhenVerbose
        , IfElse (markNodeAnnotations i) (markNodeAnnotations t & hVal %~ markCaseHandler) (markNodeAnnotations e)
        )

instance MarkBodyAnnotations v Composite where
    markBodyAnnotations x =
        ( neverShowAnnotations
        , morphMap (Proxy @MarkAnnotations #?> markNodeAnnotations) x
        )

instance MarkBodyAnnotations v PostfixFunc where
    markBodyAnnotations x =
        ( neverShowAnnotations
        , morphMap (Proxy @MarkAnnotations #?> markNodeAnnotations) x
        )

instance MarkBodyAnnotations v Term where
    markBodyAnnotations (BodyLeaf (LeafLiteral x@LiteralBytes{})) = (dontShowEval, BodyLeaf (LeafLiteral x))
    markBodyAnnotations (BodyLeaf (LeafLiteral x)) = (neverShowAnnotations, BodyLeaf (LeafLiteral x))
    markBodyAnnotations (BodyRecord x) = markBodyAnnotations x & _2 %~ BodyRecord
    markBodyAnnotations (BodyLam x) = lamFunc markBodyAnnotations x & _2 %~ BodyLam
    markBodyAnnotations (BodyLeaf (LeafGetVar x)) =
        ( case x ^. vForm of
            GetLightParam -> showAnnotationWhenVerbose
            GetDefinition{} -> showAnnotationWhenVerbose
            _ -> neverShowAnnotations
        , LeafGetVar x & BodyLeaf
        )
    markBodyAnnotations (BodyPostfixFunc x) = markBodyAnnotations x & _2 %~ BodyPostfixFunc
    markBodyAnnotations (BodyToNom (Nominal tid binder)) =
        ( showAnnotationWhenVerbose
            & showInEvalMode .~
                ( tid ^. tidTId == Builtins.textTid
                    || newBinder ^. SugarLens.binderResultExpr . _1 . showInEvalMode
                )
        , newBinder
            & Lens.filtered (not . SugarProps.isUnfinished . (^. hVal)) .
                annotation . _1 .~ dontShowEval
            & Nominal tid & BodyToNom
        )
        where
            newBinder = markNodeAnnotations binder
    markBodyAnnotations (BodyLeaf (LeafInject x)) = (dontShowEval, BodyLeaf (LeafInject x))
    markBodyAnnotations (BodyNullaryInject x) =
        ( dontShowEval
        , x & hmap
                (Proxy @(Recursively HFunctor) #>
                    hflipped %~ hmap (const (Lens._Wrapped %~ (,) neverShowAnnotations)))
            & BodyNullaryInject
        )
    markBodyAnnotations (BodySimpleApply x) =
        ( showAnnotationWhenVerbose
        , morphMap (Proxy @MarkAnnotations #?> markNodeAnnotations) x
            & appFunc . nonHoleAnn .~ neverShowAnnotations
            & BodySimpleApply
        )
    markBodyAnnotations (BodyPostfixApply x) =
        ( neverShowAnnotations -- No need to see result of from-nom/get-field
        , morphMap (Proxy @MarkAnnotations #?> markNodeAnnotations) x & BodyPostfixApply
        )
    markBodyAnnotations (BodyLabeledApply x) =
        ( showAnnotationWhenVerbose
        , morphMap (Proxy @MarkAnnotations #?> markNodeAnnotations) x & BodyLabeledApply
        )
    markBodyAnnotations (BodyIfElse x) = markBodyAnnotations x & _2 %~ BodyIfElse
    markBodyAnnotations (BodyLeaf (LeafHole x)) =
        ( alwaysShowAnnotations
        , BodyLeaf (LeafHole x)
        )
    markBodyAnnotations (BodyFragment f) =
        ( alwaysShowAnnotations
        , f & fExpr %~
                (if Lens.has (fTypeMismatch . Lens._Just) f then nonHoleAnn .~ dontShowType else id) .
                markNodeAnnotations
            & BodyFragment
        )

nonHoleAnn ::
    Lens.Traversal' (Annotated (ShowAnnotation, a) # Term v1 n i o) ShowAnnotation
nonHoleAnn =
    Lens.filtered (Lens.nullOf (hVal . SugarLens.bodyUnfinished)) .
    annotation . _1

markCaseHandler ::
    Term v n i o # Annotated (ShowAnnotation, a) ->
    Term v n i o # Annotated (ShowAnnotation, a)
markCaseHandler =
    _BodyLam . lamFunc . fBody .
    SugarLens.binderResultExpr . Lens.ifiltered (const . Lens.nullOf SugarLens.bodyUnfinished) . _1
    .~ neverShowAnnotations
