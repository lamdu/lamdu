{-# LANGUAGE TypeApplications, FlexibleInstances, DefaultSignatures, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Lamdu.Sugar.Lens
    ( Annotations(..), HAnnotations(..)
    , childPayloads
    , bodyUnfinished
    , defSchemes
    , binderResultExpr
    , getVarName
    , paneBinder
    , unfinishedPayloads
    , taggedListItems, taggedListBodyItems
    , taggedItemTagChoices
    , tagChoiceOptions
    ) where

import           Control.Lens (Traversal)
import qualified Control.Lens as Lens
import           Control.Monad.Unit (Unit)
import           Hyper
import           Hyper.Class.Morph
import           Lamdu.Sugar.Props (SugarExpr(..), binderVarRefUnfinished)
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

childPayloads ::
    HTraversable expr =>
    Lens.Traversal' (expr # Annotated a) a
childPayloads f =
    htraverse (const (annotation f))

unfinishedPayloads ::
    forall t a.
    SugarExpr t =>
    Lens.Traversal' (Annotated a # t) a
unfinishedPayloads f (Ann (Const a) x) =
    withDict (sugarExprRecursive (Proxy @t)) $
    flip Ann
    <$> htraverse (Proxy @SugarExpr #> unfinishedPayloads f) x
    <*> ((if isUnfinished x then f a else pure a) <&> Const)

bodyUnfinished :: Lens.Traversal' (Term v name i o # Ann a) ()
bodyUnfinished =
    _BodyLeaf . _LeafHole . Lens.united
    & Lens.failing (_BodyFragment . Lens.united)
    & Lens.failing (_BodyLeaf . _LeafGetVar . _GetBinder . binderVarRefUnfinished)
    & Lens.failing (_BodyLabeledApply . aFunc . hVal . Lens._Wrapped . binderVarRefUnfinished)

defBodySchemes :: Lens.Traversal' (DefinitionBody v name i o expr) (Scheme name Unit)
defBodySchemes f (DefinitionBodyBuiltin b) =
    b & biType %%~ f
    <&> DefinitionBodyBuiltin
defBodySchemes f (DefinitionBodyExpression de) =
    de & deType %%~ f
    <&> DefinitionBodyExpression

defSchemes :: Lens.Traversal' (Definition v name i o expr) (Scheme name Unit)
defSchemes = drBody . defBodySchemes

binderResultExpr ::
    Lens.IndexedLens' (Term v name i o # Annotated ()) (Annotated a # Binder v name i o) a
binderResultExpr f (Ann (Const pl) x) =
    case x ^. bBody of
    BinderTerm e ->
        Lens.indexed f
        (hmap (Proxy @(Recursively HFunctor) #> hflipped %~ hmap (\_ Const{} -> Const ())) e)
        pl
        <&> Const
        <&> (`Ann` x)
    BinderLet l ->
        lBody (binderResultExpr f) l
        <&> BinderLet
        <&> Binder (x ^. bAddOuterLet)
        <&> Ann (Const pl)

getVarName :: Lens.Traversal' (GetVar a o) a
getVarName f (GetParam x) = (pNameRef . nrName) f x <&> GetParam
getVarName f (GetBinder x) = (bvNameRef . nrName) f x <&> GetBinder
getVarName _ (GetParamsRecord x) = GetParamsRecord x & pure

binderParamsFuncParams ::
    Traversal
    (Params v0 name i o)
    (Params v1 name i o)
    (FuncParam v0)
    (FuncParam v1)
binderParamsFuncParams f (NullParam x) = _1 f x <&> NullParam
binderParamsFuncParams f (RecordParams x) = traverse f x <&> RecordParams
binderParamsFuncParams f (VarParam x) = _1 f x <&> VarParam

paneBinder :: Traversal (Pane v0 n i o a0) (Pane v1 n i o a1) (Annotated a0 # Assignment v0 n i o) (Annotated a1 # Assignment v1 n i o)
paneBinder = paneBody . _PaneDefinition . drBody . _DefinitionBodyExpression . deContent

taggedListBodyItems ::
    Traversal (TaggedListBody n0 i0 o a0) (TaggedListBody n1 i1 o a1)
    (TaggedItem n0 i0 o a0) (TaggedItem n1 i1 o a1)
taggedListBodyItems f (TaggedListBody hd tl) = TaggedListBody <$> f hd <*> (traverse . tsiItem) f tl

taggedListItems ::
    Traversal (TaggedList n i o a0) (TaggedList n i o a1)
    (TaggedItem n i o a0) (TaggedItem n i o a1)
taggedListItems = tlItems . Lens._Just . taggedListBodyItems

taggedItemTagChoices :: Functor i => Lens.Setter' (TaggedItem n i o a) (TagChoice n o)
taggedItemTagChoices =
    Lens.sets (\f (TaggedItem t d a v) -> TaggedItem (t & tagRefReplace . Lens.mapped %~ f) d (a <&> f) v)

tagChoiceOptions :: Lens.Setter (TagChoice n0 o0) (TagChoice n1 o1) (TagOption n0 o0) (TagOption n1 o1)
tagChoiceOptions = Lens.sets (\f (TagChoice o n) -> TagChoice (o <&> f) (f n))

class Annotations a b s t where
    annotations :: Traversal s t a b
    default annotations ::
        ( s ~ (e0 # h0), t ~ (e1 # h1)
        , HAnnotations a b e0 e1, HAnnotations a b h0 h1
        ) => Traversal s t a b
    annotations = hAnnotations

class HAnnotations a b s t where
    hAnnotations ::
        HAnnotations a b h0 h1 =>
        Traversal (s # h0) (t # h1) a b
    default hAnnotations ::
        ( HMorphWithConstraint s t (HAnnotations a b)
        , HTraversable t
        , HAnnotations a b h0 h1
        ) => Traversal (s # h0) (t # h1) a b
    hAnnotations f = morphTraverse (Proxy @(HAnnotations a b) #?> hAnnotations f)

instance Annotations a b (BinderVarRef n o) (BinderVarRef n o) where annotations _ = pure
instance Annotations a b (GetVar n o) (GetVar n o) where annotations _ = pure
instance Annotations a b (TagRef n i o) (TagRef n i o) where annotations _ = pure
instance Annotations a b (i (TagChoice n o)) (i (TagChoice n o)) where annotations _ = pure

instance Annotations a b s0 t0 => Annotations a b (s0, x) (t0, x) where
    annotations = _1 . annotations

instance Annotations a b (Params a n i o) (Params b n i o) where
    annotations = binderParamsFuncParams . fpAnnotation

instance Annotations a b (Payload a o) (Payload b o) where
    annotations = plAnnotation

instance Annotations a b s t => Annotations a b (WorkArea a n i o s) (WorkArea b n i o t) where
    annotations f (WorkArea panes repl globals) =
        WorkArea
        <$> (traverse . paneBinder . hAnnotations) f panes
        <*> (replExpr . hAnnotations) f repl
        ?? globals

instance HAnnotations a b p0 p1 => HAnnotations a b (Ann p0) (Ann p1) where
    hAnnotations f (Ann p x) = Ann <$> hAnnotations f p <*> hAnnotations f x

instance Annotations a b s t => HAnnotations a b (Const s) (Const t) where
    hAnnotations = Lens._Wrapped . annotations

instance HAnnotations a b (Composite a n i o) (Composite b n i o)
instance HAnnotations a b (IfElse a n i o) (IfElse b n i o)
instance HAnnotations a b (LabeledApply a n i o) (LabeledApply b n i o)
instance HAnnotations a b (Let a n i o) (Let b n i o)
instance HAnnotations a b (PostfixApply a n i o) (PostfixApply b n i o)
instance HAnnotations a b (PostfixFunc a n i o) (PostfixFunc b n i o)

instance HAnnotations a b (FragOpt a n i o) (FragOpt b n i o) where
    hAnnotations f (FragPostfix x) = (traverse . hAnnotations) f x <&> FragPostfix
    hAnnotations _ (FragInject x) = FragInject x & pure
    hAnnotations _ (FragWrapInRec x) = FragWrapInRec x & pure
    hAnnotations _ (FragApplyFunc x) = FragApplyFunc x & pure
    hAnnotations f (FragOp x) = morphTraverse (Proxy @(HAnnotations a b) #?> hAnnotations f) x <&> FragOp
    hAnnotations _ (FragToNom x) = FragToNom x & pure
    hAnnotations _ FragLam = pure FragLam
    hAnnotations _ FragDefer = pure FragDefer
    hAnnotations f (FragIf x) = hAnnotations f x <&> FragIf
    hAnnotations f (FragArgument x) = hAnnotations f x <&> FragArgument

instance HAnnotations a b (Assignment a n i o) (Assignment b n i o) where
    hAnnotations f (BodyFunction x) = hAnnotations f x <&> BodyFunction
    hAnnotations f (BodyPlain x) = (apBody . hAnnotations) f x <&> BodyPlain

instance HAnnotations a b (Binder a n i o) (Binder b n i o) where
    hAnnotations = bBody . hAnnotations

instance HAnnotations a b (BinderBody a n i o) (BinderBody b n i o) where
    hAnnotations f (BinderLet x) = hAnnotations f x <&> BinderLet
    hAnnotations f (BinderTerm x) = hAnnotations f x <&> BinderTerm

instance HAnnotations a b (Else a n i o) (Else b n i o) where
    hAnnotations f (SimpleElse x) = hAnnotations f x <&> SimpleElse
    hAnnotations f (ElseIf x) = (eIfElse . hAnnotations) f x <&> ElseIf

instance HAnnotations a b (Fragment a n i o) (Fragment b n i o) where
    hAnnotations = fExpr . hAnnotations

instance HAnnotations a b (Function a n i o) (Function b n i o) where
    hAnnotations f x =
        (,)
        <$> annotations f (x ^. fParams)
        <*> hAnnotations f (x ^. fBody)
        <&> \(p, b) -> x { _fParams = p, _fBody = b }

instance HAnnotations a b (Term a n i o) (Term b n i o) where
    hAnnotations _ (BodyLeaf x) = BodyLeaf x & pure
    hAnnotations f (BodyLam x) = (lamFunc . hAnnotations) f x <&> BodyLam
    hAnnotations f (BodyIfElse x) = hAnnotations f x <&> BodyIfElse
    hAnnotations f (BodyRecord x) = hAnnotations f x <&> BodyRecord
    hAnnotations f (BodyToNom x) = (nVal . hAnnotations) f x <&> BodyToNom
    hAnnotations f (BodySimpleApply x) = (morphTraverse1 . hAnnotations) f x <&> BodySimpleApply
    hAnnotations f (BodyLabeledApply x) = hAnnotations f x <&> BodyLabeledApply
    hAnnotations f (BodyFragment x) = hAnnotations f x <&> BodyFragment
    hAnnotations f (BodyPostfixApply x) = hAnnotations f x <&> BodyPostfixApply
    hAnnotations f (BodyPostfixFunc x) = hAnnotations f x <&> BodyPostfixFunc
    hAnnotations f (BodyNullaryInject (NullaryInject i r)) =
        NullaryInject <$> hAnnotations f i <*> hAnnotations f r <&> BodyNullaryInject
