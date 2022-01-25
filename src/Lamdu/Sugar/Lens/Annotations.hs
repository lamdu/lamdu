{-# LANGUAGE TypeApplications, FlexibleInstances, DefaultSignatures, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}

module Lamdu.Sugar.Lens.Annotations
    ( Annotations(..), HAnnotations(..), paneBinder
    ) where

import           Control.Lens (Traversal)
import qualified Control.Lens as Lens
import           Hyper
import           Hyper.Class.Morph
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

binderParamsFuncParams ::
    Traversal
    (Params v0 name i o)
    (Params v1 name i o)
    (FuncParam v0)
    (FuncParam v1)
binderParamsFuncParams f (ParamsRecord x) = traverse f x <&> ParamsRecord
binderParamsFuncParams f (ParamVar x) = vParam f x <&> ParamVar

paneBinder :: Traversal (Pane v0 n i o a0) (Pane v1 n i o a1) (Annotated a0 # Assignment v0 n i o) (Annotated a1 # Assignment v1 n i o)
paneBinder = paneBody . _PaneDefinition . drBody . _DefinitionBodyExpression . deContent

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

instance Annotations a b s0 t0 => Annotations a b (s0, x) (t0, x) where
    annotations = _1 . annotations

instance Annotations a b (Params a n i o) (Params b n i o) where
    annotations = binderParamsFuncParams . fpAnnotation

instance Annotations a b (Payload a o) (Payload b o) where
    annotations = plAnnotation

instance
    (HAnnotations a b (Ann (Const s)) (Ann (Const t)), r ~ WorkArea b n i o t) =>
    Annotations a b (WorkArea a n i o s) r where
    annotations f (WorkArea panes repl globals) =
        WorkArea
        <$> (traverse . paneBinder . hAnnotations) f panes
        <*> (replExpr . hAnnotations) f repl
        ?? globals

instance r ~ Ann (Const (b, x, y)) => HAnnotations a b (Ann (Const (a, x, y))) r where
    hAnnotations f (Ann p x) =
        Ann
        <$> (Lens._Wrapped . _1) f p
        <*> hAnnotations f x

instance HAnnotations a b (Ann (Const (Payload a o))) (Ann (Const (Payload b o))) where
    hAnnotations f (Ann p x) =
        Ann
        <$> (Lens._Wrapped . plAnnotation) f p
        <*> hAnnotations f x

instance HAnnotations a b (Const (GetVar n o)) (Const (GetVar n o)) where hAnnotations _ = Lens._Wrapped pure
instance HAnnotations a b (Const (i (TagChoice n o))) (Const (i (TagChoice n o))) where hAnnotations _ = Lens._Wrapped pure
instance HAnnotations a b (Const (TagRef n i o)) (Const (TagRef n i o)) where hAnnotations _ = Lens._Wrapped pure
instance HAnnotations a b (Composite a n i o) (Composite b n i o)
instance HAnnotations a b (IfElse a n i o) (IfElse b n i o)
instance HAnnotations a b (LabeledApply a n i o) (LabeledApply b n i o)
instance HAnnotations a b (Let a n i o) (Let b n i o)
instance HAnnotations a b (PostfixApply a n i o) (PostfixApply b n i o)
instance HAnnotations a b (PostfixFunc a n i o) (PostfixFunc b n i o)

instance r ~ HoleOpt b n i o => HAnnotations a b (HoleOpt a n i o) r where
    hAnnotations f (HoleBinder x) = hAnnotations f x <&> HoleBinder
    hAnnotations _ (HoleVarsRecord x) = HoleVarsRecord x & pure

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
