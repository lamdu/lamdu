{-# LANGUAGE UndecidableInstances, TypeFamilies, FlexibleInstances, RankNTypes, TypeApplications, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lamdu.Sugar.Lens.Actions
    ( Actions(..)
    ) where

import qualified Control.Lens as Lens
import           Data.Property (Property)
import qualified Data.Property as Property
import           Hyper
import           Hyper.Class.Morph (morphMapped1)
import qualified Lamdu.Sugar.Types.Eval as Eval
import qualified Lamdu.Sugar.Types.Expression as Expr
import qualified Lamdu.Sugar.Types.GetVar as GetVar
import           Lamdu.Sugar.Types.NameRef (NameRef)
import qualified Lamdu.Sugar.Types.NameRef as NameRef
import qualified Lamdu.Sugar.Types.Parts as Parts
import qualified Lamdu.Sugar.Types.Tag as Tag
import qualified Lamdu.Sugar.Types.Type as Type

import           Lamdu.Prelude

class Actions s t a b where
    actions :: (forall x. a x -> b x) -> s -> t

class HActions s t a b where
    hActionsBody ::
        Recursively HFunctor k =>
        (forall x. a x -> b x) -> s # k -> t # k

instance Actions (NameRef name o0) (NameRef name o1) o0 o1 where
    actions = Lens.over NameRef.nrGotoDefinition

instance Actions (Tag.TagOption name o0) (Tag.TagOption name o1) o0 o1 where
    actions = Lens.over Tag.toPick

instance Actions (Tag.TagChoice name o0) (Tag.TagChoice name o1) o0 o1 where
    actions f (Tag.TagChoice x y) = Tag.TagChoice (fmap (actions f) x) (actions f y)

instance Functor i => Actions (Tag.TagRef name i o0) (Tag.TagRef name i o1) o0 o1 where
    actions f (Tag.TagRef tag repl jump) =
        Tag.TagRef tag (fmap (actions f) repl) (fmap f jump)

instance Functor i => Actions (Tag.OptionalTag name i o0) (Tag.OptionalTag name i o1) o0 o1 where
    actions f (Tag.OptionalTag x y) = Tag.OptionalTag (actions f x) (f y)

instance Actions (Type.TId name o0) (Type.TId name o1) o0 o1 where
    actions = Lens.over Type.tidGotoDefinition

recursiveHMap ::
    forall k h0 h1. Recursively HFunctor k =>
    (forall n. Recursively HFunctor n => h0 # n -> h1 # n) -> k # h0 -> k # h1
recursiveHMap f =
    withDict (recursively (Proxy @(HFunctor k))) $
    hmap (Proxy @(Recursively HFunctor) #> f)

hActions ::
    (HActions s t a b, Recursively HFunctor k) =>
    (forall x. a x -> b x) -> k # s -> k # t
hActions f = recursiveHMap (hActionsBody f)

instance HActions (Type.Type name o0) (Type.Type name o1) o0 o1 where
    hActionsBody _ (Type.TVar x) = Type.TVar x
    hActionsBody f (Type.TFun t) = t & morphMapped1 %~ hActions f & Type.TFun
    hActionsBody f (Type.TInst tid args) =
        Type.TInst (actions f tid) (args <&> _2 %~ hActions f)
    hActionsBody f (Type.TRecord fields) = fmap (hActions f) fields & Type.TRecord
    hActionsBody f (Type.TVariant fields) = fmap (hActions f) fields & Type.TVariant

instance Actions (Type.Scheme name o0) (Type.Scheme name o1) o0 o1 where
    actions f = Type.schemeType %~ hmap (Proxy @(Recursively HFunctor) #> hActionsBody f)

instance Actions (GetVar.DefinitionOutdatedType name o0 a) (GetVar.DefinitionOutdatedType name o1 a) o0 o1 where
    actions = Lens.over GetVar.defTypeUseCurrent

instance Actions (GetVar.DefinitionForm name o0) (GetVar.DefinitionForm name o1) o0 o1 where
    actions f = GetVar._DefTypeChanged %~ actions f

instance Actions (GetVar.VarForm name o0) (GetVar.VarForm name o1) o0 o1 where
    actions f = GetVar._GetDefinition %~ actions f

instance Actions (GetVar.VarInline o0) (GetVar.VarInline o1) o0 o1 where
    actions = Lens.over GetVar._InlineVar

instance Actions (GetVar.GetVar name o0) (GetVar.GetVar name o1) o0 o1 where
    actions f (GetVar.GetVar x0 x1 var x2) = GetVar.GetVar (actions f x0) (actions f x1) var (actions f x2)

instance Actions (Eval.EvalException o0) (Eval.EvalException o1) o0 o1 where
    actions f = Eval.evalExceptionJumpTo . Lens.mapped %~ f

instance Actions (Eval.EvalCompletionResult o0) (Eval.EvalCompletionResult o1) o0 o1 where
    actions f = Eval._EvalError %~ actions f

instance Actions (Eval.EvalCompletion o0) (Eval.EvalCompletion o1) o0 o1 where
    actions f = fmap (fmap (actions f))

instance Functor i => Actions (Parts.AddParam name i o0) (Parts.AddParam name i o1) o0 o1 where
    actions f = Parts._AddNext . Lens.mapped %~ actions f

instance Actions (Parts.NullParamActions o0) (Parts.NullParamActions o1) o0 o1 where
    actions = Lens.over Parts.npDeleteLambda

instance Functor i => Actions (Parts.VarParamInfo name i o0) (Parts.VarParamInfo name i o1) o0 o1 where
    actions f (Parts.VarParamInfo a b c d) =
        Parts.VarParamInfo (actions f a) (actions f b) (actions f c) (f d)

instance Actions (Parts.DetachAction o0) (Parts.DetachAction o1) o0 o1 where
    actions = Lens.over Parts._DetachAction

instance Actions (Parts.Delete o0) (Parts.Delete o1) o0 o1 where
    actions f (Parts.SetToHole x) = Parts.SetToHole (f x)
    actions f (Parts.Delete x) = Parts.Delete (f x)
    actions _ Parts.CannotDelete = Parts.CannotDelete

instance Actions (Parts.NodeActions o0) (Parts.NodeActions o1) o0 o1 where
    actions f (Parts.NodeActions x0 x1 x2 x3 x4 x5) =
        Parts.NodeActions
        (actions f x0) (actions f x1) (fmap f x2) (f x3) (fmap f x4) (fmap f x5)

instance Functor i => Actions (Parts.TaggedItem name i o0 a) (Parts.TaggedItem name i o1 a) o0 o1 where
    actions f (Parts.TaggedItem x0 x1 x2 val) =
        Parts.TaggedItem (actions f x0) (f x1) (fmap (actions f) x2) val

instance Functor i => Actions (Parts.TaggedSwappableItem name i o0 a) (Parts.TaggedSwappableItem name i o1 a) o0 o1 where
    actions f (Parts.TaggedSwappableItem x y) =
        Parts.TaggedSwappableItem (actions f x) (f y)

instance Functor i => Actions (Parts.TaggedListBody name i o0 a) (Parts.TaggedListBody name i o1 a) o0 o1 where
    actions f (Parts.TaggedListBody x y) =
        Parts.TaggedListBody (actions f x) (fmap (actions f) y)

instance Functor i => Actions (Parts.TaggedList name i o0 a) (Parts.TaggedList name i o1 a) o0 o1 where
    actions f (Parts.TaggedList x y) =
        Parts.TaggedList (fmap (actions f) x) (fmap (actions f) y)

instance Functor i => Actions (Parts.Params v name i o0) (Parts.Params v name i o1) o0 o1 where
    actions f (Parts.NullParam x) = x & _2 %~ actions f & Parts.NullParam
    actions f (Parts.VarParam x) = x & _2 %~ actions f & Parts.VarParam
    actions f (Parts.RecordParams x) = actions f x & Parts.RecordParams

instance Actions (Parts.Payload v o0) (Parts.Payload v o1) o0 o1 where
    actions f = Parts.plActions %~ actions f

instance Actions (Parts.ClosedCompositeActions o0) (Parts.ClosedCompositeActions o1) o0 o1 where
    actions = Lens.over Parts.closedCompositeOpen

instance Functor i => HActions (Parts.NullaryInject name i o0) (Parts.NullaryInject name i o1) o0 o1 where
    hActionsBody f (Parts.NullaryInject x0 x1) =
        Parts.NullaryInject
        (recursiveHMap (Lens._Wrapped %~ actions f) x0)
        (recursiveHMap (Lens._Wrapped . Lens.mapped %~ actions f) x1)

instance HActions (Parts.PunnedVar name o0) (Parts.PunnedVar name o1) o0 o1 where
    hActionsBody f = Parts.pvVar %~ recursiveHMap (Lens._Wrapped %~ actions f)

instance ( ann ~ Parts.Annotation () name
         , Recursively HFunctor (t ann name i o1)
         , HActions
             (t ann name i o0)
             (t ann name i o1) o0 o1) =>
         Actions (Parts.Option t name i o0) (Parts.Option t name i o1) o0 o1 where
    actions f (Parts.Option x0 x1 match newTag) =
        Parts.Option
        (x0 & hActions f & hflipped %~ hmap (const (Lens._Wrapped %~ actions f)))
        (f x1) match newTag

instance Functor i => HActions (Expr.AnnotatedArg v name i o0) (Expr.AnnotatedArg v name i o1) o0 o1 where
    hActionsBody f = Expr.aaExpr %~ hActions f

instance (Functor i, t1 ~ Expr.Term v name i o1) => HActions (Expr.Term v name i o0) t1 o0 o1 where
    hActionsBody f (Expr.BodyLam x) = hActionsBody f x & Expr.BodyLam
    hActionsBody f (Expr.BodySimpleApply x) = x & morphMapped1 %~ hActions f & Expr.BodySimpleApply
    hActionsBody f (Expr.BodyPostfixApply x) = x & hActionsBody f & Expr.BodyPostfixApply
    hActionsBody f (Expr.BodyLabeledApply x) = x & hActionsBody f & Expr.BodyLabeledApply
    hActionsBody f (Expr.BodyRecord x) = x & hActionsBody f & Expr.BodyRecord
    hActionsBody f (Expr.BodyIfElse x) = x & hActionsBody f & Expr.BodyIfElse
    hActionsBody f (Expr.BodyToNom x) = x & hActionsBody f & Expr.BodyToNom
    hActionsBody f (Expr.BodyPostfixFunc x) = x & hActionsBody f & Expr.BodyPostfixFunc
    hActionsBody f (Expr.BodyNullaryInject x) = x & hActionsBody f & Expr.BodyNullaryInject
    hActionsBody f (Expr.BodyFragment x) = x & hActionsBody f & Expr.BodyFragment
    hActionsBody f (Expr.BodyLeaf x) = actions f x & Expr.BodyLeaf

instance Functor i => HActions (Expr.Lambda v name i o0) (Expr.Lambda v name i o1) o0 o1 where
    hActionsBody f = Expr.lamFunc %~ hActionsBody f

instance Functor i => HActions (Expr.Function v name i o0) (Expr.Function v name i o1) o0 o1 where
    hActionsBody f (Expr.Function x0 x1 x2 bodyScopes) =
        Expr.Function (x0 & Lens.mapped . Property.pSet . Lens.mapped %~ f) (actions f x1) (hActions f x2) bodyScopes

instance Functor i => HActions (Expr.Binder v name i o0) (Expr.Binder v name i o1) o0 o1 where
    hActionsBody f (Expr.Binder x0 x1) = Expr.Binder (f x0) (hActionsBody f x1)

instance Functor i => HActions (Expr.BinderBody v name i o0) (Expr.BinderBody v name i o1) o0 o1 where
    hActionsBody f (Expr.BinderLet x) = hActionsBody f x & Expr.BinderLet
    hActionsBody f (Expr.BinderTerm x) = hActionsBody f x & Expr.BinderTerm

instance Functor i => HActions (Expr.Let v name i o0) (Expr.Let v name i o1) o0 o1 where
    hActionsBody f (Expr.Let x0 usages x1 x2 x3) =
        Expr.Let (hActions f x0) usages (actions f x1) (f x2) (hActions f x3)

instance Functor i => HActions (Expr.Assignment v name i o0) (Expr.Assignment v name i o1) o0 o1 where
    hActionsBody f (Expr.BodyFunction x) = hActionsBody f x & Expr.BodyFunction
    hActionsBody f (Expr.BodyPlain x) = hActionsBody f x & Expr.BodyPlain

instance Functor i => HActions (Expr.AssignPlain v name i o0) (Expr.AssignPlain v name i o1) o0 o1 where
    hActionsBody f (Expr.AssignPlain x0 x1) = Expr.AssignPlain (f x0) (hActionsBody f x1)

instance Functor i => HActions (Expr.PostfixApply v name i o0) (Expr.PostfixApply v name i o1) o0 o1 where
    hActionsBody f (Expr.PostfixApply x0 x1) = Expr.PostfixApply (hActions f x0) (hActions f x1)

instance Functor i => HActions (Expr.PostfixFunc v name i o0) (Expr.PostfixFunc v name i o1) o0 o1 where
    hActionsBody f (Expr.PfCase x) = hActionsBody f x & Expr.PfCase
    hActionsBody f (Expr.PfFromNom x) = actions f x & Expr.PfFromNom
    hActionsBody f (Expr.PfGetField x) = actions f x & Expr.PfGetField

instance Functor i => HActions (Expr.Composite v name i o0) (Expr.Composite v name i o1) o0 o1 where
    hActionsBody f (Expr.Composite x0 x1 x2) =
        Expr.Composite
        ((fmap (hActions f) x0) & actions f)
        (x1 <&> hActionsBody f)
        (hActionsBody f x2)

instance Functor i => HActions (Expr.CompositeTail v name i o0) (Expr.CompositeTail v name i o1) o0 o1 where
    hActionsBody f (Expr.OpenComposite x) = hActions f x & Expr.OpenComposite
    hActionsBody f (Expr.ClosedComposite x) = actions f x & Expr.ClosedComposite

instance Functor i => HActions (Expr.LabeledApply v name i o0) (Expr.LabeledApply v name i o1) o0 o1 where
    hActionsBody f (Expr.LabeledApply x0 x1 x2 x3) =
        Expr.LabeledApply
        (recursiveHMap (Lens._Wrapped %~ actions f) x0)
        (fmap (hActionsBody f) x1)
        (fmap (hActionsBody f) x2)
        (fmap (hActionsBody f) x3)

instance Functor i => HActions (Expr.OperatorArgs v name i o0) (Expr.OperatorArgs v name i o1) o0 o1 where
    hActionsBody f (Expr.OperatorArgs x0 x1 x2) =
        Expr.OperatorArgs (hActions f x0) (hActions f x1) (f x2)

instance Functor i => HActions (Expr.IfElse v name i o0) (Expr.IfElse v name i o1) o0 o1 where
    hActionsBody f (Expr.IfElse x0 x1 x2) = Expr.IfElse (hActions f x0) (hActions f x1) (hActions f x2)

instance Functor i => HActions (Expr.Else v name i o0) (Expr.Else v name i o1) o0 o1 where
    hActionsBody f (Expr.SimpleElse x) = hActionsBody f x & Expr.SimpleElse
    hActionsBody f (Expr.ElseIf x) = hActionsBody f x & Expr.ElseIf

instance Functor i => HActions (Expr.ElseIfBody v name i o0) (Expr.ElseIfBody v name i o1) o0 o1 where
    hActionsBody f (Expr.ElseIfBody x0 x1) = Expr.ElseIfBody (f x0) (hActionsBody f x1)

instance Functor i => HActions (Expr.Nominal v name i o0) (Expr.Nominal v name i o1) o0 o1 where
    hActionsBody f (Expr.Nominal x0 x1) = Expr.Nominal (actions f x0) (hActions f x1)

instance Functor i => HActions (Expr.Fragment v name i o0) (Expr.Fragment v name i o1) o0 o1 where
    hActionsBody f (Expr.Fragment x0 x1 mismatch x2 x3 suffixes) =
        Expr.Fragment (hActions f x0) (f x1) mismatch ((fmap . fmap . fmap . fmap) (actions f) x2) (fmap (actions f) x3) suffixes

instance Functor i => HActions (Expr.FragOpt v name i o0) (Expr.FragOpt v name i o1) o0 o1 where
    hActionsBody f (Expr.FragPostfix x) = fmap (hActions f) x & Expr.FragPostfix
    hActionsBody f (Expr.FragInject x) = actions f x & Expr.FragInject -- (TagRef name i o)
    hActionsBody f (Expr.FragWrapInRec x) = actions f x & Expr.FragWrapInRec
    hActionsBody f (Expr.FragApplyFunc x) = actions f x & Expr.FragApplyFunc
    hActionsBody f (Expr.FragOp x) = hActionsBody f x & Expr.FragOp -- (FragOperator v name i o k)
    hActionsBody f (Expr.FragToNom x) = actions f x & Expr.FragToNom
    hActionsBody _ Expr.FragLam = Expr.FragLam
    hActionsBody _ Expr.FragDefer = Expr.FragDefer
    hActionsBody f (Expr.FragIf x) = hActions f x & Expr.FragIf
    hActionsBody f (Expr.FragArgument x) = hActionsBody f x & Expr.FragArgument -- (HoleOpt v name i o k) -- Apply fragmented expr with argument

instance Functor i => HActions (Expr.FragOperator v name i o0) (Expr.FragOperator v name i o1) o0 o1 where
    hActionsBody f (Expr.FragOperator x0 x1 args) =
        Expr.FragOperator (recursiveHMap (Lens._Wrapped %~ actions f) x0) (hActions f x1) args

instance Functor i => HActions (Expr.HoleOpt v name i o0) (Expr.HoleOpt v name i o1) o0 o1 where
    hActionsBody f = Expr._HoleBinder %~ hActionsBody f

instance Functor i => Actions (Expr.Leaf name i o0) (Expr.Leaf name i o1) o0 o1 where
    actions f (Expr.LeafLiteral x) = literal f x & Expr.LeafLiteral
    actions f (Expr.LeafHole x) = actions f x & Expr.LeafHole
    actions f (Expr.LeafGetVar x) = actions f x & Expr.LeafGetVar
    actions f (Expr.LeafInject x) = actions f x & Expr.LeafInject

literal :: forall o0 o1. (o0 () -> o1 ()) -> Parts.Literal (Property o0) -> Parts.Literal (Property o1)
literal f =
    \case
    Parts.LiteralNum x -> editProperty x & Parts.LiteralNum
    Parts.LiteralBytes x -> editProperty x & Parts.LiteralBytes
    Parts.LiteralChar x -> editProperty x & Parts.LiteralChar
    Parts.LiteralText x -> editProperty x & Parts.LiteralText

    where
        editProperty :: Property o0 a -> Property o1 a
        editProperty = Property.pSet . Lens.mapped %~ f

instance Functor i => Actions (Expr.Hole name i o0) (Expr.Hole name i o1) o0 o1 where
    actions f = Expr.holeOptions . Lens.mapped . Lens.mapped . Lens.mapped . Lens.mapped %~ actions f
