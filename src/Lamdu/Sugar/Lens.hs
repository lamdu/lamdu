{-# LANGUAGE FlexibleContexts, RankNTypes, TemplateHaskell, ScopedTypeVariables #-}
module Lamdu.Sugar.Lens
    ( PayloadOf(..), _OfExpr, _OfLabeledApplyFunc, _OfNullaryVal
    , bodyChildren, overBodyChildren, bodyChildPayloads
    , binderChildren, overBinderChildren
    , labeledApplyChildren, overLabeledApplyChildren
    , ifElseChildren, overIfElseChildren
    , letChildren, overLetChildren
    , exprPayloads, binderPayloads
    , payloadsOf
    , bodyUnfinished, unfinishedExprPayloads
    , defSchemes
    , assignmentBodyAddFirstParam
    , binderFuncParamActions
    , binderResultExpr
    , holeTransformExprs, holeOptionTransformExprs
    , annotationTypes
    , assignmentSubExprParams, assignmentPayloads
    , binderSubExprParams
    , paramsAnnotations
    ) where

import qualified Control.Lens as Lens
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

data PayloadOf name i o
    = OfExpr (Body name i o ())
    | OfElseIf (ElseIfContent name i o ())
    | OfLet (Let name i o ())
    | OfAssignFunction (Function name i o ())
    | OfLabeledApplyFunc (BinderVarRef name o)
    | OfRelayedArg (GetVar name o)
    | OfNullaryVal (NullaryVal name i o)
Lens.makePrisms ''PayloadOf

letChildren ::
    Applicative f =>
    (ParentNode (Binder name i o) a -> f (ParentNode (Binder name i o) b)) ->
    (Assignment name i o a -> f (Assignment name i o b)) ->
    Let name i o a -> f (Let name i o b)
letChildren b a x =
    (\v bod -> x{_lValue=v, _lBody=bod})
    <$> a (x ^. lValue)
    <*> b (x ^. lBody)

overLetChildren ::
    (ParentNode (Binder name i o) a -> ParentNode (Binder name i o) b) ->
    (Assignment name i o a -> Assignment name i o b) ->
    Let name i o a -> Let name i o b
overLetChildren b a = Lens.runIdentity . letChildren (pure . b) (pure . a)

binderChildren ::
    Applicative f =>
    (Node (NullaryVal name i o) a -> f (Node (NullaryVal name i o) b)) ->
    (Node (BinderVarRef name o) a -> f (Node (BinderVarRef name o) b)) ->
    (Node (GetVar name o) a -> f (Node (GetVar name o) b)) ->
    (ParentNode (Else name i o) a -> f (ParentNode (Else name i o) b)) ->
    (ParentNode (Binder name i o) a -> f (ParentNode (Binder name i o) b)) ->
    (Expression name i o a -> f (Expression name i o b)) ->
    (Assignment name i o a -> f (Assignment name i o b)) ->
    Binder name i o a -> f (Binder name i o b)
binderChildren n l r e b f _a (BinderExpr x) =
    bodyChildren n l r e b f x <&> BinderExpr
binderChildren _n _l _r _e b _f a (BinderLet x) =
    letChildren b a x <&> BinderLet

overBinderChildren ::
    (Node (NullaryVal name i o) a -> Node (NullaryVal name i o) b) ->
    (Node (BinderVarRef name o) a -> Node (BinderVarRef name o) b) ->
    (Node (GetVar name o) a -> Node (GetVar name o) b) ->
    (ParentNode (Else name i o) a -> ParentNode (Else name i o) b) ->
    (ParentNode (Binder name i o) a -> ParentNode (Binder name i o) b) ->
    (Expression name i o a -> Expression name i o b) ->
    (Assignment name i o a -> Assignment name i o b) ->
    Binder name i o a -> Binder name i o b
overBinderChildren n v r e b f a =
    Lens.runIdentity .
    binderChildren
    (pure . n) (pure . v) (pure . r) (pure . e) (pure . b) (pure . f) (pure . a)

labeledApplyChildren ::
    Applicative f =>
    (Node (BinderVarRef name o) a -> f (Node (BinderVarRef name o) b)) ->
    (Node (GetVar name o) a -> f (Node (GetVar name o) b)) ->
    (Expression name i o a -> f (Expression name i o b)) ->
    LabeledApply name i o a -> f (LabeledApply name i o b)
labeledApplyChildren l r e (LabeledApply func special annotated relayed) =
    uncurry LabeledApply
    <$> funcAndSpecial
    <*> (traverse . traverse) e annotated
    <*> traverse r relayed
    where
        funcAndSpecial =
            case special of
            Infix left right ->
                -- Correct order in infix is operator in the middle.
                (\l0 f0 r0 -> (f0, Infix l0 r0))
                <$> e left
                <*> l func
                <*> e right
            _ ->
                (,)
                <$> l func
                <*> traverse e special

overLabeledApplyChildren ::
    (Node (BinderVarRef name o) a -> Node (BinderVarRef name o) b) ->
    (Node (GetVar name o) a -> Node (GetVar name o) b) ->
    (Expression name i o a -> Expression name i o b) ->
    LabeledApply name i o a -> LabeledApply name i o b
overLabeledApplyChildren l r e =
    Lens.runIdentity . labeledApplyChildren (pure . l) (pure . r) (pure . e)

ifElseChildren ::
    Applicative f =>
    (ParentNode (Else name i o) a -> f (ParentNode (Else name i o) b)) ->
    (Expression name i o a -> f (Expression name i o b)) ->
    IfElse name i o a -> f (IfElse name i o b)
ifElseChildren onElse onExpr (IfElse if_ then_ else_) =
    IfElse <$> onExpr if_ <*> onExpr then_ <*> onElse else_

overIfElseChildren ::
    (ParentNode (Else name i o) a -> ParentNode (Else name i o) b) ->
    (Expression name i o a -> Expression name i o b) ->
    IfElse name i o a -> IfElse name i o b
overIfElseChildren onElse onExpr =
    Lens.runIdentity . ifElseChildren (pure . onElse) (pure . onExpr)

injectContentChildren ::
    Applicative f =>
    (Node (NullaryVal name i o) a -> f (Node (NullaryVal name i o) b)) ->
    (Expression name i o a -> f (Expression name i o b)) ->
    InjectContent name i o a -> f (InjectContent name i o b)
injectContentChildren _ e (InjectVal x) = e x <&> InjectVal
injectContentChildren n _ (InjectNullary x) = n x <&> InjectNullary

bodyChildren ::
    Applicative f =>
    (Node (NullaryVal name i o) a -> f (Node (NullaryVal name i o) b)) ->
    (Node (BinderVarRef name o) a -> f (Node (BinderVarRef name o) b)) ->
    (Node (GetVar name o) a -> f (Node (GetVar name o) b)) ->
    (ParentNode (Else name i o) a -> f (ParentNode (Else name i o) b)) ->
    (ParentNode (Binder name i o) a -> f (ParentNode (Binder name i o) b)) ->
    (Expression name i o a -> f (Expression name i o b)) ->
    Body name i o a -> f (Body name i o b)
bodyChildren n l r e b f =
    \case
    BodyPlaceHolder -> pure BodyPlaceHolder
    BodyLiteral x -> BodyLiteral x & pure
    BodyGetVar  x -> BodyGetVar  x & pure
    BodyHole    x -> BodyHole    x & pure
    BodyLam          x -> (lamFunc . fBody) b x <&> BodyLam
    BodySimpleApply  x -> traverse f x <&> BodySimpleApply
    BodyLabeledApply x -> labeledApplyChildren l r f x <&> BodyLabeledApply
    BodyRecord       x -> traverse f x <&> BodyRecord
    BodyGetField     x -> traverse f x <&> BodyGetField
    BodyCase         x -> traverse f x <&> BodyCase
    BodyIfElse       x -> ifElseChildren e f x <&> BodyIfElse
    BodyInject       x -> iContent (injectContentChildren n f) x <&> BodyInject
    BodyFromNom      x -> traverse f x <&> BodyFromNom
    BodyFragment     x -> fExpr f x <&> BodyFragment
    BodyToNom        x -> traverse b x <&> BodyToNom

parentNodePayload ::
    (f a -> p) ->
    Lens.IndexedLens' p (ParentNode f a) a
parentNodePayload c f (PNode (Node pl x)) =
    Lens.indexed f (c x) pl <&> (`Node` x) <&> PNode

binderIndex :: Binder name i o a -> PayloadOf name i o
binderIndex (BinderLet x) = void x & OfLet
binderIndex (BinderExpr x) = void x & OfExpr

elseIndex :: Else name i o a -> PayloadOf name i o
elseIndex (ElseIf x) = void x & OfElseIf
elseIndex (SimpleElse x) = void x & OfExpr

assignmentBodyIndex :: AssignmentBody name i o a -> PayloadOf name i o
assignmentBodyIndex (BodyFunction x) = void x & OfAssignFunction
assignmentBodyIndex (BodyPlain x) = binderIndex (x ^. apBody)

bodyChildPayloads ::
    forall name i o a.
    Lens.IndexedTraversal' (PayloadOf name i o) (Body name i o a) a
bodyChildPayloads f =
    bodyChildren
    (leafNodePayload OfNullaryVal f)
    (Lens.cloneIndexedLens labeledFuncPayloads f)
    (Lens.cloneIndexedLens relayedPayloads f)
    (parentNodePayload elseIndex f)
    (parentNodePayload binderIndex f)
    (parentNodePayload (OfExpr . void) f)
    where
        labeledFuncPayloads ::
            Lens.AnIndexedLens' (PayloadOf name i o) (Node (BinderVarRef name o) a) a
        labeledFuncPayloads = leafNodePayload OfLabeledApplyFunc
        relayedPayloads ::
            Lens.AnIndexedLens' (PayloadOf name i o) (Node (GetVar name o) a) a
        relayedPayloads = leafNodePayload OfRelayedArg

overBodyChildren ::
    (Node (NullaryVal name i o) a -> Node (NullaryVal name i o) b) ->
    (Node (BinderVarRef name o) a -> Node (BinderVarRef name o) b) ->
    (Node (GetVar name o) a -> Node (GetVar name o) b) ->
    (ParentNode (Else name i o) a -> ParentNode (Else name i o) b) ->
    (ParentNode (Binder name i o) a -> ParentNode (Binder name i o) b) ->
    (Expression name i o a -> Expression name i o b) ->
    Body name i o a -> Body name i o b
overBodyChildren n v r e b f =
    Lens.runIdentity .
    bodyChildren (pure . n) (pure . v) (pure . r) (pure . e) (pure . b) (pure . f)

leafNodePayload ::
    (l -> p) ->
    Lens.IndexedLens p (Node l a) (Node l b) a b
leafNodePayload c f (Node pl x) =
    Lens.indexed f (c x) pl <&> (`Node` x)

parentNodePayloads ::
    Lens.AnIndexedTraversal i (f a) (f b) a b ->
    (f a -> i) ->
    Lens.IndexedTraversal i
    (ParentNode f a)
    (ParentNode f b)
    a b
parentNodePayloads b i f (PNode (Node pl x)) =
    flip Node
    <$> Lens.cloneIndexedTraversal b f x
    <*> Lens.indexed f (i x) pl
    <&> PNode

binderPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (ParentNode (Binder name i o) a)
    (ParentNode (Binder name i o) b)
    a b
binderPayloads = parentNodePayloads binderBodyPayloads binderIndex

assignmentBodyPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (AssignmentBody name i o a)
    (AssignmentBody name i o b)
    a b
assignmentBodyPayloads f (BodyFunction x) =
    (fBody . binderPayloads) f x <&> BodyFunction
assignmentBodyPayloads f (BodyPlain x) =
    (apBody . binderBodyPayloads) f x <&> BodyPlain

assignmentPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (ParentNode (AssignmentBody name i o) a)
    (ParentNode (AssignmentBody name i o) b)
    a b
assignmentPayloads = parentNodePayloads assignmentBodyPayloads assignmentBodyIndex

letPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (Let name i o a)
    (Let name i o b)
    a b
letPayloads f =
    letChildren (binderPayloads f) (assignmentPayloads f)

binderBodyPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (Binder name i o a)
    (Binder name i o b)
    a b
binderBodyPayloads f (BinderExpr x) = bodyPayloads f x <&> BinderExpr
binderBodyPayloads f (BinderLet x) = letPayloads f x <&> BinderLet

elseIfContentPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (ElseIfContent name i o a)
    (ElseIfContent name i o b)
    a b
elseIfContentPayloads f =
    eiContent (ifElseChildren (elsePayloads f) (exprPayloads f))

elseBodyPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (Else name i o a)
    (Else name i o b)
    a b
elseBodyPayloads f (SimpleElse x) = bodyPayloads f x <&> SimpleElse
elseBodyPayloads f (ElseIf x) = elseIfContentPayloads f x <&> ElseIf

exprPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (Expression name i o a)
    (Expression name i o b)
    a b
exprPayloads = parentNodePayloads bodyPayloads (OfExpr . void)

elsePayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (ParentNode (Else name i o) a)
    (ParentNode (Else name i o) b)
    a b
elsePayloads = parentNodePayloads elseBodyPayloads elseIndex

bodyPayloads ::
    forall name i o a b.
    Lens.IndexedTraversal (PayloadOf name i o)
    (Body name i o a)
    (Body name i o b)
    a b
bodyPayloads f =
    bodyChildren
    (leafNodePayload OfNullaryVal f)
    (Lens.cloneIndexedLens labeledFuncPayloads f)
    (Lens.cloneIndexedLens relayedPayloads f)
    (elsePayloads f)
    (binderPayloads f)
    (exprPayloads f)
    where
        labeledFuncPayloads ::
            Lens.AnIndexedLens (PayloadOf name i o)
            (Node (BinderVarRef name o) a)
            (Node (BinderVarRef name o) b)
            a b
        labeledFuncPayloads = leafNodePayload OfLabeledApplyFunc
        relayedPayloads ::
            Lens.AnIndexedLens (PayloadOf name i o)
            (Node (GetVar name o) a)
            (Node (GetVar name o) b)
            a b
        relayedPayloads = leafNodePayload OfRelayedArg

payloadsOf ::
    Lens.Fold (Body name i o ()) a ->
    Lens.IndexedTraversal' (PayloadOf name i o) (Expression name i o b) b
payloadsOf x =
    exprPayloads . Lens.ifiltered predicate
    where
        predicate idx _ = Lens.has (_OfExpr . x) idx

binderVarRefUnfinished :: Lens.Traversal' (BinderVarRef name m) ()
binderVarRefUnfinished =
    bvForm . _GetDefinition . Lens.failing _DefDeleted (_DefTypeChanged . Lens.united)

bodyUnfinished :: Lens.Traversal' (Body name i o a) ()
bodyUnfinished =
    _BodyHole . Lens.united
    & Lens.failing (_BodyFragment . Lens.united)
    & Lens.failing (_BodyGetVar . _GetBinder . binderVarRefUnfinished)
    & Lens.failing (_BodyLabeledApply . aFunc . val . binderVarRefUnfinished)

unfinishedExprPayloads ::
    Lens.IndexedTraversal' (PayloadOf name i o) (Expression name i o a) a
unfinishedExprPayloads = payloadsOf bodyUnfinished

defBodySchemes :: Lens.Traversal' (DefinitionBody name i o expr) (Scheme name)
defBodySchemes f (DefinitionBodyBuiltin b) =
    b & biType %%~ f
    <&> DefinitionBodyBuiltin
defBodySchemes f (DefinitionBodyExpression de) =
    de & deType %%~ f
    <&> DefinitionBodyExpression

defSchemes :: Lens.Traversal' (Definition name i o expr) (Scheme name)
defSchemes = drBody . defBodySchemes

binderFuncParamActions ::
    Lens.Traversal' (BinderParams name i o) (FuncParamActions name i o)
binderFuncParamActions _ (NullParam a) = pure (NullParam a)
binderFuncParamActions f (Params ps) = (traverse . fpInfo . piActions) f ps <&> Params

binderResultExpr :: Lens.IndexedLens' (PayloadOf name i o) (ParentNode (Binder name i o) a) a
binderResultExpr f (PNode (Node pl x)) =
    case x of
    BinderExpr e -> Lens.indexed f (OfExpr (void e)) pl <&> (`Node` x) <&> PNode
    BinderLet l ->
        lBody (binderResultExpr f) l
        <&> BinderLet
        <&> Node pl <&> PNode

holeOptionTransformExprs ::
    Monad i =>
    (ParentNode (Binder n0 i o) (Payload n0 i o ()) ->
     i (ParentNode (Binder n1 i o) (Payload n1 i o ()))) ->
    HoleOption n0 i o -> HoleOption n1 i o
holeOptionTransformExprs onExpr option =
    option
    { _hoSugaredBaseExpr = option ^. hoSugaredBaseExpr >>= onExpr
    , _hoResults = option ^. hoResults <&> Lens._2 %~ (>>= holeResultConverted onExpr)
    }

holeTransformExprs ::
    Monad i =>
    (ParentNode (Binder n0 i o) (Payload n0 i o ()) ->
        i (ParentNode (Binder n1 i o) (Payload n1 i o ()))) ->
    Hole n0 i o -> Hole n1 i o
holeTransformExprs onExpr hole =
    hole
    { _holeOptions = hole ^. holeOptions <&> traverse %~ holeOptionTransformExprs onExpr
    , _holeOptionLiteral =
        hole ^. holeOptionLiteral <&> Lens.mapped . Lens._2 %~ (>>= holeResultConverted onExpr)
    }

assignmentBodyAddFirstParam :: Lens' (AssignmentBody name i o a) (AddFirstParam name i o)
assignmentBodyAddFirstParam f (BodyFunction x) = fAddFirstParam f x <&> BodyFunction
assignmentBodyAddFirstParam f (BodyPlain x) = apAddFirstParam f x <&> BodyPlain

annotationTypes :: Lens.Traversal' (Annotation name i) (Type name)
annotationTypes _ AnnotationNone = pure AnnotationNone
annotationTypes f (AnnotationType x) = f x <&> AnnotationType
annotationTypes f (AnnotationVal x) = (annotationType . Lens._Just) f x <&> AnnotationVal

-- TODO: rename paramsAnnotations
paramsAnnotations :: Lens.Traversal' (BinderParams name i o) (Annotation name i)
paramsAnnotations f (NullParam x) = fpAnnotation f x <&> NullParam
paramsAnnotations f (Params xs) = (traverse . fpAnnotation) f xs <&> Params

funcSubExprParams :: Lens.Traversal' (Function name i o a) (BinderParams name i o)
funcSubExprParams f x =
    (\p b -> x{_fParams = p, _fBody = b})
    <$> f (x ^. fParams)
    <*> binderSubExprParams f (x ^. fBody)

elseSubExprParams :: Lens.Traversal' (Else name i o a) (BinderParams name i o)
elseSubExprParams f (SimpleElse x) = bodySubExprParams f x <&> SimpleElse
elseSubExprParams f (ElseIf x) =
    eiContent
    (ifElseChildren
        ((_PNode . val . elseSubExprParams) f)
        ((_PNode . val . bodySubExprParams) f))
    x <&> ElseIf

bodySubExprParams :: Lens.Traversal' (Body name i o a) (BinderParams name i o)
bodySubExprParams f (BodyLam x) = (lamFunc . funcSubExprParams) f x <&> BodyLam
bodySubExprParams f x =
    bodyChildren pure pure pure
    ((_PNode . val . elseSubExprParams) f)
    (binderSubExprParams f)
    ((_PNode . val . bodySubExprParams) f) x

binderBodySubExprParams :: Lens.Traversal' (Binder name i o a) (BinderParams name i o)
binderBodySubExprParams f (BinderExpr x) =
    bodySubExprParams f x <&> BinderExpr
binderBodySubExprParams f (BinderLet x) =
    (\v b -> BinderLet x{_lValue = v, _lBody = b})
    <$> assignmentSubExprParams f (x ^. lValue)
    <*> binderSubExprParams f (x ^. lBody)

binderSubExprParams :: Lens.Traversal' (ParentNode (Binder name i o) a) (BinderParams name i o)
binderSubExprParams = _PNode . val . binderBodySubExprParams

assignmentBodySubExprParams ::
    Lens.Traversal' (AssignmentBody name i o a) (BinderParams name i o)
assignmentBodySubExprParams f (BodyPlain x) = (apBody . binderBodySubExprParams) f x <&> BodyPlain
assignmentBodySubExprParams f (BodyFunction x) = funcSubExprParams f x <&> BodyFunction

assignmentSubExprParams :: Lens.Traversal' (Assignment name i o a) (BinderParams name i o)
assignmentSubExprParams = _PNode . val . assignmentBodySubExprParams
