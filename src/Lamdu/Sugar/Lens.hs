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
    , stripAnnotations
    ) where

import qualified Control.Lens as Lens
import           Data.Tree.Diverse (Node, Ann(..), Children(..), ann, val, hoist)
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

data PayloadOf name i o
    = OfExpr (Body name i o (Ann ()))
    | OfElseIf (ElseIfContent name i o (Ann ()))
    | OfLet (Let name i o (Ann ()))
    | OfAssignFunction (Function name i o (Ann ()))
    | OfLabeledApplyFunc (BinderVarRef name o)
    | OfRelayedArg (GetVar name o)
    | OfNullaryVal (NullaryVal name i o)
Lens.makePrisms ''PayloadOf

letChildren ::
    Applicative f =>
    (Node n (Binder name i o) -> f (Node m (Binder name i o))) ->
    (Node n (AssignmentBody name i o) -> f (Node m (AssignmentBody name i o))) ->
    Let name i o n -> f (Let name i o m)
letChildren b a x =
    (\v bod -> x{_lValue=v, _lBody=bod})
    <$> a (x ^. lValue)
    <*> b (x ^. lBody)

overLetChildren ::
    (Node f (Binder name i o) -> Node g (Binder name i o)) ->
    (Node f (AssignmentBody name i o) -> Node g (AssignmentBody name i o)) ->
    Let name i o f -> Let name i o g
overLetChildren b a = Lens.runIdentity . letChildren (pure . b) (pure . a)

binderChildren ::
    Applicative f =>
    (n (NullaryVal name i o) -> f (m (NullaryVal name i o))) ->
    (n (BinderVarRef name o) -> f (m (BinderVarRef name o))) ->
    (n (GetVar name o) -> f (m (GetVar name o))) ->
    (Node n (Else name i o) -> f (Node m (Else name i o))) ->
    (Node n (Binder name i o) -> f (Node m (Binder name i o))) ->
    (Node n (Body name i o) -> f (Node m (Body name i o))) ->
    (Node n (AssignmentBody name i o) -> f (Node m (AssignmentBody name i o))) ->
    Binder name i o n -> f (Binder name i o m)
binderChildren n l r e b f _a (BinderExpr x) =
    bodyChildren n l r e b f x <&> BinderExpr
binderChildren _n _l _r _e b _f a (BinderLet x) =
    letChildren b a x <&> BinderLet

overBinderChildren ::
    (f (NullaryVal name i o) -> g (NullaryVal name i o)) ->
    (f (BinderVarRef name o) -> g (BinderVarRef name o)) ->
    (f (GetVar name o) -> g (GetVar name o)) ->
    (Node f (Else name i o) -> Node g (Else name i o)) ->
    (Node f (Binder name i o) -> Node g (Binder name i o)) ->
    (Node f (Body name i o) -> Node g (Body name i o)) ->
    (Node f (AssignmentBody name i o) -> Node g (AssignmentBody name i o)) ->
    Binder name i o f -> Binder name i o g
overBinderChildren n v r e b f a =
    Lens.runIdentity .
    binderChildren
    (pure . n) (pure . v) (pure . r) (pure . e) (pure . b) (pure . f) (pure . a)

labeledApplyChildren ::
    Applicative f =>
    (n (BinderVarRef name o) -> f (m (BinderVarRef name o))) ->
    (n (GetVar name o) -> f (m (GetVar name o))) ->
    (Node n (Body name i o) -> f (Node m (Body name i o))) ->
    LabeledApply name i o n -> f (LabeledApply name i o m)
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
    (f (BinderVarRef name o) -> g (BinderVarRef name o)) ->
    (f (GetVar name o) -> g (GetVar name o)) ->
    (Node f (Body name i o) -> Node g (Body name i o)) ->
    LabeledApply name i o f -> LabeledApply name i o g
overLabeledApplyChildren l r e =
    Lens.runIdentity . labeledApplyChildren (pure . l) (pure . r) (pure . e)

ifElseChildren ::
    Applicative f =>
    (Node n (Else name i o) -> f (Node m (Else name i o))) ->
    (Node n (Body name i o) -> f (Node m (Body name i o))) ->
    IfElse name i o n -> f (IfElse name i o m)
ifElseChildren onElse onExpr (IfElse if_ then_ else_) =
    IfElse <$> onExpr if_ <*> onExpr then_ <*> onElse else_

overIfElseChildren ::
    (Node f (Else name i o) -> Node g (Else name i o)) ->
    (Node f (Body name i o) -> Node g (Body name i o)) ->
    IfElse name i o f -> IfElse name i o g
overIfElseChildren onElse onExpr =
    Lens.runIdentity . ifElseChildren (pure . onElse) (pure . onExpr)

injectContentChildren ::
    Applicative f =>
    (n (NullaryVal name i o) -> f (m (NullaryVal name i o))) ->
    (Node n (Body name i o) -> f (Node m (Body name i o))) ->
    InjectContent name i o n -> f (InjectContent name i o m)
injectContentChildren _ e (InjectVal x) = e x <&> InjectVal
injectContentChildren n _ (InjectNullary x) = n x <&> InjectNullary

bodyChildren ::
    Applicative f =>
    (n (NullaryVal name i o) -> f (m (NullaryVal name i o))) ->
    (n (BinderVarRef name o) -> f (m (BinderVarRef name o))) ->
    (n (GetVar name o) -> f (m (GetVar name o))) ->
    (Node n (Else name i o) -> f (Node m (Else name i o))) ->
    (Node n (Binder name i o) -> f (Node m (Binder name i o))) ->
    (Node n (Body name i o) -> f (Node m (Body name i o))) ->
    Body name i o n -> f (Body name i o m)
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
    (expr (Ann a) -> p) ->
    Lens.IndexedLens' p (Node (Ann a) expr) a
parentNodePayload c f (Ann pl x) =
    Lens.indexed f (c x) pl <&> (`Ann` x)

stripAnnotations :: Children expr => expr (Ann a) -> expr (Ann ())
stripAnnotations = hoist (ann .~ ())

binderIndex :: Binder name i o (Ann a) -> PayloadOf name i o
binderIndex (BinderLet x) = stripAnnotations x & OfLet
binderIndex (BinderExpr x) = stripAnnotations x & OfExpr

elseIndex :: Else name i o (Ann a) -> PayloadOf name i o
elseIndex (ElseIf x) = x & eiContent %~ stripAnnotations & OfElseIf
elseIndex (SimpleElse x) = stripAnnotations x & OfExpr

assignmentBodyIndex :: AssignmentBody name i o (Ann a) -> PayloadOf name i o
assignmentBodyIndex (BodyFunction x) = stripAnnotations x & OfAssignFunction
assignmentBodyIndex (BodyPlain x) = binderIndex (x ^. apBody)

bodyChildPayloads ::
    forall name i o a.
    Lens.IndexedTraversal' (PayloadOf name i o) (Body name i o (Ann a)) a
bodyChildPayloads f =
    bodyChildren
    (leafNodePayload OfNullaryVal f)
    (Lens.cloneIndexedLens labeledFuncPayloads f)
    (Lens.cloneIndexedLens relayedPayloads f)
    (parentNodePayload elseIndex f)
    (parentNodePayload binderIndex f)
    (parentNodePayload (OfExpr . stripAnnotations) f)
    where
        labeledFuncPayloads ::
            Lens.AnIndexedLens' (PayloadOf name i o) (Ann a (BinderVarRef name o)) a
        labeledFuncPayloads = leafNodePayload OfLabeledApplyFunc
        relayedPayloads ::
            Lens.AnIndexedLens' (PayloadOf name i o) (Ann a (GetVar name o)) a
        relayedPayloads = leafNodePayload OfRelayedArg

overBodyChildren ::
    (f (NullaryVal name i o) -> g (NullaryVal name i o)) ->
    (f (BinderVarRef name o) -> g (BinderVarRef name o)) ->
    (f (GetVar name o) -> g (GetVar name o)) ->
    (Node f (Else name i o) -> Node g (Else name i o)) ->
    (Node f (Binder name i o) -> Node g (Binder name i o)) ->
    (Node f (Body name i o) -> Node g (Body name i o)) ->
    Body name i o f -> Body name i o g
overBodyChildren n v r e b f =
    Lens.runIdentity .
    bodyChildren (pure . n) (pure . v) (pure . r) (pure . e) (pure . b) (pure . f)

leafNodePayload ::
    (l -> p) ->
    Lens.IndexedLens p (Ann a l) (Ann b l) a b
leafNodePayload c f (Ann pl x) =
    Lens.indexed f (c x) pl <&> (`Ann` x)

parentNodePayloads ::
    Lens.AnIndexedTraversal i (e (Ann a)) (e (Ann b)) a b ->
    (e (Ann a) -> i) ->
    Lens.IndexedTraversal i
    (Node (Ann a) e)
    (Node (Ann b) e)
    a b
parentNodePayloads b i f (Ann pl x) =
    flip Ann
    <$> Lens.cloneIndexedTraversal b f x
    <*> Lens.indexed f (i x) pl

binderPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (Node (Ann a) (Binder name i o))
    (Node (Ann b) (Binder name i o))
    a b
binderPayloads = parentNodePayloads binderBodyPayloads binderIndex

assignmentBodyPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (AssignmentBody name i o (Ann a))
    (AssignmentBody name i o (Ann b))
    a b
assignmentBodyPayloads f (BodyFunction x) =
    (fBody . binderPayloads) f x <&> BodyFunction
assignmentBodyPayloads f (BodyPlain x) =
    (apBody . binderBodyPayloads) f x <&> BodyPlain

assignmentPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (Assignment name i o a)
    (Assignment name i o b)
    a b
assignmentPayloads = parentNodePayloads assignmentBodyPayloads assignmentBodyIndex

letPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (Let name i o (Ann a))
    (Let name i o (Ann b))
    a b
letPayloads f =
    letChildren (binderPayloads f) (assignmentPayloads f)

binderBodyPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (Binder name i o (Ann a))
    (Binder name i o (Ann b))
    a b
binderBodyPayloads f (BinderExpr x) = bodyPayloads f x <&> BinderExpr
binderBodyPayloads f (BinderLet x) = letPayloads f x <&> BinderLet

elseIfContentPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (ElseIfContent name i o (Ann a))
    (ElseIfContent name i o (Ann b))
    a b
elseIfContentPayloads f =
    eiContent (ifElseChildren (elsePayloads f) (exprPayloads f))

elseBodyPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (Else name i o (Ann a))
    (Else name i o (Ann b))
    a b
elseBodyPayloads f (SimpleElse x) = bodyPayloads f x <&> SimpleElse
elseBodyPayloads f (ElseIf x) = elseIfContentPayloads f x <&> ElseIf

exprPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (Expression name i o a)
    (Expression name i o b)
    a b
exprPayloads = parentNodePayloads bodyPayloads (OfExpr . stripAnnotations)

elsePayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (Node (Ann a) (Else name i o))
    (Node (Ann b) (Else name i o))
    a b
elsePayloads = parentNodePayloads elseBodyPayloads elseIndex

bodyPayloads ::
    forall name i o a b.
    Lens.IndexedTraversal (PayloadOf name i o)
    (Body name i o (Ann a))
    (Body name i o (Ann b))
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
            (Ann a (BinderVarRef name o))
            (Ann b (BinderVarRef name o))
            a b
        labeledFuncPayloads = leafNodePayload OfLabeledApplyFunc
        relayedPayloads ::
            Lens.AnIndexedLens (PayloadOf name i o)
            (Ann a (GetVar name o))
            (Ann b (GetVar name o))
            a b
        relayedPayloads = leafNodePayload OfRelayedArg

payloadsOf ::
    Lens.Fold (Body name i o (Ann ())) a ->
    Lens.IndexedTraversal' (PayloadOf name i o) (Expression name i o b) b
payloadsOf x =
    exprPayloads . Lens.ifiltered predicate
    where
        predicate idx _ = Lens.has (_OfExpr . x) idx

binderVarRefUnfinished :: Lens.Traversal' (BinderVarRef name m) ()
binderVarRefUnfinished =
    bvForm . _GetDefinition . Lens.failing _DefDeleted (_DefTypeChanged . Lens.united)

bodyUnfinished :: Lens.Traversal' (Body name i o (Ann a)) ()
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

binderResultExpr :: Lens.IndexedLens' (PayloadOf name i o) (Node (Ann a) (Binder name i o)) a
binderResultExpr f (Ann pl x) =
    case x of
    BinderExpr e ->
        Lens.indexed f (OfExpr (stripAnnotations e)) pl
        <&> (`Ann` x)
    BinderLet l ->
        lBody (binderResultExpr f) l
        <&> BinderLet
        <&> Ann pl

holeOptionTransformExprs ::
    Monad i =>
    (Node (Ann (Payload n0 i o ())) (Binder n0 i o) ->
        i (Node (Ann (Payload n1 i o ())) (Binder n1 i o))) ->
    HoleOption n0 i o -> HoleOption n1 i o
holeOptionTransformExprs onExpr option =
    option
    { _hoSugaredBaseExpr = option ^. hoSugaredBaseExpr >>= onExpr
    , _hoResults = option ^. hoResults <&> Lens._2 %~ (>>= holeResultConverted onExpr)
    }

holeTransformExprs ::
    Monad i =>
    (Node (Ann (Payload n0 i o ())) (Binder n0 i o) ->
        i (Node (Ann (Payload n1 i o ())) (Binder n1 i o))) ->
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

funcSubExprParams :: Lens.Traversal' (Function name i o (Ann a)) (BinderParams name i o)
funcSubExprParams f x =
    (\p b -> x{_fParams = p, _fBody = b})
    <$> f (x ^. fParams)
    <*> binderSubExprParams f (x ^. fBody)

elseSubExprParams :: Lens.Traversal' (Else name i o (Ann a)) (BinderParams name i o)
elseSubExprParams f (SimpleElse x) = bodySubExprParams f x <&> SimpleElse
elseSubExprParams f (ElseIf x) =
    eiContent
    (ifElseChildren
        ((val . elseSubExprParams) f)
        ((val . bodySubExprParams) f))
    x <&> ElseIf

bodySubExprParams :: Lens.Traversal' (Body name i o (Ann a)) (BinderParams name i o)
bodySubExprParams f (BodyLam x) = (lamFunc . funcSubExprParams) f x <&> BodyLam
bodySubExprParams f x =
    bodyChildren pure pure pure
    ((val . elseSubExprParams) f)
    (binderSubExprParams f)
    ((val . bodySubExprParams) f) x

binderBodySubExprParams :: Lens.Traversal' (Binder name i o (Ann a)) (BinderParams name i o)
binderBodySubExprParams f (BinderExpr x) =
    bodySubExprParams f x <&> BinderExpr
binderBodySubExprParams f (BinderLet x) =
    (\v b -> BinderLet x{_lValue = v, _lBody = b})
    <$> assignmentSubExprParams f (x ^. lValue)
    <*> binderSubExprParams f (x ^. lBody)

binderSubExprParams :: Lens.Traversal' (Node (Ann a) (Binder name i o)) (BinderParams name i o)
binderSubExprParams = val . binderBodySubExprParams

assignmentBodySubExprParams ::
    Lens.Traversal' (AssignmentBody name i o (Ann a)) (BinderParams name i o)
assignmentBodySubExprParams f (BodyPlain x) = (apBody . binderBodySubExprParams) f x <&> BodyPlain
assignmentBodySubExprParams f (BodyFunction x) = funcSubExprParams f x <&> BodyFunction

assignmentSubExprParams :: Lens.Traversal' (Assignment name i o a) (BinderParams name i o)
assignmentSubExprParams = val . assignmentBodySubExprParams
