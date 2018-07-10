{-# LANGUAGE FlexibleContexts, RankNTypes, TemplateHaskell, ScopedTypeVariables #-}
module Lamdu.Sugar.Lens
    ( PayloadOf(..), _OfExpr, _OfLabeledApplyFunc, _OfNullaryVal
    , bodyChildren, overBodyChildren, bodyChildPayloads
    , labeledApplyChildren, overLabeledApplyChildren
    , ifElseChildren
    , binderExprs, funcExprs, assignmentExprs
    , exprPayloads
    , payloadsOf
    , bodyUnfinished, unfinishedExprPayloads
    , defSchemes
    , assignmentBody, binderFormBody
    , assignmentAddFirstParam
    , binderFuncParamActions
    , binderResultExpr
    , binderEntityId
    , definitionExprs
    , holeTransformExprs, holeOptionTransformExprs
    , annotationTypes
    , assignmentSubExprParams, binderSubExprParams
    , paramsAnnotations
    ) where

import qualified Control.Lens as Lens
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

data PayloadOf name i o
    = OfExpr (Body name i o ())
    | OfLabeledApplyFunc (BinderVarRef name o)
    | OfRelayedArg (GetVar name o)
    | OfNullaryVal (NullaryVal name i o)
Lens.makePrisms ''PayloadOf

assignmentBodyExprs ::
    Applicative f =>
    (Expression name i o a -> f (Expression name i o b)) ->
    AssignmentBody name i o a -> f (AssignmentBody name i o b)
assignmentBodyExprs f (BodyFunction x) = (afFunction . funcExprs) f x <&> BodyFunction
assignmentBodyExprs f (BodyPlain x) = (apBody . binderExprs) f x <&> BodyPlain

assignmentExprs ::
    Applicative f =>
    (Expression name i o a -> f (Expression name i o b)) ->
    Assignment name i o a -> f (Assignment name i o b)
assignmentExprs = aBody . assignmentBodyExprs

binderExprs ::
    Applicative f =>
    (Expression name i o a -> f (Expression name i o b)) ->
    Binder name i o a -> f (Binder name i o b)
binderExprs f (BinderExpr x) =
    f x <&> BinderExpr
binderExprs f (BinderLet x) =
    (\v bod -> x{_lValue=v, _lBody=bod})
    <$> assignmentExprs f (x ^. lValue)
    <*> binderExprs f (x ^. lBody)
    <&> BinderLet

funcExprs ::
    Applicative f =>
    (Expression name i o a -> f (Expression name i o b)) ->
    Function name i o a -> f (Function name i o b)
funcExprs = fBody . binderExprs

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

elseChildren ::
    Applicative f =>
    (Expression name i o a -> f (Expression name i o b)) ->
    Else name i o a -> f (Else name i o b)
elseChildren f (SimpleElse x) = f x <&> SimpleElse
elseChildren f (ElseIf x) = (eiContent . ifElseChildren) f x <&> ElseIf

ifElseChildren ::
    Applicative f =>
    (Expression name i o a -> f (Expression name i o b)) ->
    IfElse name i o a -> f (IfElse name i o b)
ifElseChildren f (IfElse i t d e) =
    IfElse <$> f i <*> f t <*> pure d <*> elseChildren f e

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
    (Expression name i o a -> f (Expression name i o b)) ->
    Body name i o a -> f (Body name i o b)
bodyChildren n l r f =
    \case
    BodyPlaceHolder -> pure BodyPlaceHolder
    BodyLiteral x -> BodyLiteral x & pure
    BodyGetVar  x -> BodyGetVar  x & pure
    BodyHole    x -> BodyHole    x & pure
    BodyLam          x -> (lamFunc . funcExprs) f x <&> BodyLam
    BodySimpleApply  x -> traverse f x <&> BodySimpleApply
    BodyLabeledApply x -> labeledApplyChildren l r f x <&> BodyLabeledApply
    BodyRecord       x -> traverse f x <&> BodyRecord
    BodyGetField     x -> traverse f x <&> BodyGetField
    BodyCase         x -> traverse f x <&> BodyCase
    BodyIfElse       x -> ifElseChildren f x <&> BodyIfElse
    BodyInject       x -> iContent (injectContentChildren n f) x <&> BodyInject
    BodyFromNom      x -> traverse f x <&> BodyFromNom
    BodyFragment     x -> fExpr f x <&> BodyFragment
    BodyToNom        x -> (traverse . binderExprs) f x <&> BodyToNom

parentNodePayload ::
    (f a -> p) ->
    Lens.IndexedLens' p (ParentNode f a) a
parentNodePayload c f (PNode (Node pl x)) =
    Lens.indexed f (c x) pl <&> (`Node` x) <&> PNode

bodyChildPayloads ::
    forall name i o a.
    Lens.IndexedTraversal' (PayloadOf name i o) (Body name i o a) a
bodyChildPayloads f =
    bodyChildren
    (leafNodePayload OfNullaryVal f)
    (Lens.cloneIndexedLens labeledFuncPayloads f)
    (Lens.cloneIndexedLens relayedPayloads f)
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
    (Expression name i o a -> Expression name i o b) ->
    Body name i o a -> Body name i o b
overBodyChildren n f r e =
    Lens.runIdentity . bodyChildren (pure . n) (pure . f) (pure . r) (pure . e)

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

exprPayloads ::
    Lens.IndexedTraversal (PayloadOf name i o)
    (Expression name i o a)
    (Expression name i o b)
    a b
exprPayloads = parentNodePayloads bodyPayloads (OfExpr . void)

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

binderResultExpr :: Lens.IndexedLens' (PayloadOf name i o) (Binder name i o a) a
binderResultExpr f (BinderLet l) = lBody (binderResultExpr f) l <&> BinderLet
binderResultExpr f (BinderExpr e) = parentNodePayload (OfExpr . void) f e <&> BinderExpr

binderEntityId ::
    Lens' (Binder name i o (Payload name i o a)) EntityId
binderEntityId f (BinderExpr e) =
    e & _PNode . ann . plEntityId %%~ f <&> BinderExpr
binderEntityId f (BinderLet l) =
    l & lEntityId %%~ f <&> BinderLet

definitionExprs ::
    Lens.Traversal
    (Definition name i o a) (Definition name i o b)
    (Expression name i o a) (Expression name i o b)
definitionExprs = drBody . _DefinitionBodyExpression . deContent . assignmentExprs

holeOptionTransformExprs ::
    Monad i =>
    (Binder n0 i o (Payload n0 i o ()) ->
     i (Binder n1 i o (Payload n1 i o ()))) ->
    HoleOption n0 i o -> HoleOption n1 i o
holeOptionTransformExprs onExpr option =
    option
    { _hoSugaredBaseExpr = option ^. hoSugaredBaseExpr >>= onExpr
    , _hoResults = option ^. hoResults <&> Lens._2 %~ (>>= holeResultConverted onExpr)
    }

holeTransformExprs ::
    Monad i =>
    (Binder n0 i o (Payload n0 i o ()) ->
        i (Binder n1 i o (Payload n1 i o ()))) ->
    Hole n0 i o -> Hole n1 i o
holeTransformExprs onExpr hole =
    hole
    { _holeOptions = hole ^. holeOptions <&> traverse %~ holeOptionTransformExprs onExpr
    , _holeOptionLiteral =
        hole ^. holeOptionLiteral <&> Lens.mapped . Lens._2 %~ (>>= holeResultConverted onExpr)
    }

binderFormBody ::
    Lens
    (AssignmentBody name i o a)
    (AssignmentBody name i o b)
    (Binder name i o a)
    (Binder name i o b)
binderFormBody f (BodyFunction x) = (afFunction . fBody) f x <&> BodyFunction
binderFormBody f (BodyPlain x) = apBody f x <&> BodyPlain

assignmentBody ::
    Lens
    (Assignment name i o a)
    (Assignment name i o b)
    (Binder name i o a)
    (Binder name i o b)
assignmentBody = aBody . binderFormBody

assignmentBodyAddFirstParam :: Lens' (AssignmentBody name i o a) (AddFirstParam name i o)
assignmentBodyAddFirstParam f (BodyFunction x) = (afFunction . fAddFirstParam) f x <&> BodyFunction
assignmentBodyAddFirstParam f (BodyPlain x) = apAddFirstParam f x <&> BodyPlain

assignmentAddFirstParam :: Lens' (Assignment name i o a) (AddFirstParam name i o)
assignmentAddFirstParam = aBody . assignmentBodyAddFirstParam

annotationTypes :: Lens.Traversal' (Annotation name i) (Type name)
annotationTypes _ AnnotationNone = pure AnnotationNone
annotationTypes f (AnnotationType x) = f x <&> AnnotationType
annotationTypes f (AnnotationVal x) = (annotationType . Lens._Just) f x <&> AnnotationVal

paramsAnnotations :: Lens.Traversal' (BinderParams name i o) (Annotation name i)
paramsAnnotations f (NullParam x) = fpAnnotation f x <&> NullParam
paramsAnnotations f (Params xs) = (traverse . fpAnnotation) f xs <&> Params

funcSubExprParams :: Lens.Traversal' (Function name i o a) (BinderParams name i o)
funcSubExprParams f x =
    (\p b -> x{_fParams = p, _fBody = b})
    <$> f (x ^. fParams)
    <*> binderSubExprParams f (x ^. fBody)

bodySubExprParams :: Lens.Traversal' (Body name i o a) (BinderParams name i o)
bodySubExprParams f (BodyLam x) = (lamFunc . funcSubExprParams) f x <&> BodyLam
bodySubExprParams f x = bodyChildren pure pure pure ((_PNode . val . bodySubExprParams) f) x

binderSubExprParams :: Lens.Traversal' (Binder name i o a) (BinderParams name i o)
binderSubExprParams f (BinderExpr x) =
    (_PNode . val . bodySubExprParams) f x <&> BinderExpr
binderSubExprParams f (BinderLet x) =
    (\v b -> BinderLet x{_lValue = v, _lBody = b})
    <$> assignmentSubExprParams f (x ^. lValue)
    <*> binderSubExprParams f (x ^. lBody)

assignmentBodySubExprParams :: Lens.Traversal' (AssignmentBody name i o a) (BinderParams name i o)
assignmentBodySubExprParams f (BodyPlain x) = (apBody . binderSubExprParams) f x <&> BodyPlain
assignmentBodySubExprParams f (BodyFunction x) = (afFunction . funcSubExprParams) f x <&> BodyFunction

assignmentSubExprParams :: Lens.Traversal' (Assignment name i o a) (BinderParams name i o)
assignmentSubExprParams = aBody . assignmentBodySubExprParams
