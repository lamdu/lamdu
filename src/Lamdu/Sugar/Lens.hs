{-# LANGUAGE FlexibleContexts, RankNTypes, TemplateHaskell, ScopedTypeVariables #-}
module Lamdu.Sugar.Lens
    ( PayloadOf(..), _OfExpr, _OfLabeledApplyFunc, _OfNullaryVal
    , bodyChildren, overBodyChildren, bodyChildPayloads
    , labeledApplyChildren, overLabeledApplyChildren
    , binderExprs, binderContentExprs, funcExprs, assignmentExprs
    , subExprPayloads
    , payloadsOf
    , bodyUnfinished, unfinishedExprPayloads
    , defSchemes
    , assignmentBody, binderFormBody
    , assignmentAddFirstParam, binderFormAddFirstParam
    , binderFuncParamActions
    , binderContentResultExpr
    , binderContentEntityId
    , leftMostLeaf
    , definitionExprs
    , holeTransformExprs, holeOptionTransformExprs
    , annotationTypes
    , assignmentSubExprParams, binderSubExprParams
    , binderParamsAnnotations
    ) where

import qualified Control.Lens as Lens
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

data PayloadOf name i o
    = OfExpr (Expression name i o ())
    | OfLabeledApplyFunc (LabeledApplyFunc name o ())
    | OfRelayedArg (RelayedArg name o ())
    | OfNullaryVal (NullaryVal name i o ())
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

binderContentExprs ::
    Applicative f =>
    (Expression name i o a -> f (Expression name i o b)) ->
    BinderContent name i o a -> f (BinderContent name i o b)
binderContentExprs f (BinderExpr x) =
    f x <&> BinderExpr
binderContentExprs f (BinderLet x) =
    (\val bod -> x{_lValue=val, _lBody=bod})
    <$> assignmentExprs f (x ^. lValue)
    <*> binderExprs f (x ^. lBody)
    <&> BinderLet

binderExprs ::
    Applicative f =>
    (Expression name i o a -> f (Expression name i o b)) ->
    Binder name i o a -> f (Binder name i o b)
binderExprs = bContent . binderContentExprs

funcExprs ::
    Applicative f =>
    (Expression name i o a -> f (Expression name i o b)) ->
    Function name i o a -> f (Function name i o b)
funcExprs = fBody . binderExprs

labeledApplyChildren ::
    Applicative f =>
    (LabeledApplyFunc name o a -> f (LabeledApplyFunc name o b)) ->
    (RelayedArg name o a -> f (RelayedArg name o b)) ->
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
    (LabeledApplyFunc name o a -> LabeledApplyFunc name o b) ->
    (RelayedArg name o a -> RelayedArg name o b) ->
    (Expression name i o a -> Expression name i o b) ->
    LabeledApply name i o a -> LabeledApply name i o b
overLabeledApplyChildren l r e =
    Lens.runIdentity . labeledApplyChildren (pure . l) (pure . r) (pure . e)

injectValChildren ::
    Applicative f =>
    (NullaryVal name i o a -> f (NullaryVal name i o b)) ->
    (Expression name i o a -> f (Expression name i o b)) ->
    InjectVal name i o a -> f (InjectVal name i o b)
injectValChildren _ e (InjectVal x) = e x <&> InjectVal
injectValChildren n _ (InjectNullary x) = n x <&> InjectNullary

bodyChildren ::
    Applicative f =>
    (NullaryVal name i o a -> f (NullaryVal name i o b)) ->
    (LabeledApplyFunc name o a -> f (LabeledApplyFunc name o b)) ->
    (RelayedArg name o a -> f (RelayedArg name o b)) ->
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
    BodyIfElse       x -> traverse f x <&> BodyIfElse
    BodyInject       x -> iMVal (injectValChildren n f) x <&> BodyInject
    BodyFromNom      x -> traverse f x <&> BodyFromNom
    BodyFragment     x -> fExpr f x <&> BodyFragment
    BodyToNom        x -> (traverse . binderExprs) f x <&> BodyToNom

bodyChildPayloads ::
    forall name i o a.
    Lens.IndexedTraversal' (PayloadOf name i o) (Body name i o a) a
bodyChildPayloads f =
    bodyChildren
    (nullaryValPayload f)
    (Lens.cloneIndexedLens labeledFuncPayloads f)
    (Lens.cloneIndexedLens relayedPayloads f)
    (exprPayload f)
    where
        labeledFuncPayloads ::
            Lens.AnIndexedLens' (PayloadOf name i o) (LabeledApplyFunc name o a) a
        labeledFuncPayloads = labeledApplyFuncPayload
        relayedPayloads ::
            Lens.AnIndexedLens' (PayloadOf name i o) (RelayedArg name o a) a
        relayedPayloads = relayedArgPayload

overBodyChildren ::
    (NullaryVal name i o a -> NullaryVal name i o b) ->
    (LabeledApplyFunc name o a -> LabeledApplyFunc name o b) ->
    (RelayedArg name o a -> RelayedArg name o b) ->
    (Expression name i o a -> Expression name i o b) ->
    Body name i o a -> Body name i o b
overBodyChildren n f r e =
    Lens.runIdentity . bodyChildren (pure . n) (pure . f) (pure . r) (pure . e)

exprPayload ::
    Lens.IndexedLens' (PayloadOf name i o) (Expression name i o a) a
exprPayload f val@(Expression pl x) =
    Lens.indexed f (OfExpr (void val)) pl <&> (`Expression` x)

nullaryValPayload ::
    Lens.IndexedLens (PayloadOf name i o)
    (NullaryVal name i o a)
    (NullaryVal name i o b)
    a b
nullaryValPayload f val =
    Lens.indexed f (OfNullaryVal (void val)) (val ^. nullaryPayload)
    <&> \x -> val & nullaryPayload .~ x

labeledApplyFuncPayload ::
    Lens.AnIndexedLens (PayloadOf name i o)
    (LabeledApplyFunc name o a)
    (LabeledApplyFunc name o b)
    a b
labeledApplyFuncPayload f val@(LabeledApplyFunc func pl) =
    Lens.indexed f (OfLabeledApplyFunc (void val)) pl
    <&> LabeledApplyFunc func

relayedArgPayload ::
    Lens.AnIndexedLens (PayloadOf name i o)
    (RelayedArg name o a)
    (RelayedArg name o b)
    a b
relayedArgPayload f val@(RelayedArg x pl) =
    Lens.indexed f (OfRelayedArg (void val)) pl
    <&> RelayedArg x

subExprPayloads ::
    forall name i o a b.
    Lens.IndexedTraversal (PayloadOf name i o)
    (Expression name i o a)
    (Expression name i o b)
    a b
subExprPayloads f val@(Expression pl x) =
    flip Expression
    <$> bodyChildren
        (nullaryValPayload f)
        (Lens.cloneIndexedLens labeledFuncPayloads f)
        (Lens.cloneIndexedLens relayedPayloads f)
        (subExprPayloads f) x
    <*> Lens.indexed f (OfExpr (void val)) pl
    where
        labeledFuncPayloads ::
            Lens.AnIndexedLens (PayloadOf name i o)
            (LabeledApplyFunc name o a)
            (LabeledApplyFunc name o b)
            a b
        labeledFuncPayloads = labeledApplyFuncPayload
        relayedPayloads ::
            Lens.AnIndexedLens (PayloadOf name i o)
            (RelayedArg name o a)
            (RelayedArg name o b)
            a b
        relayedPayloads = relayedArgPayload

payloadsOf ::
    Lens.Fold (Body name i o ()) a ->
    Lens.IndexedTraversal' (PayloadOf name i o) (Expression name i o b) b
payloadsOf x =
    subExprPayloads . Lens.ifiltered predicate
    where
        predicate idx _ = Lens.has (_OfExpr . body . x) idx

binderVarRefUnfinished :: Lens.Traversal' (BinderVarRef name m) ()
binderVarRefUnfinished =
    bvForm . _GetDefinition . Lens.failing _DefDeleted (_DefTypeChanged . Lens.united)

bodyUnfinished :: Lens.Traversal' (Body name i o a) ()
bodyUnfinished =
    _BodyHole . Lens.united
    & Lens.failing (_BodyFragment . Lens.united)
    & Lens.failing (_BodyGetVar . _GetBinder . binderVarRefUnfinished)
    & Lens.failing (_BodyLabeledApply . aFunc . fVar . binderVarRefUnfinished)

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

binderContentResultExpr :: Lens' (BinderContent name i o a) (Expression name i o a)
binderContentResultExpr f (BinderLet l) = l & lBody . bContent . binderContentResultExpr %%~ f <&> BinderLet
binderContentResultExpr f (BinderExpr e) = f e <&> BinderExpr

binderContentEntityId ::
    Lens' (BinderContent name i o (Payload name i o a)) EntityId
binderContentEntityId f (BinderExpr e) =
    e & annotation . plEntityId %%~ f <&> BinderExpr
binderContentEntityId f (BinderLet l) =
    l & lEntityId %%~ f <&> BinderLet

leftMostLeaf :: Expression name i o a -> Expression name i o a
leftMostLeaf val =
    case val ^.. body . bodyChildren pure pure pure of
    [] -> val
    (x:_) -> leftMostLeaf x

definitionExprs ::
    Lens.Traversal
    (Definition name i o a) (Definition name i o b)
    (Expression name i o a) (Expression name i o b)
definitionExprs = drBody . _DefinitionBodyExpression . deContent . assignmentExprs

holeOptionTransformExprs ::
    Monad i =>
    (BinderContent n0 i o (Payload n0 i o ()) ->
     i (BinderContent n1 i o (Payload n1 i o ()))) ->
    HoleOption n0 i o -> HoleOption n1 i o
holeOptionTransformExprs onExpr option =
    option
    { _hoSugaredBaseExpr = option ^. hoSugaredBaseExpr >>= onExpr
    , _hoResults = option ^. hoResults <&> Lens._2 %~ (>>= holeResultConverted onExpr)
    }

holeTransformExprs ::
    Monad i =>
    (BinderContent n0 i o (Payload n0 i o ()) ->
        i (BinderContent n1 i o (Payload n1 i o ()))) ->
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

binderFormAddFirstParam :: Lens' (AssignmentBody name i o a) (AddFirstParam name i o)
binderFormAddFirstParam f (BodyFunction x) = (afFunction . fAddFirstParam) f x <&> BodyFunction
binderFormAddFirstParam f (BodyPlain x) = apAddFirstParam f x <&> BodyPlain

assignmentAddFirstParam :: Lens' (Assignment name i o a) (AddFirstParam name i o)
assignmentAddFirstParam = aBody . binderFormAddFirstParam

annotationTypes :: Lens.Traversal' (Annotation name i) (Type name)
annotationTypes _ AnnotationNone = pure AnnotationNone
annotationTypes f (AnnotationType x) = f x <&> AnnotationType
annotationTypes f (AnnotationVal x) = (annotationType . Lens._Just) f x <&> AnnotationVal

binderParamsAnnotations :: Lens.Traversal' (BinderParams name i o) (Annotation name i)
binderParamsAnnotations f (NullParam x) = fpAnnotation f x <&> NullParam
binderParamsAnnotations f (Params xs) = (traverse . fpAnnotation) f xs <&> Params

funcSubExprParams :: Lens.Traversal' (Function name i o a) (BinderParams name i o)
funcSubExprParams f x =
    (\p b -> x{_fParams = p, _fBody = b})
    <$> f (x ^. fParams)
    <*> binderSubExprParams f (x ^. fBody)

bodySubExprParams :: Lens.Traversal' (Body name i o a) (BinderParams name i o)
bodySubExprParams f (BodyLam x) = (lamFunc . funcSubExprParams) f x <&> BodyLam
bodySubExprParams f x = bodyChildren pure pure pure ((body . bodySubExprParams) f) x

binderContentSubExprParams :: Lens.Traversal' (BinderContent name i o a) (BinderParams name i o)
binderContentSubExprParams f (BinderExpr x) =
    (body . bodySubExprParams) f x <&> BinderExpr
binderContentSubExprParams f (BinderLet x) =
    (\v b -> BinderLet x{_lValue = v, _lBody = b})
    <$> assignmentSubExprParams f (x ^. lValue)
    <*> binderSubExprParams f (x ^. lBody)

binderSubExprParams :: Lens.Traversal' (Binder name i o a) (BinderParams name i o)
binderSubExprParams = bContent . binderContentSubExprParams

assignmentBodySubExprParams :: Lens.Traversal' (AssignmentBody name i o a) (BinderParams name i o)
assignmentBodySubExprParams f (BodyPlain x) = (apBody . binderSubExprParams) f x <&> BodyPlain
assignmentBodySubExprParams f (BodyFunction x) = (afFunction . funcSubExprParams) f x <&> BodyFunction

assignmentSubExprParams :: Lens.Traversal' (Assignment name i o a) (BinderParams name i o)
assignmentSubExprParams = aBody . assignmentBodySubExprParams
