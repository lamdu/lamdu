{-# LANGUAGE FlexibleContexts, RankNTypes, TemplateHaskell, ScopedTypeVariables #-}
module Lamdu.Sugar.Lens
    ( PayloadOf(..), _OfExpr, _OfLabeledApplyFunc, _OfNullaryVal
    , bodyChildren, overBodyChildren, bodyChildPayloads
    , labeledApplyChildren
    , binderExprs, binderContentExprs, funcExprs, assignmentExprs
    , subExprPayloads, payloadsIndexedByPath
    , payloadsOf
    , bodyUnfinished, unfinishedExprPayloads, fragmentExprs
    , defSchemes
    , assignmentBody, binderFormBody
    , assignmentAddFirstParam, binderFormAddFirstParam
    , binderFuncParamActions
    , binderContentResultExpr
    , binderContentEntityId
    , leftMostLeaf
    , workAreaExpressions, definitionExprs
    , holeTransformExprs, holeOptionTransformExprs
    ) where

import qualified Control.Lens as Lens
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

data PayloadOf name i o
    = OfExpr (Expression name i o ())
    | OfLabeledApplyFunc (LabeledApplyFunc name o ())
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

letExprs ::
    Applicative f =>
    (Expression name i o a -> f (Expression name i o b)) ->
    Let name i o a -> f (Let name i o b)
letExprs f x =
    (\val bod -> x{_lValue=val, _lBody=bod})
    <$> assignmentExprs f (x ^. lValue)
    <*> binderExprs f (x ^. lBody)

binderContentExprs ::
    Applicative f =>
    (Expression name i o a -> f (Expression name i o b)) ->
    BinderContent name i o a -> f (BinderContent name i o b)
binderContentExprs f (BinderExpr x) = f x <&> BinderExpr
binderContentExprs f (BinderLet x) = letExprs f x <&> BinderLet

binderExprs ::
    Applicative f =>
    (Expression name i o a -> f (Expression name i o b)) ->
    Binder name i o a -> f (Binder name i o b)
binderExprs = bbContent . binderContentExprs

funcExprs ::
    Applicative f =>
    (Expression name i o a -> f (Expression name i o b)) ->
    Function name i o a -> f (Function name i o b)
funcExprs = fBody . binderExprs

labeledApplyChildren ::
    Applicative f =>
    (LabeledApplyFunc name o a -> f (LabeledApplyFunc name o b)) ->
    (Expression name i o a -> f (Expression name i o b)) ->
    LabeledApply name i o a -> f (LabeledApply name i o b)
labeledApplyChildren l e (LabeledApply func special annotated relayed) =
    uncurry LabeledApply
    <$> funcAndSpecial
    <*> (traverse . traverse) e annotated
    <*> pure relayed
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
    (Expression name i o a -> f (Expression name i o b)) ->
    Body name i o a -> f (Body name i o b)
bodyChildren n l f =
    \case
    BodyPlaceHolder -> pure BodyPlaceHolder
    BodyLiteral x -> BodyLiteral x & pure
    BodyGetVar  x -> BodyGetVar  x & pure
    BodyHole    x -> BodyHole    x & pure
    BodyLam          x -> (lamFunc . funcExprs) f x <&> BodyLam
    BodySimpleApply  x -> traverse f x <&> BodySimpleApply
    BodyLabeledApply x -> labeledApplyChildren l f x <&> BodyLabeledApply
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
    bodyChildren (nullaryValPayload f) (Lens.cloneIndexedLens labeledFuncPayloads f) g
    where
        g val@(Expression pl x) = Lens.indexed f (OfExpr (void val)) pl <&> (`Expression` x)
        labeledFuncPayloads ::
            Lens.AnIndexedLens' (PayloadOf name i o) (LabeledApplyFunc name o a) a
        labeledFuncPayloads = labeledApplyFuncPayload

overBodyChildren ::
    (NullaryVal name i o a -> NullaryVal name i o b) ->
    (LabeledApplyFunc name o a -> LabeledApplyFunc name o b) ->
    (Expression name i o a -> Expression name i o b) ->
    Body name i o a -> Body name i o b
overBodyChildren n f e =
    Lens.runIdentity . bodyChildren (pure . n) (pure . f) (pure . e)

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

subExprPayloads ::
    forall name i o a b.
    Lens.IndexedTraversal (PayloadOf name i o)
    (Expression name i o a)
    (Expression name i o b)
    a b
subExprPayloads f val@(Expression pl x) =
    flip Expression
    <$> bodyChildren (nullaryValPayload f) (Lens.cloneIndexedLens labeledFuncPayloads f) (subExprPayloads f) x
    <*> Lens.indexed f (OfExpr (void val)) pl
    where
        labeledFuncPayloads ::
            Lens.AnIndexedLens (PayloadOf name i o)
            (LabeledApplyFunc name o a)
            (LabeledApplyFunc name o b)
            a b
        labeledFuncPayloads = labeledApplyFuncPayload

payloadsIndexedByPath ::
    Lens.IndexedTraversal [PayloadOf name i o]
    (Expression name i o a)
    (Expression name i o b)
    a b
payloadsIndexedByPath f =
    go []
    where
        go path val@(Expression pl x) =
            Expression
            <$> Lens.indexed f newPath pl
            <*> bodyChildren (goNull newPath) (goFunc newPath) (go newPath) x
            where
                newPath = OfExpr (void val) : path
        goFunc path val@(LabeledApplyFunc func pl) =
            Lens.indexed f (OfLabeledApplyFunc (void val) : path) pl
            <&> LabeledApplyFunc func
        goNull path val =
            Lens.indexed f (OfNullaryVal (void val) : path) (val ^. nullaryPayload)
            <&> \x -> val & nullaryPayload .~ x

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
    & Lens.failing (_BodyLabeledApply . aFunc . afVar . binderVarRefUnfinished)

unfinishedExprPayloads ::
    Lens.IndexedTraversal' (PayloadOf name i o) (Expression name i o a) a
unfinishedExprPayloads = payloadsOf bodyUnfinished

subExprsOf ::
    Lens.Traversal' (Body name i o ()) b ->
    Lens.IndexedTraversal' [PayloadOf name i o] (Expression name i o a) a
subExprsOf f =
    payloadsIndexedByPath . Lens.ifiltered predicate
    where
        predicate (_:parent:_) _ = Lens.has (_OfExpr . body . f) parent
        predicate _ _ = False

fragmentExprs ::
    Lens.IndexedTraversal' [PayloadOf name i o] (Expression name i o a) a
fragmentExprs = subExprsOf _BodyFragment

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
binderContentResultExpr f (BinderLet l) = l & lBody . bbContent . binderContentResultExpr %%~ f <&> BinderLet
binderContentResultExpr f (BinderExpr e) = f e <&> BinderExpr

binderContentEntityId ::
    Lens' (BinderContent name i o (Payload name i o a)) EntityId
binderContentEntityId f (BinderExpr e) =
    e & annotation . plEntityId %%~ f <&> BinderExpr
binderContentEntityId f (BinderLet l) =
    l & lEntityId %%~ f <&> BinderLet

leftMostLeaf :: Expression name i o a -> Expression name i o a
leftMostLeaf val =
    case val ^.. body . bodyChildren pure pure of
    [] -> val
    (x:_) -> leftMostLeaf x

definitionExprs ::
    Lens.Traversal
    (Definition name i o a) (Definition name i o b)
    (Expression name i o a) (Expression name i o b)
definitionExprs = drBody . _DefinitionBodyExpression . deContent . aBody . assignmentBodyExprs

workAreaExpressions ::
    Lens.Traversal
    (WorkArea name i o a) (WorkArea name i o b)
    (Expression name i o a) (Expression name i o b)
workAreaExpressions f (WorkArea panes repl globals) =
    WorkArea
    <$> (traverse . paneDefinition . definitionExprs) f panes
    <*> (replExpr . binderExprs) f repl
    ?? globals

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
