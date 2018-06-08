{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Lamdu.Sugar.Lens
    ( subExprPayloads, payloadsIndexedByPath
    , payloadsOf
    , bodyUnfinished, unfinishedExprPayloads, fragmentExprs
    , defSchemes
    , assignmentBody, binderFormBody
    , assignmentAddFirstParam, binderFormAddFirstParam
    , binderFuncParamActions
    , binderContentExpr
    , binderContentEntityId
    , leftMostLeaf
    , workAreaExpressions
    , holeTransformExprs, holeOptionTransformExprs
    ) where

import qualified Control.Lens as Lens
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

subExprPayloads ::
    Lens.IndexedTraversal
    (Expression name i o ())
    (Expression name i o a)
    (Expression name i o b)
    (Payload name i o a) (Payload name i o b)
subExprPayloads f val@(Expression pl x) =
    Expression
    <$> Lens.indexed f (void val) pl
    <*> (Lens.traversed .> subExprPayloads) f x

payloadsIndexedByPath ::
    Lens.IndexedTraversal
    [Expression name i o ()]
    (Expression name i o a)
    (Expression name i o b)
    (Payload name i o a) (Payload name i o b)
payloadsIndexedByPath f =
    go []
    where
        go path val@(Expression pl x) =
            Expression
            <$> Lens.indexed f newPath pl
            <*> Lens.traversed (go newPath) x
            where
                newPath = void val : path

payloadsOf ::
    Lens.Fold (Body name i o (Expression name i o ())) a ->
    Lens.IndexedTraversal'
    (Expression name i o ())
    (Expression name i o b)
    (Payload name i o b)
payloadsOf x =
    subExprPayloads . Lens.ifiltered predicate
    where
        predicate idx _ = Lens.has (body . x) idx

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
    Lens.IndexedTraversal'
    (Expression name i o ())
    (Expression name i o a)
    (Payload name i o a)
unfinishedExprPayloads = payloadsOf bodyUnfinished

subExprsOf ::
    Lens.Traversal' (Body name i o (Expression name i o ())) b ->
    Lens.IndexedTraversal'
    [Expression name i o ()]
    (Expression name i o a)
    (Payload name i o a)
subExprsOf f =
    payloadsIndexedByPath . Lens.ifiltered predicate
    where
        predicate (_:parent:_) _ = Lens.has (body . f) parent
        predicate _ _ = False

fragmentExprs ::
    Lens.IndexedTraversal'
    [Expression name i o ()]
    (Expression name i o a)
    (Payload name i o a)
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

binderContentExpr :: Lens' (BinderContent name i o a) a
binderContentExpr f (BinderLet l) = l & lBody . bbContent . binderContentExpr %%~ f <&> BinderLet
binderContentExpr f (BinderExpr e) = f e <&> BinderExpr

binderContentEntityId ::
    Lens' (BinderContent name i o (Expression name i o a)) EntityId
binderContentEntityId f (BinderExpr e) =
    e & annotation . plEntityId %%~ f <&> BinderExpr
binderContentEntityId f (BinderLet l) =
    l & lEntityId %%~ f <&> BinderLet

leftMostLeaf :: Expression name i o a -> Expression name i o a
leftMostLeaf val =
    case val ^.. body . Lens.traversed of
    [] -> val
    (x:_) -> leftMostLeaf x

workAreaExpressions ::
    Lens.Traversal
    (WorkArea name i o a) (WorkArea name i o b)
    (Expression name i o a) (Expression name i o b)
workAreaExpressions f (WorkArea panes repl globals) =
    WorkArea
    <$> (traverse . paneDefinition . traverse) f panes
    <*> replExpr f repl
    ?? globals

holeOptionTransformExprs ::
    Monad i => (a -> i b) -> HoleOption i o a -> HoleOption i o b
holeOptionTransformExprs onExpr option =
    option
    { _hoSugaredBaseExpr = option ^. hoSugaredBaseExpr >>= onExpr
    , _hoResults = option ^. hoResults <&> Lens._2 %~ (>>= holeResultConverted onExpr)
    }

holeTransformExprs ::
    Monad i => (a -> i b) -> Hole i o a -> Hole i o b
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
    (BinderBody name i o a)
    (BinderBody name i o b)
binderFormBody f (BodyFunction x) = (afFunction . fBody) f x <&> BodyFunction
binderFormBody f (BodyPlain x) = apBody f x <&> BodyPlain

assignmentBody ::
    Lens
    (Assignment name i o a)
    (Assignment name i o b)
    (BinderBody name i o a)
    (BinderBody name i o b)
assignmentBody = aBody . binderFormBody

binderFormAddFirstParam :: Lens' (AssignmentBody name i o a) (AddFirstParam name i o)
binderFormAddFirstParam f (BodyFunction x) = (afFunction . fAddFirstParam) f x <&> BodyFunction
binderFormAddFirstParam f (BodyPlain x) = apAddFirstParam f x <&> BodyPlain

assignmentAddFirstParam :: Lens' (Assignment name i o a) (AddFirstParam name i o)
assignmentAddFirstParam = aBody . binderFormAddFirstParam
