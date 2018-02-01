{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, RecordWildCards, RankNTypes #-}
module Lamdu.Sugar.Lens
    ( subExprPayloads, payloadsIndexedByPath
    , payloadsOf
    , bodyUnfinished, unfinishedExprPayloads, fragmentExprs
    , defSchemes
    , binderFuncParamActions
    , binderContentExpr
    , binderContentEntityId
    , leftMostLeaf
    , workAreaExpressions
    , workAreaTagNames
    ) where

import qualified Control.Lens as Lens
import           Lamdu.Calc.Type.Scheme (Scheme)
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Types

import           Lamdu.Prelude

subExprPayloads ::
    Lens.IndexedTraversal
    (Expression name m ())
    (Expression name m a)
    (Expression name m b)
    (Payload m a) (Payload m b)
subExprPayloads f val@(Expression body pl) =
    Expression
    <$> (Lens.traversed .> subExprPayloads) f body
    <*> Lens.indexed f (void val) pl

payloadsIndexedByPath ::
    Lens.IndexedTraversal
    [Expression name m ()]
    (Expression name m a)
    (Expression name m b)
    (Payload m a) (Payload m b)
payloadsIndexedByPath f =
    go []
    where
        go path val@(Expression body pl) =
            Expression
            <$> Lens.traversed (go newPath) body
            <*> Lens.indexed f newPath pl
            where
                newPath = void val : path

payloadsOf ::
    Lens.Fold (Body name m (Expression name m ())) a ->
    Lens.IndexedTraversal'
    (Expression name m ())
    (Expression name m b)
    (Payload m b)
payloadsOf body =
    subExprPayloads . Lens.ifiltered predicate
    where
        predicate idx _ = Lens.has (rBody . body) idx

binderVarRefUnfinished :: Lens.Traversal' (BinderVarRef name m) ()
binderVarRefUnfinished =
    bvForm . _GetDefinition . Lens.failing _DefDeleted (_DefTypeChanged . Lens.united)

bodyUnfinished :: Lens.Traversal' (Body name m a) ()
bodyUnfinished =
    _BodyHole . Lens.united
    & Lens.failing (_BodyFragment . Lens.united)
    & Lens.failing (_BodyGetVar . _GetBinder . binderVarRefUnfinished)
    & Lens.failing (_BodyLabeledApply . aFunc . binderVarRefUnfinished)

unfinishedExprPayloads ::
    Lens.IndexedTraversal'
    (Expression name m ())
    (Expression name m a)
    (Payload m a)
unfinishedExprPayloads = payloadsOf bodyUnfinished

subExprsOf ::
    Lens.Traversal' (Body name m (Expression name m ())) b ->
    Lens.IndexedTraversal'
    [Expression name m ()]
    (Expression name m a)
    (Payload m a)
subExprsOf f =
    payloadsIndexedByPath . Lens.ifiltered predicate
    where
        predicate (_:parent:_) _ = Lens.has (rBody . f) parent
        predicate _ _ = False

fragmentExprs ::
    Lens.IndexedTraversal'
    [Expression name m ()]
    (Expression name m a)
    (Payload m a)
fragmentExprs = subExprsOf _BodyFragment

defBodySchemes :: Lens.Traversal' (DefinitionBody name m expr) Scheme
defBodySchemes f (DefinitionBodyBuiltin b) =
    b & biType %%~ f
    <&> DefinitionBodyBuiltin
defBodySchemes f (DefinitionBodyExpression de) =
    de & deType %%~ f
    <&> DefinitionBodyExpression

defSchemes :: Lens.Traversal' (Definition name m expr) Scheme
defSchemes = drBody . defBodySchemes

binderFuncParamActions ::
    Lens.Traversal' (BinderParams name m) (FuncParamActions m)
binderFuncParamActions _ BinderWithoutParams = pure BinderWithoutParams
binderFuncParamActions _ (NullParam a) = pure (NullParam a)
binderFuncParamActions f (Params ps) = (traverse . fpInfo . piActions) f ps <&> Params

binderContentExpr :: Lens' (BinderContent name m a) a
binderContentExpr f (BinderLet l) = l & lBody . bbContent . binderContentExpr %%~ f <&> BinderLet
binderContentExpr f (BinderExpr e) = f e <&> BinderExpr

binderContentEntityId ::
    Lens' (BinderContent name m (Expression name m a)) EntityId
binderContentEntityId f (BinderExpr e) =
    e & rPayload . plEntityId %%~ f <&> BinderExpr
binderContentEntityId f (BinderLet l) =
    l & lEntityId %%~ f <&> BinderLet

leftMostLeaf :: Expression name m a -> Expression name m a
leftMostLeaf val =
    case val ^.. rBody . Lens.traversed of
    [] -> val
    (x:_) -> leftMostLeaf x

workAreaExpressions ::
    Lens.Traversal
    (WorkArea name m a) (WorkArea name m b)
    (Expression name m a) (Expression name m b)
workAreaExpressions f (WorkArea panes repl) =
    WorkArea
    <$> (traverse . paneDefinition . traverse) f panes
    <*> f repl

binderContentTagNames :: Functor m => Lens.IndexedSetter' T.Tag (BinderContent name m (Expression name m a)) name
binderContentTagNames f (BinderExpr expr) =
    expressionTagNames f expr <&> BinderExpr
binderContentTagNames f (BinderLet Let{..}) =
    (\_lValue _lName _lBody -> Let{..})
    <$> binderTagNames f _lValue
    <*> tagNames f _lName
    <*> (bbContent . binderContentTagNames) f _lBody
    <&> BinderLet

newTagNames ::
    Lens.IndexedTraversal T.Tag (name0, TagInfo, a) (name1, TagInfo, a) name0 name1
newTagNames f (n, info, pl) =
    Lens.indexed f (info ^. tagVal) n
    <&> \x -> (x, info, pl)

tagOptionNames ::
    Lens.IndexedTraversal T.Tag (TagOption name0 m a) (TagOption name1 m a) name0 name1
tagOptionNames f TagOption{..} =
    Lens.indexed f (_toInfo ^. tagVal) _toName
    <&> \_toName -> TagOption{..}

tagSelectionTagNames ::
    Functor m => Lens.IndexedSetter T.Tag (TagSelection name0 m a) (TagSelection name1 m a) name0 name1
tagSelectionTagNames f TagSelection{..} =
    (\_tsOptions _tsNewTag -> TagSelection{..})
    <$> (Lens.mapped . traverse . tagOptionNames) f _tsOptions
    <*> (Lens.mapped . newTagNames) f _tsNewTag

tagNames ::
    Functor m => Lens.IndexedSetter T.Tag (Tag name0 m) (Tag name1 m) name0 name1
tagNames f Tag{..} =
    (\_tagName _tagSelection -> Tag{..})
    <$> Lens.indexed f (_tagInfo ^. tagVal) _tagName
    <*> tagSelectionTagNames f _tagSelection

binderTagNames :: Functor m => Lens.IndexedSetter' T.Tag (Binder name m (Expression name m a)) name
binderTagNames f Binder{..} =
    (\_bParams _bBody -> Binder{..})
    <$> (_Params . traverse . traverse . piTag . tagNames) f _bParams
    <*> (bbContent . binderContentTagNames) f _bBody

definitionTagNames :: Functor m => Lens.IndexedSetter' T.Tag (Definition name m (Expression name m a)) name
definitionTagNames f Definition{..} =
    (\_drName _drBody -> Definition{..})
    <$> tagNames f _drName
    <*> (_DefinitionBodyExpression . deContent . binderTagNames) f _drBody

compositeItemTagNames :: Functor m => Lens.IndexedSetter' T.Tag (CompositeItem name m (Expression name m a)) name
compositeItemTagNames f CompositeItem{..} =
    (\_ciTag _ciExpr -> CompositeItem{..})
    <$> tagNames f _ciTag
    <*> expressionTagNames f _ciExpr

compositeTagNames :: Functor m => Lens.IndexedSetter' T.Tag (Composite name m (Expression name m a)) name
compositeTagNames f (Composite items t addItem) =
    Composite
    <$> (traverse . compositeItemTagNames) f items
    <*> (traverse . expressionTagNames) f t
    <*> tagSelectionTagNames f addItem

exprBodyTagNames :: Functor m => Lens.IndexedSetter' T.Tag (Body name m (Expression name m a)) name
exprBodyTagNames f (BodyGetField (GetField r t)) =
    GetField <$> expressionTagNames f r <*> tagNames f t <&> BodyGetField
exprBodyTagNames f (BodyInject (Inject t v)) =
    Inject <$> tagNames f t <*> (traverse . expressionTagNames) f v <&> BodyInject
exprBodyTagNames f (BodyCase (Case k b)) =
    Case
    <$> (traverse . expressionTagNames) f k
    <*> compositeTagNames f b
    <&> BodyCase
exprBodyTagNames f (BodyRecord r) = compositeTagNames f r <&> BodyRecord
exprBodyTagNames f (BodyLam lam) = (lamBinder . binderTagNames) f lam <&> BodyLam
exprBodyTagNames f x = (traverse . expressionTagNames) f x

expressionTagNames :: Functor m => Lens.IndexedSetter' T.Tag (Expression name m a) name
expressionTagNames = rBody . exprBodyTagNames

-- TODO: This traversal and with all its helper sub-traversals is somewhat tedious
-- and bug-prone. Perhaps some more generic SYB solution is suitable instead.
workAreaTagNames :: Functor m => Lens.IndexedSetter' T.Tag (WorkArea name m a) name
workAreaTagNames f (WorkArea panes repl) =
    WorkArea
    <$> (traverse . paneDefinition . definitionTagNames) f panes
    <*> expressionTagNames f repl
