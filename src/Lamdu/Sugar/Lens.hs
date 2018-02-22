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
    Lens.Traversal' (BinderParams name m) (FuncParamActions name m)
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

binderContentTagNames ::
    Functor m =>
    Lens.IndexedSetter T.Tag
    (BinderContent name0 m (Expression name0 m a))
    (BinderContent name1 m (Expression name1 m a))
    name0 name1
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

paramInfoTagNames ::
    Functor m =>
    Lens.IndexedSetter T.Tag
    (ParamInfo name0 m)
    (ParamInfo name1 m)
    name0 name1
paramInfoTagNames f (ParamInfo tag actions) =
    ParamInfo
    <$> tagNames f tag
    <*> (fpAddNext . Lens._Just . tagSelectionTagNames) f actions

binderTagNames ::
    Functor m =>
    Lens.IndexedSetter T.Tag
    (Binder name0 m (Expression name0 m a))
    (Binder name1 m (Expression name1 m a))
    name0 name1
binderTagNames f Binder{..} =
    (\_bParams _bBody _bActions -> Binder{..})
    <$> (_Params . traverse . traverse . paramInfoTagNames) f _bParams
    <*> (bbContent . binderContentTagNames) f _bBody
    <*> (baAddFirstParam . _PrependParam . tagSelectionTagNames) f _bActions

definitionTagNames ::
    Functor m =>
    Lens.IndexedSetter T.Tag
    (Definition name0 m (Expression name0 m a))
    (Definition name1 m (Expression name1 m a))
    name0 name1
definitionTagNames f Definition{..} =
    (\_drName _drBody -> Definition{..})
    <$> tagNames f _drName
    <*> (_DefinitionBodyExpression . deContent . binderTagNames) f _drBody

compositeItemTagNames ::
    Functor m =>
    Lens.IndexedSetter T.Tag
    (CompositeItem name0 m (Expression name0 m a))
    (CompositeItem name1 m (Expression name1 m a))
    name0 name1
compositeItemTagNames f CompositeItem{..} =
    (\_ciTag _ciExpr -> CompositeItem{..})
    <$> tagNames f _ciTag
    <*> expressionTagNames f _ciExpr

compositeTagNames ::
    Functor m =>
    Lens.IndexedSetter T.Tag
    (Composite name0 m (Expression name0 m a))
    (Composite name1 m (Expression name1 m a))
    name0 name1
compositeTagNames f (Composite items t addItem) =
    Composite
    <$> (traverse . compositeItemTagNames) f items
    <*> (traverse . expressionTagNames) f t
    <*> tagSelectionTagNames f addItem

-- TODO: Not all names have their T.Tag apparent
-- Instead of the xTagNames traversal we should have the names-pass
-- add a setName which properly registers/unregisters tags.
hackTag :: String -> T.Tag
hackTag msg = error (msg ++ "-HACK: some names aren't actually indexed by tag")

binderVarRefTags ::
    Lens.IndexedTraversal T.Tag
    (BinderVarRef name0 m)
    (BinderVarRef name1 m)
    name0 name1
binderVarRefTags f = (bvNameRef . nrName) (Lens.indexed f (hackTag "binderVarRefTags"))

paramRefTagNames ::
    Lens.IndexedTraversal T.Tag
    (ParamRef name0 m) (ParamRef name1 m) name0 name1
paramRefTagNames f = (pNameRef . nrName) (Lens.indexed f (hackTag "paramRefTagNames"))

annotatedArgTagNames ::
    Functor m =>
    Lens.IndexedSetter T.Tag
    (AnnotatedArg name0 (Expression name0 m a))
    (AnnotatedArg name1 (Expression name1 m a))
    name0 name1
annotatedArgTagNames f AnnotatedArg{..} =
    (\_aaName _aaExpr -> AnnotatedArg{..})
    <$> Lens.indexed f (_aaTag ^. tagVal) _aaName
    <*> expressionTagNames f _aaExpr

labeledApplyTagNames ::
    Functor m =>
    Lens.IndexedSetter T.Tag
    (LabeledApply name0 m (Expression name0 m a))
    (LabeledApply name1 m (Expression name1 m a))
    name0 name1
labeledApplyTagNames f LabeledApply{..} =
    LabeledApply
    <$> binderVarRefTags f _aFunc
    <*> (traverse . expressionTagNames) f _aSpecialArgs
    <*> (traverse . annotatedArgTagNames) f _aAnnotatedArgs
    <*> (traverse . raValue . paramRefTagNames) f _aRelayedArgs

fragmentTagNames ::
    Functor m =>
    Lens.IndexedSetter T.Tag
    (Fragment name0 m (Expression name0 m a))
    (Fragment name1 m (Expression name1 m a))
    name0 name1
fragmentTagNames f Fragment{..} =
    (\_fExpr _fOptions -> Fragment{..})
    <$> expressionTagNames f _fExpr
    <*> (Lens.mapped . traverse . Lens.mapped . expressionTagNames) f _fOptions

nominalTagNames ::
    (Applicative f, Lens.Indexable T.Tag p) =>
    (p name0 (f name1) -> expr0 -> f expr1) ->
    p name0 (f name1) -> Nominal name0 expr0 -> f (Nominal name1 expr1)
nominalTagNames valTagNames f (Nominal tid val) =
    Nominal
    <$> tidName (Lens.indexed f (hackTag "nominalTagNames")) tid
    <*> valTagNames f val

getVarTags :: Lens.IndexedTraversal T.Tag (GetVar name0 m) (GetVar name1 m) name0 name1
getVarTags f (GetParam p) = paramRefTagNames f p <&> GetParam
getVarTags f (GetBinder b) = binderVarRefTags f b <&> GetBinder
getVarTags f (GetParamsRecord r) = traverse (Lens.indexed f (hackTag "GetParamsRecord")) r <&> GetParamsRecord

exprBodyTagNames ::
    Functor m =>
    Lens.IndexedSetter T.Tag
    (Body name0 m (Expression name0 m a))
    (Body name1 m (Expression name1 m a))
    name0 name1
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
exprBodyTagNames f (BodyLabeledApply a) = labeledApplyTagNames f a <&> BodyLabeledApply
exprBodyTagNames f (BodyFragment x) = fragmentTagNames f x <&> BodyFragment
exprBodyTagNames f (BodyToNom n) = nominalTagNames (bbContent . binderContentTagNames) f n <&> BodyToNom
exprBodyTagNames f (BodyFromNom n) = nominalTagNames expressionTagNames f n <&> BodyFromNom
exprBodyTagNames f (BodyGetVar g) = getVarTags f g <&> BodyGetVar
exprBodyTagNames f (BodyHole x) = (Lens.mapped . expressionTagNames) f x <&> BodyHole
exprBodyTagNames f (BodySimpleApply x) = (traverse . expressionTagNames) f x <&> BodySimpleApply
exprBodyTagNames f (BodyIfElse x) = (traverse . expressionTagNames) f x <&> BodyIfElse
exprBodyTagNames _ (BodyLiteral x) = BodyLiteral x & pure
exprBodyTagNames _ BodyPlaceHolder = pure BodyPlaceHolder

expressionTagNames ::
    Functor m =>
    Lens.IndexedSetter T.Tag
    (Expression name0 m a)
    (Expression name1 m a)
    name0 name1
expressionTagNames = rBody . exprBodyTagNames

-- TODO: This traversal and with all its helper sub-traversals is somewhat tedious
-- and bug-prone. Perhaps some more generic SYB solution is suitable instead.
workAreaTagNames ::
    Functor m =>
    Lens.IndexedSetter T.Tag
    (WorkArea name0 m a) (WorkArea name1 m a) name0 name1
workAreaTagNames f (WorkArea panes repl) =
    WorkArea
    <$> (traverse . paneDefinition . definitionTagNames) f panes
    <*> expressionTagNames f repl
