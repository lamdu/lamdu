{-# LANGUAGE FlexibleContexts, TypeFamilies, RecordWildCards, NamedFieldPuns, TemplateHaskell #-}
module Lamdu.Sugar.Names.Walk
    ( MonadNaming(..)
    , NameType(..), _GlobalDef, _TaggedVar, _TaggedNominal, _Tag
    , isLocal, isGlobal
    , FunctionSignature(..), Disambiguator
    , NameConvertor, CPSNameConvertor
    , toWorkArea, toDef, toExpression, toBody
    ) where

import qualified Control.Lens as Lens
import qualified Data.Set as Set
import           Data.Tree.Diverse (Node, Ann(..))
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.CPS (CPS(..), liftCPS)
import qualified Lamdu.Sugar.Types as Sugar
import           Lamdu.Sugar.Types hiding (Tag)

import           Lamdu.Prelude

-- TODO: Maybe remove "TaggedNominal", make it a disambiguation context?
data NameType = GlobalDef | TaggedVar | TaggedNominal | Tag | TypeVar
    deriving (Eq, Ord, Show)

Lens.makePrisms ''NameType

-- | Bound by a local binder. Used to determine which locals collide
isLocal :: NameType -> Bool
isLocal TaggedVar = True
isLocal _ = False

-- | Not bound by a local binder and not a tag
--
-- A context-less tag is not considered a global, because it is not an
-- actual entity. Used to determine collisions of binder-less names
isGlobal :: NameType -> Bool
isGlobal GlobalDef = True
isGlobal TaggedNominal = True
isGlobal _ = False

type CPSNameConvertor m = OldName m -> CPS m (NewName m)
type NameConvertor m = OldName m -> m (NewName m)

data FunctionSignature = FunctionSignature
    { sSpecialArgs :: SpecialArgs ()
    , sNormalArgs :: Set T.Tag
    } deriving (Eq, Ord, Show)

type Disambiguator = FunctionSignature

-- TODO: Rename MonadNameWalk
class (Monad m, Monad (IM m)) => MonadNaming m where
    type OldName m
    type NewName m
    type IM m :: * -> *
    opRun :: m (m a -> IM m a)

    opWithName :: Sugar.VarInfo -> NameType -> CPSNameConvertor m
    opGetName :: Maybe Disambiguator -> NameType -> NameConvertor m

toParamRef ::
    MonadNaming m =>
    ParamRef (OldName m) o ->
    m (ParamRef (NewName m) o)
toParamRef = (pNameRef . nrName) (opGetName Nothing TaggedVar)

binderVarType :: BinderVarForm name m -> NameType
binderVarType GetLet = TaggedVar
binderVarType (GetDefinition _) = GlobalDef

toCompositeFields ::
    MonadNaming m =>
    CompositeFields p (OldName m) (Type (OldName m)) ->
    m (CompositeFields p (NewName m) (Type (NewName m)))
toCompositeFields (CompositeFields fields mExt) =
    CompositeFields
    <$> traverse toField fields
    <*> Lens._Just (opGetName Nothing TypeVar) mExt
    where
        toField (tag, typ) = (,) <$> toTagInfoOf Tag tag <*> toType typ

toTId :: MonadNaming m => TId (OldName m) -> m (TId (NewName m))
toTId = tidName %%~ opGetName Nothing TaggedNominal

toTBody ::
    MonadNaming m =>
    TBody (OldName m) (Type (OldName m)) ->
    m (TBody (NewName m) (Type (NewName m)))
toTBody (TVar tv) = opGetName Nothing TypeVar tv <&> TVar
toTBody (TFun a b) = TFun <$> toType a <*> toType b
toTBody (TInst tid params) = TInst <$> toTId tid <*> traverse toType params
toTBody (TRecord composite) = TRecord <$> toCompositeFields composite
toTBody (TVariant composite) = TVariant <$> toCompositeFields composite

toType :: MonadNaming m => Type (OldName m) -> m (Type (NewName m))
toType = tBody %%~ toTBody

toScheme :: MonadNaming m => Scheme (OldName m) -> m (Scheme (NewName m))
toScheme (Scheme tvs cs typ) = Scheme tvs cs <$> toType typ

toDefinitionOutdatedType ::
    MonadNaming m =>
    DefinitionOutdatedType (OldName m) p ->
    m (DefinitionOutdatedType (NewName m) p)
toDefinitionOutdatedType (DefinitionOutdatedType whenUsed current useCur) =
    DefinitionOutdatedType <$> toScheme whenUsed <*> toScheme current ?? useCur

toBinderVarRef ::
    MonadNaming m =>
    Maybe Disambiguator ->
    BinderVarRef (OldName m) o ->
    m (BinderVarRef (NewName m) o)
toBinderVarRef mDisambig (BinderVarRef nameRef form var inline) =
    BinderVarRef
    <$> ( nrName %%~
          opGetName mDisambig (binderVarType form)
        ) nameRef
    <*> (_GetDefinition . _DefTypeChanged %%~ toDefinitionOutdatedType) form
    ?? var
    ?? inline

toGetVar ::
    MonadNaming m =>
    GetVar (OldName m) o ->
    m (GetVar (NewName m) o)
toGetVar (GetParam x) = toParamRef x <&> GetParam
toGetVar (GetBinder x) = toBinderVarRef Nothing x <&> GetBinder
toGetVar (GetParamsRecord x) =
    traverse (opGetName Nothing Tag) x <&> GetParamsRecord

toNodeActions ::
    MonadNaming m =>
    NodeActions (OldName m) (IM m) o ->
    m (NodeActions (NewName m) (IM m) o)
toNodeActions = wrapInRecord toTagSelection

toResRecord ::
    MonadNaming m =>
    ResRecord (OldName m) a -> m (ResRecord (NewName m) a)
toResRecord = recordFields . traverse . _1 %%~ toTagInfoOf Tag

toResBody ::
    MonadNaming m =>
    (a -> m b) -> ResBody (OldName m) a -> m (ResBody (NewName m) b)
toResBody f =
    \case
    RFunc    x -> RFunc x & pure
    RError   x -> RError x & pure
    RBytes   x -> RBytes x & pure
    RFloat   x -> RFloat x & pure
    RText    x -> RText x & pure
    RArray   x -> RArray x & pure
    RStream  x -> RStream x & pure
    RTree    x -> RTree x & pure
    RTable   x -> (rtHeaders . traverse) (toTagInfoOf Tag) x <&> RTable
    RRecord  x -> toResRecord x <&> RRecord
    RInject  x -> riTag (toTagInfoOf Tag) x <&> RInject
    <&> (>>= traverse f)

toResVal :: MonadNaming m => ResVal (OldName m) -> m (ResVal (NewName m))
toResVal = resBody (toResBody toResVal)

toValAnnotation :: MonadNaming m => ValAnnotation (OldName m) (IM m) -> m (ValAnnotation (NewName m) (IM m))
toValAnnotation (ValAnnotation evalRes typ) =
    do
        run <- opRun
        Lens._Just toType typ
            <&>
            ValAnnotation
            (evalRes <&> traverse . traverse %~ (>>= run . toResVal))

toAnnotation :: MonadNaming m => Annotation (OldName m) (IM m) -> m (Annotation (NewName m) (IM m))
toAnnotation AnnotationNone = pure AnnotationNone
toAnnotation (AnnotationType typ) = toType typ <&> AnnotationType
toAnnotation (AnnotationVal x) = toValAnnotation x <&> AnnotationVal

toParentNode ::
    MonadNaming m =>
    (a (Ann (Payload (OldName m) (IM m) o p)) ->
        m (b (Ann (Payload (NewName m) (IM m) o p)))) ->
    Node (Ann (Payload (OldName m) (IM m) o p)) a ->
    m (Node (Ann (Payload (NewName m) (IM m) o p)) b)
toParentNode = toNode

toLet ::
    MonadNaming m =>
    Let (OldName m) (IM m) o (Ann (Payload (OldName m) (IM m) o a)) ->
    m (Let (NewName m) (IM m) o (Ann (Payload (NewName m) (IM m) o a)))
toLet Let{..} =
    do
        (_lName, _lBody) <-
            unCPS (withTag TaggedVar _lVarInfo _lName)
            (toParentNode toBinder _lBody)
        _lValue <- toAssignment _lValue
        pure Let{..}

toBinder ::
    MonadNaming m =>
    Binder (OldName m) (IM m) o (Ann (Payload (OldName m) (IM m) o a)) ->
    m (Binder (NewName m) (IM m) o (Ann (Payload (NewName m) (IM m) o a)))
toBinder (BinderLet l) = toLet l <&> BinderLet
toBinder (BinderExpr e) = toBody e <&> BinderExpr

toAddFirstParam ::
    MonadNaming m =>
    AddFirstParam (OldName m) (IM m) o ->
    m (AddFirstParam (NewName m) (IM m) o)
toAddFirstParam = _PrependParam toTagSelection

toFunction ::
    MonadNaming m =>
    Function (OldName m) (IM m) o (Ann (Payload (OldName m) (IM m) o a)) ->
    m (Function (NewName m) (IM m) o (Ann (Payload (NewName m) (IM m) o a)))
toFunction Function{..} =
    (\(_fParams, _fBody) _fAddFirstParam -> Function{..})
    <$> unCPS (withBinderParams _fParams) (toParentNode toBinder _fBody)
    <*> toAddFirstParam _fAddFirstParam

toBinderPlain ::
    MonadNaming m =>
    AssignPlain (OldName m) (IM m) o (Ann (Payload (OldName m) (IM m) o a)) ->
    m (AssignPlain (NewName m) (IM m) o (Ann (Payload (NewName m) (IM m) o a)))
toBinderPlain AssignPlain{..} =
    (\_apBody _apAddFirstParam -> AssignPlain{..})
    <$> toBinder _apBody
    <*> toAddFirstParam _apAddFirstParam

toAssignmentBody ::
    MonadNaming m =>
    AssignmentBody (OldName m) (IM m) o (Ann (Payload (OldName m) (IM m) o a)) ->
    m (AssignmentBody (NewName m) (IM m) o (Ann (Payload (NewName m) (IM m) o a)))
toAssignmentBody (BodyPlain x) = toBinderPlain x <&> BodyPlain
toAssignmentBody (BodyFunction x) = toFunction x <&> BodyFunction

toAssignment ::
    MonadNaming m =>
    Assignment (OldName m) (IM m) o (Payload (OldName m) (IM m) o a) ->
    m (Assignment (NewName m) (IM m) o (Payload (NewName m) (IM m) o a))
toAssignment = toParentNode toAssignmentBody


toLam ::
    MonadNaming m =>
    Lambda (OldName m) (IM m) o (Ann (Payload (OldName m) (IM m) o a)) ->
    m (Lambda (NewName m) (IM m) o (Ann (Payload (NewName m) (IM m) o a)))
toLam = lamFunc toFunction

toTagInfoOf ::
    MonadNaming m => NameType -> TagInfo (OldName m) -> m (TagInfo (NewName m))
toTagInfoOf nameType = tagName (opGetName Nothing nameType)

toTagSelection ::
    MonadNaming m =>
    TagSelection (OldName m) (IM m) o a ->
    m (TagSelection (NewName m) (IM m) o a)
toTagSelection t@TagSelection{..} =
    opRun
    <&>
    \run -> t & tsOptions %~ (>>= run . (traverse . toInfo) (toTagInfoOf Tag))

toTagOf ::
    MonadNaming m =>
    NameType -> Sugar.Tag (OldName m) (IM m) o ->
    m (Sugar.Tag (NewName m) (IM m) o)
toTagOf nameType (Sugar.Tag info actions) =
    Sugar.Tag
    <$> toTagInfoOf nameType info
    <*> toTagSelection actions

withTag ::
    MonadNaming m =>
    NameType -> Sugar.VarInfo ->
    Sugar.Tag (OldName m) (IM m) o ->
    CPS m (Sugar.Tag (NewName m) (IM m) o)
withTag nameType varInfo (Sugar.Tag info actions) =
    Sugar.Tag
    <$> tagName (opWithName varInfo nameType) info
    <*> liftCPS (toTagSelection actions)

toNode ::
    MonadNaming m =>
    (a -> m b) ->
    Ann (Payload (OldName m) (IM m) o p) a ->
    m (Ann (Payload (NewName m) (IM m) o p) b)
toNode toV (Ann pl v) =
    Ann
    <$> toPayload pl
    <*> toV v

toAnnotatedArg ::
    MonadNaming m =>
    (a -> m b) ->
    AnnotatedArg (OldName m) a ->
    m (AnnotatedArg (NewName m) b)
toAnnotatedArg expr (AnnotatedArg tag e) =
    AnnotatedArg
    <$> toTagInfoOf Tag tag
    <*> expr e

toLabeledApply ::
    MonadNaming m =>
    LabeledApply (OldName m) (IM m) o (Ann (Payload (OldName m) (IM m) o a)) ->
    m (LabeledApply (NewName m) (IM m) o (Ann (Payload (NewName m) (IM m) o a)))
toLabeledApply app@LabeledApply{..} =
    LabeledApply
    <$> toNode (toBinderVarRef (Just (funcSignature app))) _aFunc
    <*> traverse toExpression _aSpecialArgs
    <*> traverse (toAnnotatedArg toExpression) _aAnnotatedArgs
    <*> traverse (toNode toGetVar) _aRelayedArgs

toHole ::
    MonadNaming m =>
    Hole (OldName m) (IM m) o ->
    m (Hole (NewName m) (IM m) o)
toHole hole =
    opRun
    <&>
    \run ->
    SugarLens.holeTransformExprs (run . toParentNode toBinder) hole

toFragment ::
    MonadNaming m =>
    Fragment (OldName m) (IM m) o (Ann (Payload (OldName m) (IM m) o a)) ->
    m (Fragment (NewName m) (IM m) o (Ann (Payload (NewName m) (IM m) o a)))
toFragment Fragment{..} =
    do
        run <- opRun
        newExpr <- toExpression _fExpr
        pure Fragment
            { _fExpr = newExpr
            , _fHeal = _fHeal
            , _fOptions =
                 _fOptions
                 <&> Lens.mapped %~
                     SugarLens.holeOptionTransformExprs
                     (run . toParentNode toBinder)
            }

toComposite ::
    MonadNaming m =>
    (a -> m b) ->
    Composite (OldName m) (IM m) o a ->
    m (Composite (NewName m) (IM m) o b)
toComposite expr Composite{..} =
    (\_cItems _cAddItem -> Composite{..})
    <$> (traverse . ciTag) (toTagOf Tag) _cItems
    <*> toTagSelection _cAddItem
    >>= traverse expr

toCase ::
    MonadNaming m =>
    (a -> m b) ->
    Case (OldName m) (IM m) o a ->
    m (Case (NewName m) (IM m) o b)
toCase expr (Case k c) = Case <$> traverse expr k <*> toComposite expr c

toInjectVal ::
    MonadNaming m =>
    InjectContent (OldName m) (IM m) o (Ann (Payload (OldName m) (IM m) o a)) ->
    m (InjectContent (NewName m) (IM m) o (Ann (Payload (NewName m) (IM m) o a)))
toInjectVal (InjectVal v) = toExpression v <&> InjectVal
toInjectVal (InjectNullary n) = toNode (nullaryAddItem toTagSelection) n <&> InjectNullary

toInject ::
    MonadNaming m =>
    Inject (OldName m) (IM m) o (Ann (Payload (OldName m) (IM m) o a)) ->
    m (Inject (NewName m) (IM m) o (Ann (Payload (NewName m) (IM m) o a)))
toInject (Inject t v) =
    Inject <$> toTagOf Tag t <*> toInjectVal v

toElse ::
    MonadNaming m =>
    Else (OldName m) (IM m) o (Ann (Payload (OldName m) (IM m) o a)) ->
    m (Else (NewName m) (IM m) o (Ann (Payload (NewName m) (IM m) o a)))
toElse (SimpleElse x) = toBody x <&> SimpleElse
toElse (ElseIf x) = eiContent toIfElse x <&> ElseIf

toIfElse ::
    MonadNaming m =>
    IfElse (OldName m) (IM m) o (Ann (Payload (OldName m) (IM m) o a)) ->
    m (IfElse (NewName m) (IM m) o (Ann (Payload (NewName m) (IM m) o a)))
toIfElse (IfElse i t e) =
    IfElse
    <$> toExpression i
    <*> toExpression t
    <*> toParentNode toElse e

toBody ::
    MonadNaming m =>
    Body (OldName m) (IM m) o (Ann (Payload (OldName m) (IM m) o a)) ->
    m (Body (NewName m) (IM m) o (Ann (Payload (NewName m) (IM m) o a)))
toBody =
    \case
    BodyGetField     x -> x & traverse toExpression >>= gfTag toTag <&> BodyGetField
    BodyInject       x -> x & toInject <&> BodyInject
    BodyRecord       x -> x & toComposite toExpression <&> BodyRecord
    BodyCase         x -> x & toCase toExpression <&> BodyCase
    BodyIfElse       x -> x & toIfElse <&> BodyIfElse
    BodySimpleApply  x -> x & traverse toExpression <&> BodySimpleApply
    BodyLabeledApply x -> x & toLabeledApply <&> BodyLabeledApply
    BodyHole         x -> x & toHole <&> BodyHole
    BodyFromNom      x -> x & traverse toExpression >>= nTId toTId <&> BodyFromNom
    BodyToNom        x -> x & traverse (toNode toBinder) >>= nTId toTId <&> BodyToNom
    BodyGetVar       x -> x & toGetVar <&> BodyGetVar
    BodyLiteral      x -> x & BodyLiteral & pure
    BodyLam          x -> x & toLam <&> BodyLam
    BodyFragment     x -> x & toFragment <&> BodyFragment
    BodyPlaceHolder    -> pure BodyPlaceHolder
    where
        toTag = toTagOf Tag

funcSignature :: LabeledApply name i o a -> FunctionSignature
funcSignature apply =
    FunctionSignature
    { sSpecialArgs = apply ^. aSpecialArgs & void
    , sNormalArgs = apply ^.. aAnnotatedArgs . traverse . aaTag . tagVal & Set.fromList
    }

toPayload ::
    MonadNaming m =>
    Payload (OldName m) (IM m) o a ->
    m (Payload (NewName m) (IM m) o a)
toPayload Payload{..} =
    do
        _plAnnotation <- toAnnotation _plAnnotation
        _plActions <- toNodeActions _plActions
        pure Payload{..}

toExpression ::
    MonadNaming m =>
    Expression (OldName m) (IM m) o (Payload (OldName m) (IM m) o a) ->
    m (Expression (NewName m) (IM m) o (Payload (NewName m) (IM m) o a))
toExpression = toParentNode toBody

withParamInfo ::
    MonadNaming m =>
    Sugar.VarInfo -> ParamInfo (OldName m) (IM m) o ->
    CPS m (ParamInfo (NewName m) (IM m) o)
withParamInfo varInfo (ParamInfo tag fpActions) =
    ParamInfo
    <$> withTag TaggedVar varInfo tag
    <*> liftCPS ((fpAddNext . Sugar._AddNext) toTagSelection fpActions)

withFuncParam ::
    MonadNaming m =>
    (Sugar.VarInfo -> a -> CPS m b) -> FuncParam (OldName m) (IM m) a ->
    CPS m (FuncParam (NewName m) (IM m) b)
withFuncParam f (FuncParam pl varInfo info) =
    FuncParam
    <$> liftCPS (toAnnotation pl)
    <*> pure varInfo
    <*> f varInfo info

withBinderParams ::
    MonadNaming m =>
    BinderParams (OldName m) (IM m) o ->
    CPS m (BinderParams (NewName m) (IM m) o)
withBinderParams (NullParam x) = withFuncParam (const pure) x <&> NullParam
withBinderParams (Params xs) = traverse (withFuncParam withParamInfo) xs <&> Params

toDefExpr ::
    MonadNaming m =>
    DefinitionExpression (OldName m) (IM m) o (Payload (OldName m) (IM m) o a) ->
    m (DefinitionExpression (NewName m) (IM m) o (Payload (NewName m) (IM m) o a))
toDefExpr (DefinitionExpression typ presMode content) =
    DefinitionExpression
    <$> toScheme typ
    <*> pure presMode
    <*> toAssignment content

toDefinitionBody ::
    MonadNaming m =>
    DefinitionBody (OldName m) (IM m) o (Payload (OldName m) (IM m) o a) ->
    m (DefinitionBody (NewName m) (IM m) o (Payload (NewName m) (IM m) o a))
toDefinitionBody (DefinitionBodyBuiltin bi) =
    bi & biType %%~ toScheme <&> DefinitionBodyBuiltin
toDefinitionBody (DefinitionBodyExpression expr) =
    toDefExpr expr <&> DefinitionBodyExpression

toDef ::
    MonadNaming m =>
    Definition (OldName m) (IM m) o (Payload (OldName m) (IM m) o a) ->
    m (Definition (NewName m) (IM m) o (Payload (NewName m) (IM m) o a))
toDef Definition{..} =
    do
        -- NOTE: A global def binding is not considered a binder, as
        -- it exists everywhere, not just inside the binding
        _drName <- toTagOf GlobalDef _drName
        _drBody <- toDefinitionBody _drBody
        pure Definition{..}

toPane ::
    MonadNaming m =>
    Pane (OldName m) (IM m) o (Payload (OldName m) (IM m) o a) ->
    m (Pane (NewName m) (IM m) o (Payload (NewName m) (IM m) o a))
toPane = paneDefinition toDef

toRepl ::
    MonadNaming m =>
    Repl (OldName m) (IM m) o (Payload (OldName m) (IM m) o a) ->
    m (Repl (NewName m) (IM m) o (Payload (NewName m) (IM m) o a))
toRepl (Repl bod varInfo res) =
    Repl
    <$> toParentNode toBinder bod
    <*> pure varInfo
    <*> (traverse . Lens._Just . _EvalSuccess) toResVal res

toWorkArea ::
    MonadNaming m =>
    WorkArea (OldName m) (IM m) o (Payload (OldName m) (IM m) o a) ->
    m (WorkArea (NewName m) (IM m) o (Payload (NewName m) (IM m) o a))
toWorkArea WorkArea { _waPanes, _waRepl, _waGlobals } =
    do
        run <- opRun
        panes <- traverse toPane _waPanes
        repl <- toRepl _waRepl
        let globals = _waGlobals >>= run . toGlobals
        WorkArea panes repl globals & pure
    where
        toGlobals = (traverse . nrName) (opGetName Nothing GlobalDef)
