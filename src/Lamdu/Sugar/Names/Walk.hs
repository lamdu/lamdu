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
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.CPS (CPS(..), liftCPS)
import qualified Lamdu.Sugar.Names.NameGen as NameGen
import qualified Lamdu.Sugar.Types as Sugar
import           Lamdu.Sugar.Types hiding (Tag)
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

-- TODO: Maybe remove "TaggedNominal", make it a disambiguation context?
data NameType = GlobalDef | TaggedVar | TaggedNominal | Tag
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
class (Monad m, Monad (SM m)) => MonadNaming m where
    type OldName m
    type NewName m
    type SM m :: * -> *
    opRun :: m (m a -> T (SM m) a)

    opWithName :: NameGen.VarInfo -> NameType -> CPSNameConvertor m
    opGetName :: Maybe Disambiguator -> NameType -> NameConvertor m

type TM m = T (SM m)

isFunctionType :: Sugar.Type name -> NameGen.VarInfo
isFunctionType typ =
    case typ ^. tBody of
    Sugar.TFun {} -> NameGen.Function
    _ -> NameGen.NormalVar

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
toCompositeFields =
    compositeFields . traverse %%~ toField
    where
        toField (tag, typ) = (,) <$> toTagInfoOf Tag tag <*> toType typ

toTId :: MonadNaming m => TId (OldName m) -> m (TId (NewName m))
toTId = tidName %%~ opGetName Nothing TaggedNominal

toTBody ::
    MonadNaming m =>
    TBody (OldName m) (Type (OldName m)) ->
    m (TBody (NewName m) (Type (NewName m)))
toTBody (TVar tv) = TVar tv & pure
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
    NodeActions (OldName m) (TM m) o ->
    m (NodeActions (NewName m) (TM m) o)
toNodeActions = wrapInRecord toTagSelection

toResRecord ::
    MonadNaming m =>
    ResRecord (OldName m) a -> m (ResRecord (NewName m) a)
toResRecord = recordFields . traverse . _1 %%~ toTagInfoOf Tag

toResBody ::
    MonadNaming m =>
    (a -> m b) -> ResBody (OldName m) a -> m (ResBody (NewName m) b)
toResBody f body =
    case body of
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
    >>= traverse f

toResVal :: MonadNaming m => ResVal (OldName m) -> m (ResVal (NewName m))
toResVal = resBody (toResBody toResVal)

toAnnotation :: MonadNaming m => Annotation (OldName m) -> m (Annotation (NewName m))
toAnnotation (Annotation typ evalRes) =
    Annotation
    <$> toType typ
    <*> (traverse . traverse . traverse) toResVal evalRes

toLet ::
    MonadNaming m => (a -> m b) ->
    Let (OldName m) (TM m) o a ->
    m (Let (NewName m) (TM m) o b)
toLet expr Let{..} =
    do
        (_lName, _lBody) <-
            unCPS (withTag TaggedVar (isFunctionType (_lAnnotation ^. aInferredType)) _lName)
            (toBinderBody expr _lBody)
        _lAnnotation <- toAnnotation _lAnnotation
        _lValue <- toBinder expr _lValue
        _lActions <- laNodeActions toNodeActions _lActions
        pure Let{..}

toBinderContent ::
    MonadNaming m => (a -> m b) ->
    BinderContent (OldName m) (TM m) o a ->
    m (BinderContent (NewName m) (TM m) o b)
toBinderContent expr (BinderLet l) = toLet expr l <&> BinderLet
toBinderContent expr (BinderExpr e) = expr e <&> BinderExpr

toBinderBody ::
    MonadNaming m => (a -> m b) ->
    BinderBody (OldName m) (TM m) o a ->
    m (BinderBody (NewName m) (TM m) o b)
toBinderBody expr = bbContent %%~ toBinderContent expr

toBinderActions ::
    MonadNaming m =>
    BinderActions (OldName m) (TM m) o ->
    m (BinderActions (NewName m) (TM m) o)
toBinderActions BinderActions{..} =
    BinderActions
    <$> _PrependParam toTagSelection _baAddFirstParam
    <*> Lens._Just toNodeActions _baMNodeActions

toBinder ::
    MonadNaming m => (a -> m b) ->
    Binder (OldName m) (TM m) o a ->
    m (Binder (NewName m) (TM m) o b)
toBinder expr Binder{..} =
    (\(_bParams, _bBody) _bActions -> Binder{..})
    <$> unCPS (withBinderParams _bParams) (toBinderBody expr _bBody)
    <*> toBinderActions _bActions

toLam ::
    MonadNaming m => (a -> m b) ->
    Lambda (OldName m) (TM m) o a ->
    m (Lambda (NewName m) (TM m) o b)
toLam = lamBinder . toBinder

toTagInfoOf ::
    MonadNaming m => NameType -> TagInfo (OldName m) -> m (TagInfo (NewName m))
toTagInfoOf nameType = tagName (opGetName Nothing nameType)

withTagInfoOf ::
    MonadNaming m =>
    NameType -> NameGen.VarInfo ->
    TagInfo (OldName m) -> CPS m (TagInfo (NewName m))
withTagInfoOf nameType varInfo = tagName (opWithName varInfo nameType)

toTagSelection ::
    MonadNaming m =>
    TagSelection (OldName m) (TM m) o a ->
    m (TagSelection (NewName m) (TM m) o a)
toTagSelection t@TagSelection{..} =
    opRun
    <&>
    \run -> t & tsOptions %~ (>>= run . (traverse . toInfo) (toTagInfoOf Tag))

toTagOf ::
    MonadNaming m =>
    NameType -> Sugar.Tag (OldName m) (TM m) o ->
    m (Sugar.Tag (NewName m) (TM m) o)
toTagOf nameType (Sugar.Tag info actions) =
    Sugar.Tag
    <$> toTagInfoOf nameType info
    <*> toTagSelection actions

withTag ::
    MonadNaming m =>
    NameType -> NameGen.VarInfo ->
    Sugar.Tag (OldName m) (TM m) o ->
    CPS m (Sugar.Tag (NewName m) (TM m) o)
withTag nameType varInfo (Sugar.Tag info actions) =
    Sugar.Tag
    <$> withTagInfoOf nameType varInfo info
    <*> liftCPS (toTagSelection actions)

toRelayedArg ::
    MonadNaming m =>
    RelayedArg (OldName m) (TM m) o ->
    m (RelayedArg (NewName m) (TM m) o)
toRelayedArg RelayedArg{..} =
    (\_raValue _raActions -> RelayedArg{..})
    <$> toGetVar _raValue
    <*> toNodeActions _raActions

toLabeledApply ::
    MonadNaming m =>
    (a -> m b) ->
    LabeledApply (OldName m) (TM m) o a ->
    m (LabeledApply (NewName m) (TM m) o b)
toLabeledApply expr app@LabeledApply{..} =
    LabeledApply
    <$> toBinderVarRef (Just (funcSignature app)) _aFunc
    <*> pure _aSpecialArgs
    <*> (traverse . aaTag) (toTagInfoOf Tag) _aAnnotatedArgs
    <*> traverse toRelayedArg _aRelayedArgs
    >>= traverse expr

toHole ::
    MonadNaming m =>
    Hole (TM m) o (Expression (OldName m) (TM m) o ()) ->
    m (Hole (TM m) o (Expression (NewName m) (TM m) o ()))
toHole hole =
    opRun
    <&>
    \run ->
    SugarLens.holeTransformExprs (run . toExpression) hole

toFragment ::
    MonadNaming m =>
    (a -> m b) ->
    Fragment (OldName m) (TM m) o a ->
    m (Fragment (NewName m) (TM m) o b)
toFragment expr Fragment{..} =
    do
        run <- opRun
        newExpr <- expr _fExpr
        pure Fragment
            { _fExpr = newExpr
            , _fAttach = _fAttach
            , _fOptions = _fOptions <&> Lens.mapped %~ SugarLens.holeOptionTransformExprs (run . toExpression)
            }

toComposite ::
    MonadNaming m =>
    (a -> m b) ->
    Composite (OldName m) (TM m) o a ->
    m (Composite (NewName m) (TM m) o b)
toComposite expr Composite{..} =
    (\_cItems _cAddItem -> Composite{..})
    <$> (traverse . ciTag) (toTagOf Tag) _cItems
    <*> toTagSelection _cAddItem
    >>= traverse expr

toCase ::
    MonadNaming m =>
    (a -> m b) ->
    Case (OldName m) (TM m) o a ->
    m (Case (NewName m) (TM m) o b)
toCase expr (Case k c) = Case <$> traverse expr k <*> toComposite expr c

toElseIfContent ::
    MonadNaming m =>
    (a -> m b) ->
    ElseIfContent (OldName m) (TM m) o a ->
    m (ElseIfContent (NewName m) (TM m) o b)
toElseIfContent expr ElseIfContent{..} =
    (\_eiContent _eiNodeActions -> ElseIfContent{..})
    <$> toIfElse expr _eiContent
    <*> toNodeActions _eiNodeActions

toElse ::
    MonadNaming m =>
    (a -> m b) ->
    Else (OldName m) (TM m) o a ->
    m (Else (NewName m) (TM m) o b)
toElse expr (SimpleElse x) = expr x <&> SimpleElse
toElse expr (ElseIf x) = toElseIfContent expr x <&> ElseIf

toIfElse ::
    MonadNaming m =>
    (a -> m b) ->
    IfElse (OldName m) (TM m) o a ->
    m (IfElse (NewName m) (TM m) o b)
toIfElse expr (IfElse ifThen els_) =
    IfElse
    <$> traverse expr ifThen
    <*> toElse expr els_

toBody ::
    MonadNaming m => (a -> m b) ->
    Body (OldName m) (TM m) o a ->
    m (Body (NewName m) (TM m) o b)
toBody expr = \case
    BodyGetField     x -> x & traverse expr >>= gfTag toTag <&> BodyGetField
    BodyInject       x -> x & traverse expr >>= iTag toTag <&> BodyInject
    BodyRecord       x -> x & toComposite expr <&> BodyRecord
    BodyCase         x -> x & toCase expr <&> BodyCase
    BodyIfElse       x -> x & toIfElse expr <&> BodyIfElse
    BodySimpleApply  x -> x & traverse expr <&> BodySimpleApply
    BodyLabeledApply x -> x & toLabeledApply expr <&> BodyLabeledApply
    BodyHole         x -> x & toHole <&> BodyHole
    BodyFromNom      x -> x & traverse expr >>= nTId toTId <&> BodyFromNom
    BodyToNom        x -> x & traverse (toBinderBody expr) >>= nTId toTId <&> BodyToNom
    BodyGetVar       x -> x & toGetVar <&> BodyGetVar
    BodyLiteral      x -> x & BodyLiteral & pure
    BodyLam          x -> x & toLam expr <&> BodyLam
    BodyFragment     x -> x & toFragment expr <&> BodyFragment
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
    MonadNaming m => Payload (OldName m) (TM m) o a ->
    m (Payload (NewName m) (TM m) o a)
toPayload Payload{..} =
    do
        _plAnnotation <- toAnnotation _plAnnotation
        _plActions <- toNodeActions _plActions
        pure Payload{..}

toExpression ::
    MonadNaming m =>
    Expression (OldName m) (TM m) o a ->
    m (Expression (NewName m) (TM m) o a)
toExpression (Expression body pl) =
    Expression
    <$> toBody toExpression body
    <*> toPayload pl

withParamInfo ::
    MonadNaming m =>
    NameGen.VarInfo -> ParamInfo (OldName m) (TM m) o ->
    CPS m (ParamInfo (NewName m) (TM m) o)
withParamInfo varInfo (ParamInfo tag fpActions) =
    ParamInfo
    <$> withTag TaggedVar varInfo tag
    <*> liftCPS ((fpAddNext . Sugar._AddNext) toTagSelection fpActions)

withFuncParam ::
    MonadNaming m =>
    (NameGen.VarInfo -> a -> CPS m b) -> FuncParam (OldName m) a ->
    CPS m (FuncParam (NewName m) b)
withFuncParam f (FuncParam ann info) =
    FuncParam
    <$> liftCPS (toAnnotation ann)
    <*> f varInfo info
    where
        varInfo = ann ^. aInferredType & isFunctionType

withBinderParams ::
    MonadNaming m =>
    BinderParams (OldName m) (TM m) o ->
    CPS m (BinderParams (NewName m) (TM m) o)
withBinderParams BinderWithoutParams = pure BinderWithoutParams
withBinderParams (NullParam x) = withFuncParam (const pure) x <&> NullParam
withBinderParams (Params xs) = traverse (withFuncParam withParamInfo) xs <&> Params

toDefExpr ::
    MonadNaming m => (a -> m b) ->
    DefinitionExpression (OldName m) (TM m) o a ->
    m (DefinitionExpression (NewName m) (TM m) o b)
toDefExpr f (DefinitionExpression typ presMode content) =
    DefinitionExpression
    <$> toScheme typ
    <*> pure presMode
    <*> toBinder f content

toDefinitionBody ::
    MonadNaming m => (a -> m b) ->
    DefinitionBody (OldName m) (TM m) o a ->
    m (DefinitionBody (NewName m) (TM m) o b)
toDefinitionBody _ (DefinitionBodyBuiltin bi) =
    bi & biType %%~ toScheme <&> DefinitionBodyBuiltin
toDefinitionBody f (DefinitionBodyExpression expr) =
    toDefExpr f expr <&> DefinitionBodyExpression

toDef ::
    MonadNaming m => (a -> m b) ->
    Definition (OldName m) (TM m) o a ->
    m (Definition (NewName m) (TM m) o b)
toDef f Definition{..} =
    do
        -- NOTE: A global def binding is not considered a binder, as
        -- it exists everywhere, not just inside the binding
        _drName <- toTagOf GlobalDef _drName
        _drBody <- toDefinitionBody f _drBody
        pure Definition{..}

toPane ::
    MonadNaming m =>
    Pane (OldName m) (TM m) o a ->
    m (Pane (NewName m) (TM m) o a)
toPane = paneDefinition (toDef toExpression)

toWorkArea ::
    MonadNaming m =>
    WorkArea (OldName m) (TM m) o a ->
    m (WorkArea (NewName m) (TM m) o a)
toWorkArea WorkArea { _waPanes, _waRepl } =
    WorkArea
    <$> traverse toPane _waPanes
    <*> toExpression _waRepl
