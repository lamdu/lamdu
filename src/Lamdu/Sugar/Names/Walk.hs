{-# LANGUAGE TypeFamilies, NamedFieldPuns, TemplateHaskell #-}
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
import           Hyper.Type.AST.App (appChildren)
import           Hyper.Type.AST.FuncType (FuncType(..))
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.CPS (CPS(..), liftCPS)
import qualified Lamdu.Sugar.Types as Sugar
import           Lamdu.Sugar.Types hiding (Tag(..))

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
    CompositeFields (OldName m) (Annotated a # Type (OldName m)) ->
    m (CompositeFields (NewName m) (Annotated a # Type (NewName m)))
toCompositeFields (CompositeFields fields mExt) =
    CompositeFields
    <$> traverse toField fields
    <*> Lens._Just (opGetName Nothing TypeVar) mExt
    where
        toField (tag, typ) = (,) <$> toTagOf Tag tag <*> toType typ

toTId :: MonadNaming m => TId (OldName m) -> m (TId (NewName m))
toTId = tidName %%~ opGetName Nothing TaggedNominal

toTBody ::
    MonadNaming m =>
    Type (OldName m) # Annotated a ->
    m (Type (NewName m) # Annotated a)
toTBody (TVar tv) = opGetName Nothing TypeVar tv <&> TVar
toTBody (TFun (FuncType a b)) = FuncType <$> toType a <*> toType b <&> TFun
toTBody (TRecord composite) = TRecord <$> toCompositeFields composite
toTBody (TVariant composite) = TVariant <$> toCompositeFields composite
toTBody (TInst tid params) =
    TInst <$> toTId tid <*> traverse f params
    where
        f (k, v) = (,) <$> opGetName Nothing TypeVar k <*> toType v

toType ::
    MonadNaming m =>
    Annotated a # Type (OldName m) ->
    m (Annotated a # Type (NewName m))
toType (Ann (Const pl) x) = toTBody x <&> Ann (Const pl)

toScheme :: MonadNaming m => Scheme (OldName m) -> m (Scheme (NewName m))
toScheme (Scheme tvs typ) = Scheme tvs <$> toType typ

toDefinitionOutdatedType ::
    MonadNaming m =>
    DefinitionOutdatedType (OldName m) o a ->
    m (DefinitionOutdatedType (NewName m) o a)
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
toNodeActions = wrapInRecord toTagReplace

toResRecord ::
    MonadNaming m =>
    ResRecord (OldName m) a -> m (ResRecord (NewName m) a)
toResRecord = recordFields . traverse . _1 %%~ toTagOf Tag

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
    RList    x -> RList x & pure
    RTree    x -> RTree x & pure
    RTable   x -> (rtHeaders . traverse) (toTagOf Tag) x <&> RTable
    RRecord  x -> toResRecord x <&> RRecord
    RInject  x -> riTag (toTagOf Tag) x <&> RInject
    <&> (>>= traverse f)

toResVal :: MonadNaming m => ResVal (OldName m) -> m (ResVal (NewName m))
toResVal = resBody (toResBody toResVal)

toValAnnotation ::
    MonadNaming m =>
    EvaluationScopes (OldName m) (IM m) -> m (EvaluationScopes (NewName m) (IM m))
toValAnnotation evalRes =
    opRun <&>
    \run ->
    evalRes <&> traverse . traverse %~ (>>= run . toResVal)

toAnnotation ::
    MonadNaming m =>
    Annotation (EvaluationScopes (OldName m) (IM m)) (OldName m) ->
    m (Annotation (EvaluationScopes (NewName m) (IM m)) (NewName m))
toAnnotation AnnotationNone = pure AnnotationNone
toAnnotation (AnnotationType typ) = toType typ <&> AnnotationType
toAnnotation (AnnotationVal x) = toValAnnotation x <&> AnnotationVal

toPayload ::
    MonadNaming m =>
    Payload (OldName m) (IM m) o a ->
    m (Payload (NewName m) (IM m) o a)
toPayload payload@Payload{_plAnnotation, _plActions} =
    do
        _plAnnotation <- toAnnotation _plAnnotation
        _plActions <- toNodeActions _plActions
        pure payload{_plAnnotation, _plActions}

toNode ::
    MonadNaming m =>
    (ka # Annotated (Payload (OldName m) (IM m) o p) ->
     m (kb # Annotated (Payload (NewName m) (IM m) o p))) ->
    Annotated (Payload (OldName m) (IM m) o p) # ka ->
    m (Annotated (Payload (NewName m) (IM m) o p) # kb)
toNode toV (Ann (Const pl) v) =
    Ann
    <$> (toPayload pl <&> Const)
    <*> toV v

toLet ::
    MonadNaming m =>
    Body (Let (EvaluationScopes (OldName m) (IM m))) (OldName m) (IM m) o a ->
    m (Body (Let (EvaluationScopes (NewName m) (IM m))) (NewName m) (IM m) o a)
toLet let_@Let{_lName, _lVarInfo, _lBody, _lValue} =
    do
        (_lName, _lBody) <-
            unCPS (withTagRef TaggedVar _lVarInfo _lName)
            (toNode toBinder _lBody)
        _lValue <- toAssignment _lValue
        pure let_{_lName, _lBody, _lValue}

toBinder ::
    MonadNaming m =>
    Body (Binder (EvaluationScopes (OldName m) (IM m))) (OldName m) (IM m) o a ->
    m (Body (Binder (EvaluationScopes (NewName m) (IM m))) (NewName m) (IM m) o a)
toBinder (BinderLet l) = toLet l <&> BinderLet
toBinder (BinderTerm e) = toBody e <&> BinderTerm

toAddFirstParam ::
    MonadNaming m =>
    AddFirstParam (OldName m) (IM m) o ->
    m (AddFirstParam (NewName m) (IM m) o)
toAddFirstParam = _PrependParam toTagReplace

toFunction ::
    MonadNaming m =>
    Body (Function (EvaluationScopes (OldName m) (IM m))) (OldName m) (IM m) o a ->
    m (Body (Function (EvaluationScopes (NewName m) (IM m))) (NewName m) (IM m) o a)
toFunction func@Function{_fParams, _fBody, _fAddFirstParam} =
    (\(_fParams, _fBody) _fAddFirstParam ->
         func{_fParams, _fBody, _fAddFirstParam})
    <$> unCPS (withBinderParams _fParams) (toNode toBinder _fBody)
    <*> toAddFirstParam _fAddFirstParam

toBinderPlain ::
    MonadNaming m =>
    Body (AssignPlain (EvaluationScopes (OldName m) (IM m))) (OldName m) (IM m) o a ->
    m (Body (AssignPlain (EvaluationScopes (NewName m) (IM m))) (NewName m) (IM m) o a)
toBinderPlain AssignPlain{_apBody, _apAddFirstParam} =
    (\_apBody _apAddFirstParam -> AssignPlain{_apBody, _apAddFirstParam})
    <$> toBinder _apBody
    <*> toAddFirstParam _apAddFirstParam

toAssignment ::
    MonadNaming m =>
    Expr (Assignment (EvaluationScopes (OldName m) (IM m))) (OldName m) (IM m) o a ->
    m (Expr (Assignment (EvaluationScopes (NewName m) (IM m))) (NewName m) (IM m) o a)
toAssignment =
    \case
    BodyPlain x -> toBinderPlain x <&> BodyPlain
    BodyFunction x -> toFunction x <&> BodyFunction
    & toNode

toLam ::
    MonadNaming m =>
    Body (Lambda (EvaluationScopes (OldName m) (IM m))) (OldName m) (IM m) o a ->
    m (Body (Lambda (EvaluationScopes (NewName m) (IM m))) (NewName m) (IM m) o a)
toLam = lamFunc toFunction

toTagOf ::
    MonadNaming m => NameType -> Sugar.Tag (OldName m) -> m (Sugar.Tag (NewName m))
toTagOf nameType = tagName (opGetName Nothing nameType)

toTagReplace ::
    MonadNaming m =>
    TagReplace (OldName m) (IM m) o a ->
    m (TagReplace (NewName m) (IM m) o a)
toTagReplace (TagReplace opts new anon) =
    (,) <$> opRun <*> opRun
    <&>
    \(run0, run1) ->
    TagReplace
    { _tsOptions = opts >>= run0 . (traverse . toInfo) (toTagOf Tag)
    , _tsNewTag = new >>= run1 . toInfo (toTagOf Tag)
    , _tsAnon = anon
    }

toTagRefOf ::
    MonadNaming m =>
    NameType -> Sugar.TagRef (OldName m) (IM m) o ->
    m (Sugar.TagRef (NewName m) (IM m) o)
toTagRefOf nameType (Sugar.TagRef info actions jumpTo) =
    Sugar.TagRef
    <$> toTagOf nameType info
    <*> toTagReplace actions
    ?? jumpTo

withTagRef ::
    MonadNaming m =>
    NameType -> Sugar.VarInfo ->
    Sugar.TagRef (OldName m) (IM m) o ->
    CPS m (Sugar.TagRef (NewName m) (IM m) o)
withTagRef nameType varInfo (Sugar.TagRef info actions jumpTo) =
    Sugar.TagRef
    <$> tagName (opWithName varInfo nameType) info
    <*> liftCPS (toTagReplace actions)
    ?? jumpTo

toAnnotatedArg ::
    MonadNaming m =>
    Body (AnnotatedArg (EvaluationScopes (OldName m) (IM m))) (OldName m) (IM m) o a ->
    m (Body (AnnotatedArg (EvaluationScopes (NewName m) (IM m))) (NewName m) (IM m) o a)
toAnnotatedArg (AnnotatedArg tag e) =
    AnnotatedArg
    <$> toTagOf Tag tag
    <*> toExpression e

toLabeledApply ::
    MonadNaming m =>
    Body (LabeledApply (EvaluationScopes (OldName m) (IM m))) (OldName m) (IM m) o a ->
    m (Body (LabeledApply (EvaluationScopes (NewName m) (IM m))) (NewName m) (IM m) o a)
toLabeledApply
    app@LabeledApply{_aFunc, _aSpecialArgs, _aAnnotatedArgs, _aPunnedArgs} =
    LabeledApply
    <$> toNode (Lens._Wrapped (toBinderVarRef (Just (funcSignature app)))) _aFunc
    <*> traverse toExpression _aSpecialArgs
    <*> traverse toAnnotatedArg _aAnnotatedArgs
    <*> traverse (toNode (Lens._Wrapped toGetVar)) _aPunnedArgs

toHole ::
    MonadNaming m =>
    Hole (EvaluationScopes (OldName m) (IM m)) (OldName m) (IM m) o ->
    m (Hole (EvaluationScopes (NewName m) (IM m)) (NewName m) (IM m) o)
toHole hole =
    opRun
    <&>
    \run ->
    SugarLens.holeTransformExprs (run . toNode toBinder) hole

toFragment ::
    MonadNaming m =>
    Body (Fragment (EvaluationScopes (OldName m) (IM m))) (OldName m) (IM m) o a ->
    m (Body (Fragment (EvaluationScopes (NewName m) (IM m))) (NewName m) (IM m) o a)
toFragment Fragment{_fExpr, _fHeal, _fTypeMismatch, _fOptions} =
    do
        newTypeMismatch <- Lens._Just toType _fTypeMismatch
        run <- opRun
        newExpr <- toExpression _fExpr
        pure Fragment
            { _fExpr = newExpr
            , _fTypeMismatch = newTypeMismatch
            , _fOptions =
                 _fOptions
                 <&> Lens.mapped %~
                     SugarLens.holeOptionTransformExprs
                     (run . toNode toBinder)
            , _fHeal
            }

toCompositeItem ::
    MonadNaming m =>
    Body CompositeItem (OldName m) (IM m) o a ->
    m (Body CompositeItem (NewName m) (IM m) o a)
toCompositeItem (CompositeItem del tag e) =
    CompositeItem del
    <$> toTagRefOf Tag tag
    <*> toExpression e

toComposite ::
    MonadNaming m =>
    Body Composite (OldName m) (IM m) o a ->
    m (Body Composite (NewName m) (IM m) o a)
toComposite (Composite items punned tail_ addItem) =
    Composite
    <$> traverse toCompositeItem items
    <*> traverse (toNode (Lens._Wrapped toGetVar)) punned
    <*> (_OpenComposite . _2) toExpression tail_
    <*> toTagReplace addItem

toCase ::
    MonadNaming m =>
    Body Case (OldName m) (IM m) o a ->
    m (Body Case (NewName m) (IM m) o a)
toCase (Case k c) = Case <$> (_CaseWithArg . caVal) toExpression k <*> toComposite c

toInjectVal ::
    MonadNaming m =>
    Body (InjectContent (EvaluationScopes (OldName m) (IM m))) (OldName m) (IM m) o a ->
    m (Body (InjectContent (EvaluationScopes (NewName m) (IM m))) (NewName m) (IM m) o a)
toInjectVal (InjectVal v) = toExpression v <&> InjectVal
toInjectVal (InjectNullary n) = toNode (Lens._Wrapped (nullaryAddItem toTagReplace)) n <&> InjectNullary

toInject ::
    MonadNaming m =>
    Body (Inject (EvaluationScopes (OldName m) (IM m))) (OldName m) (IM m) o a ->
    m (Body (Inject (EvaluationScopes (NewName m) (IM m))) (NewName m) (IM m) o a)
toInject (Inject t v) =
    Inject <$> toTagRefOf Tag t <*> toInjectVal v

toGetField ::
    MonadNaming m =>
    Body GetField (OldName m) (IM m) o a ->
    m (Body GetField (NewName m) (IM m) o a)
toGetField (GetField r t) = GetField <$> toExpression r <*> toTagRefOf Tag t

toNominal ::
    MonadNaming m =>
    Body (Nominal (EvaluationScopes (OldName m) (IM m))) (OldName m) (IM m) o a ->
    m (Body (Nominal (EvaluationScopes (NewName m) (IM m))) (NewName m) (IM m) o a)
toNominal (Nominal t e) = Nominal <$> toTId t <*> toNode toBinder e

toElse ::
    MonadNaming m =>
    Body (Else (EvaluationScopes (OldName m) (IM m))) (OldName m) (IM m) o a ->
    m (Body (Else (EvaluationScopes (NewName m) (IM m))) (NewName m) (IM m) o a)
toElse (SimpleElse x) = toBody x <&> SimpleElse
toElse (ElseIf x) = toIfElse x <&> ElseIf

toIfElse ::
    MonadNaming m =>
    Body IfElse (OldName m) (IM m) o a ->
    m (Body IfElse (NewName m) (IM m) o a)
toIfElse (IfElse i t e) =
    IfElse
    <$> toExpression i
    <*> toExpression t
    <*> toNode toElse e

toBody ::
    MonadNaming m =>
    Body (Term (EvaluationScopes (OldName m) (IM m))) (OldName m) (IM m) o a ->
    m (Body (Term (EvaluationScopes (NewName m) (IM m))) (NewName m) (IM m) o a)
toBody =
    \case
    BodyGetField     x -> x & toGetField <&> BodyGetField
    BodyInject       x -> x & toInject <&> BodyInject
    BodyRecord       x -> x & toComposite <&> BodyRecord
    BodyCase         x -> x & toCase <&> BodyCase
    BodyIfElse       x -> x & toIfElse <&> BodyIfElse
    BodySimpleApply  x -> x & appChildren toExpression <&> BodySimpleApply
    BodyLabeledApply x -> x & toLabeledApply <&> BodyLabeledApply
    BodyHole         x -> x & toHole <&> BodyHole
    BodyFromNom      x -> x & toTId <&> BodyFromNom
    BodyToNom        x -> x & toNominal <&> BodyToNom
    BodyGetVar       x -> x & toGetVar <&> BodyGetVar
    BodyLiteral      x -> x & BodyLiteral & pure
    BodyLam          x -> x & toLam <&> BodyLam
    BodyFragment     x -> x & toFragment <&> BodyFragment
    BodyPlaceHolder    -> pure BodyPlaceHolder

funcSignature :: LabeledApply v name i o a -> FunctionSignature
funcSignature apply =
    FunctionSignature
    { sSpecialArgs = apply ^. aSpecialArgs & void
    , sNormalArgs = apply ^.. aAnnotatedArgs . traverse . aaTag . tagVal & Set.fromList
    }

toExpression ::
    MonadNaming m =>
    Expr (Term (EvaluationScopes (OldName m) (IM m))) (OldName m) (IM m) o a ->
    m (Expr (Term (EvaluationScopes (NewName m) (IM m))) (NewName m) (IM m) o a)
toExpression = toNode toBody

withParamInfo ::
    MonadNaming m =>
    Sugar.VarInfo -> ParamInfo (OldName m) (IM m) o ->
    CPS m (ParamInfo (NewName m) (IM m) o)
withParamInfo varInfo (ParamInfo tag fpActions) =
    ParamInfo
    <$> withTagRef TaggedVar varInfo tag
    <*> liftCPS ((fpAddNext . Sugar._AddNext) toTagReplace fpActions)

withFuncParam ::
    MonadNaming m =>
    (Sugar.VarInfo -> a -> CPS m b) ->
    (FuncParam (EvaluationScopes (OldName m) (IM m)) (OldName m), a) ->
    CPS m (FuncParam (EvaluationScopes (NewName m) (IM m)) (NewName m), b)
withFuncParam f (FuncParam pl varInfo, info) =
    (,)
    <$>
    ( FuncParam
        <$> liftCPS (toAnnotation pl)
        <*> pure varInfo
    ) <*> f varInfo info

withBinderParams ::
    MonadNaming m =>
    BinderParams (EvaluationScopes (OldName m) (IM m)) (OldName m) (IM m) o ->
    CPS m (BinderParams (EvaluationScopes (NewName m) (IM m)) (NewName m) (IM m) o)
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
toDef def@Definition{_drName, _drBody} =
    do
        -- NOTE: A global def binding is not considered a binder, as
        -- it exists everywhere, not just inside the binding
        _drName <- toTagRefOf GlobalDef _drName
        _drBody <- toDefinitionBody _drBody
        pure def{_drName, _drBody}

toTagPane :: MonadNaming m => TagPane (OldName m) o -> m (TagPane (NewName m) o)
toTagPane (TagPane tag i18n setSymbol setName) =
    toTagOf Tag tag <&> \x -> TagPane x i18n setSymbol setName

toPaneBody ::
    MonadNaming m =>
    PaneBody (OldName m) (IM m) o (Payload (OldName m) (IM m) o a) ->
    m (PaneBody (NewName m) (IM m) o (Payload (NewName m) (IM m) o a))
toPaneBody (PaneDefinition def) = toDef def <&> PaneDefinition
toPaneBody (PaneTag x) = toTagPane x <&> PaneTag

toPane ::
    MonadNaming m =>
    Pane (OldName m) (IM m) o (Payload (OldName m) (IM m) o a) ->
    m (Pane (NewName m) (IM m) o (Payload (NewName m) (IM m) o a))
toPane = paneBody toPaneBody

toRepl ::
    MonadNaming m =>
    Repl (OldName m) (IM m) o (Payload (OldName m) (IM m) o a) ->
    m (Repl (NewName m) (IM m) o (Payload (NewName m) (IM m) o a))
toRepl (Repl bod varInfo res) =
    Repl
    <$> toNode toBinder bod
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
