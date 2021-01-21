{-# LANGUAGE TypeFamilies, NamedFieldPuns, TemplateHaskell #-}
module Lamdu.Sugar.Names.Walk
    ( MonadNaming(..)
    , NameType(..), _GlobalDef, _TaggedVar, _TaggedNominal, _Tag
    , isGlobal
    , FunctionSignature(..), Disambiguator
    , NameConvertor, CPSNameConvertor
    , toWorkArea, toDef, toExpression, toBody
    ) where

import qualified Control.Lens as Lens
import qualified Data.Set as Set
import           Hyper.Class.Morph (morphTraverse)
import           Hyper.Type.AST.App (MorphWitness(..))
import           Hyper.Type.AST.FuncType (FuncType(..))
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Names.CPS (CPS(..), liftCPS)
import qualified Lamdu.Sugar.Types as Sugar
import           Lamdu.Sugar.Types hiding (Tag(..))

import           Lamdu.Prelude

-- TODO: Maybe remove "TaggedNominal", make it a disambiguation context?
data NameType = GlobalDef | TaggedVar | TaggedNominal | Tag | TypeVar
    deriving (Eq, Ord, Show)

Lens.makePrisms ''NameType

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

    opWithName :: NameType -> CPSNameConvertor m
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
    (v0 -> m v1) ->
    Annotation v0 (OldName m) ->
    m (Annotation v1 (NewName m))
toAnnotation _ AnnotationNone = pure AnnotationNone
toAnnotation _ (AnnotationType typ) = toType typ <&> AnnotationType
toAnnotation v (AnnotationVal x) = v x <&> AnnotationVal

type Pl v n m = Payload (Annotation v n) n (IM m)

toPayload :: MonadNaming m => (v0 -> m v1) -> Pl v0 (OldName m) m o -> m (Pl v1 (NewName m) m o)
toPayload v payload@Payload{_plAnnotation, _plActions} =
    do
        _plAnnotation <- toAnnotation v _plAnnotation
        _plActions <- toNodeActions _plActions
        pure payload{_plAnnotation, _plActions}

toNode ::
    MonadNaming m =>
    (v0 -> m v1) ->
    (ka # Annotated (Pl v0 (OldName m) m o, p) ->
     m (kb # Annotated (Pl v1 (NewName m) m o, p))) ->
    Annotated (Pl v0 (OldName m) m o, p) # ka ->
    m (Annotated (Pl v1 (NewName m) m o, p) # kb)
toNode toVal toV (Ann (Const pl) v) =
    Ann
    <$> (_1 (toPayload toVal) pl <&> Const)
    <*> toV v

type BodyW v t n m o a = Body t (Annotation v n) n (IM m) o a
type WalkBody t v0 v1 m o a = (v0 -> m v1) -> BodyW v0 t (OldName m) m o a -> m (BodyW v1 t (NewName m) m o a)

toLet :: MonadNaming m => WalkBody Let v0 v1 m o a
toLet v let_@Let{_lName, _lBody, _lValue} =
    do
        (_lName, _lBody) <-
            unCPS (withTagRef TaggedVar _lName)
            (toNode v (toBinder v) _lBody)
        _lValue <- toAssignment v _lValue
        pure let_{_lName, _lBody, _lValue}

toBinder :: MonadNaming m => WalkBody Binder v0 v1 m o a
toBinder v (BinderLet l) = toLet v l <&> BinderLet
toBinder v (BinderTerm e) = toBody v e <&> BinderTerm

toAddFirstParam ::
    MonadNaming m =>
    AddFirstParam (OldName m) (IM m) o ->
    m (AddFirstParam (NewName m) (IM m) o)
toAddFirstParam = _PrependParam toTagReplace

toFunction :: MonadNaming m => WalkBody Function v0 v1 m o a
toFunction v func@Function{_fParams, _fBody, _fAddFirstParam} =
    (\(_fParams, _fBody) _fAddFirstParam ->
         func{_fParams, _fBody, _fAddFirstParam})
    <$> unCPS (withBinderParams v _fParams) (toNode v (toBinder v) _fBody)
    <*> toAddFirstParam _fAddFirstParam

toBinderPlain :: MonadNaming m => WalkBody AssignPlain v0 v1 m o a
toBinderPlain v AssignPlain{_apBody, _apAddFirstParam} =
    (\_apBody _apAddFirstParam -> AssignPlain{_apBody, _apAddFirstParam})
    <$> toBinder v _apBody
    <*> toAddFirstParam _apAddFirstParam

type ExprW t v n m o a = Expr t (Annotation v n) n (IM m) o a
type WalkExpr t v0 v1 m o a = (v0 -> m v1) -> ExprW t v0 (OldName m) m o a -> m (ExprW t v1 (NewName m) m o a)

toAssignment :: MonadNaming m => WalkExpr Assignment v0 v1 m o a
toAssignment v =
    \case
    BodyPlain x -> toBinderPlain v x <&> BodyPlain
    BodyFunction x -> toFunction v x <&> BodyFunction
    & toNode v

toTagOf :: MonadNaming m => NameType -> Sugar.Tag (OldName m) -> m (Sugar.Tag (NewName m))
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
    NameType ->
    Sugar.TagRef (OldName m) (IM m) o ->
    CPS m (Sugar.TagRef (NewName m) (IM m) o)
withTagRef nameType (Sugar.TagRef info actions jumpTo) =
    Sugar.TagRef
    <$> tagName (opWithName nameType) info
    <*> liftCPS (toTagReplace actions)
    ?? jumpTo

toAnnotatedArg :: MonadNaming m => WalkBody AnnotatedArg v0 v1 m o a
toAnnotatedArg v (AnnotatedArg tag e) =
    AnnotatedArg
    <$> toTagOf Tag tag
    <*> toExpression v e

toLabeledApply :: MonadNaming m => WalkBody LabeledApply v0 v1 m o a
toLabeledApply v app@LabeledApply{_aFunc, _aSpecialArgs, _aAnnotatedArgs, _aPunnedArgs} =
    LabeledApply
    <$> toNode v (Lens._Wrapped (toBinderVarRef (Just (funcSignature app)))) _aFunc
    <*> traverse (toExpression v) _aSpecialArgs
    <*> traverse (toAnnotatedArg v) _aAnnotatedArgs
    <*> traverse (toNode v (Lens._Wrapped toGetVar)) _aPunnedArgs

type SugarElem t v n m (o :: * -> *) = t (Annotation v n) n (IM m) o

toHoleOption ::
    MonadNaming m =>
    (m (ExprW Binder () (NewName m) m o ()) -> IM m (ExprW Binder () (NewName m) m o ())) ->
    (m (NewName m) -> IM m (NewName m)) ->
    HoleOption (OldName m) (IM m) o -> HoleOption (NewName m) (IM m) o
toHoleOption run0 run1 option =
    option
    { _hoSearchTerms =
        -- Hack: Just using TaggedVar as NameType because disambiguations aren't important in hole results
        option ^. hoSearchTerms >>= traverse . traverse %%~ run1 . opGetName Nothing TaggedVar
    , _hoResults = option ^. hoResults <&> _2 %~ (>>= holeResultConverted (run0 . toNode pure (toBinder pure)))
    }

toHole :: MonadNaming m => Hole (OldName m) (IM m) o -> m (Hole (NewName m) (IM m) o)
toHole hole =
    (,) <$> opRun <*> opRun
    <&>
    \(r0, r1) ->
    hole & Sugar.holeOptions . Lens.mapped . Lens.mapped %~ toHoleOption r0 r1

toFragment :: MonadNaming m => WalkBody Fragment v0 v1 m o a
toFragment v Fragment{_fExpr, _fHeal, _fTypeMismatch, _fOptions} =
    do
        newTypeMismatch <- Lens._Just toType _fTypeMismatch
        newExpr <- toExpression v _fExpr
        r0 <- opRun
        r1 <- opRun
        pure Fragment
            { _fExpr = newExpr
            , _fTypeMismatch = newTypeMismatch
            , _fOptions =
                 _fOptions
                 <&> Lens.mapped %~ toHoleOption r0 r1
            , _fHeal
            }

toCompositeItem :: MonadNaming m => WalkBody CompositeItem v0 v1 m o a
toCompositeItem v (CompositeItem del tag e) =
    CompositeItem del
    <$> toTagRefOf Tag tag
    <*> toExpression v e

toComposite :: MonadNaming m => WalkBody Composite v0 v1 m o a
toComposite v (Composite items punned tail_ addItem) =
    Composite
    <$> traverse (toCompositeItem v) items
    <*> traverse (toNode v (Lens._Wrapped toGetVar)) punned
    <*> (_OpenComposite . _2) (toExpression v) tail_
    <*> toTagReplace addItem

toCase :: MonadNaming m => WalkBody Case v0 v1 m o a
toCase v (Case k c) = Case <$> (_CaseWithArg . caVal) (toExpression v) k <*> toComposite v c

toInjectVal :: MonadNaming m => WalkBody InjectContent v0 v1 m o a
toInjectVal v (InjectVal x) = toExpression v x <&> InjectVal
toInjectVal v (InjectNullary n) = toNode v (Lens._Wrapped (nullaryAddItem toTagReplace)) n <&> InjectNullary

toInject :: MonadNaming m => WalkBody Inject v0 v1 m o a
toInject v (Inject t x) =
    Inject <$> toTagRefOf Tag t <*> toInjectVal v x

toGetField :: MonadNaming m => WalkBody GetField v0 v1 m o a
toGetField v (GetField r t) = GetField <$> toExpression v r <*> toTagRefOf Tag t

toNominal :: MonadNaming m => WalkBody Nominal v0 v1 m o a
toNominal v (Nominal t e) = Nominal <$> toTId t <*> toNode v (toBinder v) e

toElse :: MonadNaming m => WalkBody Else v0 v1 m o a
toElse v (SimpleElse x) = toBody v x <&> SimpleElse
toElse v (ElseIf x) = toIfElse v x <&> ElseIf

toIfElse :: MonadNaming m => WalkBody IfElse v0 v1 m o a
toIfElse v (IfElse i t e) = IfElse <$> toExpression v i <*> toExpression v t <*> toNode v (toElse v) e

toBody :: MonadNaming m => WalkBody Term v0 v1 m o a
toBody v =
    \case
    BodyGetField     x -> x & toGetField v <&> BodyGetField
    BodyInject       x -> x & toInject v <&> BodyInject
    BodyRecord       x -> x & toComposite v <&> BodyRecord
    BodyCase         x -> x & toCase v <&> BodyCase
    BodyIfElse       x -> x & toIfElse v <&> BodyIfElse
    BodySimpleApply  x -> x & morphTraverse (\M_App_expr -> toExpression v) <&> BodySimpleApply
    BodyLabeledApply x -> x & toLabeledApply v <&> BodyLabeledApply
    BodyHole         x -> x & toHole <&> BodyHole
    BodyFromNom      x -> x & toTId <&> BodyFromNom
    BodyToNom        x -> x & toNominal v <&> BodyToNom
    BodyGetVar       x -> x & toGetVar <&> BodyGetVar
    BodyLiteral      x -> x & BodyLiteral & pure
    BodyLam          x -> x & lamFunc (toFunction v) <&> BodyLam
    BodyFragment     x -> x & toFragment v <&> BodyFragment
    BodyPlaceHolder    -> pure BodyPlaceHolder

funcSignature :: LabeledApply v name i o a -> FunctionSignature
funcSignature apply =
    FunctionSignature
    { sSpecialArgs = apply ^. aSpecialArgs & void
    , sNormalArgs = apply ^.. aAnnotatedArgs . traverse . aaTag . tagVal & Set.fromList
    }

toExpression :: MonadNaming m => WalkExpr Term v0 v1 m o a
toExpression = toNode <*> toBody

withParamInfo ::
    MonadNaming m =>
    ParamInfo (OldName m) (IM m) o ->
    CPS m (ParamInfo (NewName m) (IM m) o)
withParamInfo (ParamInfo tag fpActions) =
    ParamInfo
    <$> withTagRef TaggedVar tag
    <*> liftCPS ((fpAddNext . Sugar._AddNext) toTagReplace fpActions)

withFuncParam ::
    MonadNaming m =>
    (v0 -> m v1) ->
    (a -> CPS m b) ->
    (FuncParam (Annotation v0 (OldName m)) (OldName m), a) ->
    CPS m (FuncParam (Annotation v1 (NewName m)) (NewName m), b)
withFuncParam v f (FuncParam pl varInfo, info) =
    (,)
    <$>
    ( FuncParam
        <$> liftCPS (toAnnotation v pl)
        <*> pure varInfo
    ) <*> f info

withBinderParams ::
    MonadNaming m =>
    (v0 -> m v1) ->
    SugarElem BinderParams v0 (OldName m) m o ->
    CPS m (SugarElem BinderParams v1 (NewName m) m o)
withBinderParams v (NullParam x) = withFuncParam v pure x <&> NullParam
withBinderParams v (Params xs) = traverse (withFuncParam v withParamInfo) xs <&> Params

type Top t n m o a = t (Annotation (EvaluationScopes n (IM m)) n) n (IM m) o (Pl (EvaluationScopes n (IM m)) n m o, a)
type WalkTop t m o a = Top t (OldName m) m o a -> m (Top t (NewName m) m o a)

toDefExpr :: MonadNaming m => WalkTop DefinitionExpression m o a
toDefExpr (DefinitionExpression typ presMode content) =
    DefinitionExpression
    <$> toScheme typ
    <*> pure presMode
    <*> toAssignment toValAnnotation content

toDefinitionBody :: MonadNaming m => WalkTop DefinitionBody m o a
toDefinitionBody (DefinitionBodyBuiltin bi) =
    bi & biType %%~ toScheme <&> DefinitionBodyBuiltin
toDefinitionBody (DefinitionBodyExpression expr) =
    toDefExpr expr <&> DefinitionBodyExpression

toDef :: MonadNaming m => WalkTop Definition m o a
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

toPaneBody :: MonadNaming m => WalkTop PaneBody m o a
toPaneBody (PaneDefinition def) = toDef def <&> PaneDefinition
toPaneBody (PaneTag x) = toTagPane x <&> PaneTag

toWorkArea :: MonadNaming m => WalkTop WorkArea m o a
toWorkArea WorkArea { _waPanes, _waRepl, _waGlobals } =
    do
        run <- opRun
        panes <- (traverse . paneBody) toPaneBody _waPanes
        repl <- replExpr (toNode toValAnnotation (toBinder toValAnnotation)) _waRepl
        let globals = _waGlobals >>= run . toGlobals
        WorkArea panes repl globals & pure
    where
        toGlobals = (traverse . nrName) (opGetName Nothing GlobalDef)
