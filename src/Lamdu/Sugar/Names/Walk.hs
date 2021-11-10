{-# LANGUAGE TypeFamilies, NamedFieldPuns, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Lamdu.Sugar.Names.Walk
    ( MonadNaming(..)
    , NameType(..), _GlobalDef, _TaggedVar, _TaggedNominal, _Tag
    , isGlobal
    , FunctionSignature(..), Disambiguator
    , IsUnambiguous(..)
    , NameConvertor, CPSNameConvertor
    , Walk(..), toWorkArea, toWorkAreaTest
    ) where

import qualified Control.Lens as Lens
import           Data.Kind (Type)
import qualified Data.Set as Set
import           Hyper.Class.Morph (morphTraverse1)
import           Hyper.Syntax (FuncType(..))
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Convert.Input (userData)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.CPS (CPS(..), liftCPS)
import qualified Lamdu.Sugar.Types as Sugar
import           Lamdu.Sugar.Types hiding (Tag(..), Type)

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
    { sIsOperator :: Bool
    , sNormalArgs :: Set T.Tag
    } deriving (Eq, Ord, Show)

type Disambiguator = FunctionSignature

data IsUnambiguous = Unambiguous | MayBeAmbiguous

-- TODO: Rename MonadNameWalk
class (Monad m, Monad (IM m)) => MonadNaming m where
    type OldName m
    type NewName m
    type IM m :: * -> *
    opRun :: m (m a -> IM m a)

    opWithName :: IsUnambiguous -> NameType -> CPSNameConvertor m
    opGetName :: Maybe Disambiguator -> IsUnambiguous -> NameType -> NameConvertor m

class Walk m s t where
    walk :: MonadNaming m => s -> m t

instance Walk m () () where
    walk = pure

instance Walk m s t => Walk m (s, x) (t, x) where
    walk = _1 walk

instance (a ~ OldName m, b ~ NewName m) => Walk m (ParamRef a o) (ParamRef b o) where
    walk p = (pNameRef . nrName) (opGetName Nothing (binderAmbiguity (p ^. pBinderMode)) TaggedVar) p

binderVarType :: BinderVarForm name m -> NameType
binderVarType GetLet = TaggedVar
binderVarType (GetDefinition _) = GlobalDef

instance
    (a ~ OldName m, b ~ NewName m, Walk m fa fb) =>
    Walk m (CompositeFields a fa) (CompositeFields b fb) where
    walk (CompositeFields fields mExt) =
        CompositeFields
        <$> traverse toField fields
        <*> Lens._Just (opGetName Nothing MayBeAmbiguous TypeVar) mExt
        where
            toField (tag, typ) = (,) <$> toTagOf Tag tag <*> walk typ

instance (a ~ OldName m, b ~ NewName m) => Walk m (TId a o) (TId b o) where
    walk = tidName %%~ opGetName Nothing MayBeAmbiguous TaggedNominal

instance (a ~ OldName m, b ~ NewName m) => Walk m (Sugar.Type a o # Annotated p) (Sugar.Type b o # Annotated p) where
    walk (TVar tv) = opGetName Nothing MayBeAmbiguous TypeVar tv <&> TVar
    walk (TFun (FuncType a b)) = FuncType <$> walk a <*> walk b <&> TFun
    walk (TRecord composite) = TRecord <$> walk composite
    walk (TVariant composite) = TVariant <$> walk composite
    walk (TInst tid params) =
        TInst <$> walk tid <*> traverse f params
        where
            f (k, v) = (,) <$> opGetName Nothing MayBeAmbiguous TypeVar k <*> walk v

instance (a ~ OldName m, b ~ NewName m) => Walk m (Annotated p # Sugar.Type a o) (Annotated p # Sugar.Type b o) where
    walk (Ann (Const pl) x) = walk x <&> Ann (Const pl)

instance (a ~ OldName m, b ~ NewName m) => Walk m (Scheme a o) (Scheme b o) where
    walk (Scheme tvs typ) = Scheme tvs <$> walk typ

instance (a ~ OldName m, b ~ NewName m) => Walk m (DefinitionOutdatedType a o p) (DefinitionOutdatedType b o p) where
    walk (DefinitionOutdatedType whenUsed current useCur) =
        DefinitionOutdatedType <$> walk whenUsed <*> walk current ?? useCur

toBinderVarRef ::
    MonadNaming m =>
    Maybe Disambiguator ->
    BinderVarRef (OldName m) o ->
    m (BinderVarRef (NewName m) o)
toBinderVarRef mDisambig (BinderVarRef nameRef form var inline) =
    BinderVarRef
    <$> ( nrName %%~
          opGetName mDisambig MayBeAmbiguous (binderVarType form)
        ) nameRef
    <*> (_GetDefinition . _DefTypeChanged %%~ walk) form
    ?? var
    ?? inline

instance (a ~ OldName m, b ~ NewName m) => Walk m (GetVar a o) (GetVar b o) where
    walk (GetParam x) = walk x <&> GetParam
    walk (GetBinder x) = toBinderVarRef Nothing x <&> GetBinder
    walk (GetParamsRecord x) =
        traverse (opGetName Nothing MayBeAmbiguous Tag) x <&> GetParamsRecord

instance (a ~ OldName m, b ~ NewName m) => Walk m (ResRecord a p) (ResRecord b p) where
    walk = recordFields . traverse . _1 %%~ toTagOf Tag

instance (a ~ OldName m, b ~ NewName m, Walk m p q) => Walk m (ResBody a p) (ResBody b q) where
    walk =
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
        RRecord  x -> walk x <&> RRecord
        RInject  x -> riTag (toTagOf Tag) x <&> RInject
        <&> (>>= traverse walk)

instance (a ~ OldName m, b ~ NewName m) => Walk m (ResVal a) (ResVal b) where
    walk = resBody walk

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (EvaluationScopes a i) (EvaluationScopes b i) where
    walk evalRes =
        opRun <&>
        \run ->
        evalRes <&> traverse . traverse %~ (>>= run . walk)

instance (a ~ OldName m, b ~ NewName m, Walk m va vb) => Walk m (Annotation va a) (Annotation vb b) where
    walk AnnotationNone = pure AnnotationNone
    walk (AnnotationType typ) = walk typ <&> AnnotationType
    walk (AnnotationVal x) = walk x <&> AnnotationVal

instance Walk m va vb => Walk m (Payload va o) (Payload vb o) where
    walk = plAnnotation walk

toNode ::
    (MonadNaming m, Walk m pa pb) =>
    (ka # Annotated pa -> m (kb # Annotated pb)) ->
    Annotated pa # ka -> m (Annotated pb # kb)
toNode toV (Ann (Const pl) v) = Ann <$> (walk pl <&> Const) <*> toV v

type BodyW t v n m (o :: Type -> Type) a = t v n (IM m) o # Annotated a
type WalkBody t m o v0 v1 a0 a1 = BodyW t v0 (OldName m) m o a0 -> m (BodyW t v1 (NewName m) m o a1)

class ToBody t where
    toBody :: (MonadNaming m, Walk m v0 v1, Walk m a0 a1) => WalkBody t m o v0 v1 a0 a1

instance ToBody Let where
    toBody let_@Let{_lName, _lBody, _lValue} =
        do
            (_lName, _lBody) <-
                unCPS (withOptionalTag MayBeAmbiguous TaggedVar _lName)
                (toExpression _lBody)
            _lValue <- toExpression _lValue
            pure let_{_lName, _lBody, _lValue}

instance ToBody Binder where
    toBody (BinderLet l) = toBody l <&> BinderLet
    toBody (BinderTerm e) = toBody e <&> BinderTerm

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (AddFirstParam a i o) (AddFirstParam b i o) where
    walk x = opRun <&> \run -> x & _PrependParam %~ (>>= run . walk)

toFunction :: (MonadNaming m, Walk m v0 v1, Walk m a0 a1) => IsUnambiguous -> WalkBody Function m o v0 v1 a0 a1
toFunction u func@Function{_fParams, _fBody, _fAddFirstParam} =
    (\(_fParams, _fBody) _fAddFirstParam ->
         func{_fParams, _fBody, _fAddFirstParam})
    <$> unCPS (withBinderParams u _fParams) (toExpression _fBody)
    <*> walk _fAddFirstParam

instance ToBody AssignPlain where
    toBody AssignPlain{_apBody, _apAddFirstParam} =
        (\_apBody _apAddFirstParam -> AssignPlain{_apBody, _apAddFirstParam})
        <$> toBody _apBody
        <*> walk _apAddFirstParam

type ExprW t v n m (o :: Type -> Type) a = Annotated a # t v n (IM m) o
type WalkExpr t m o v0 v1 a0 a1 = ExprW t v0 (OldName m) m o a0 -> m (ExprW t v1 (NewName m) m o a1)

instance ToBody Assignment where
    toBody =
        \case
        BodyPlain x -> toBody x <&> BodyPlain
        BodyFunction x -> toFunction MayBeAmbiguous x <&> BodyFunction

toTagOf :: MonadNaming m => NameType -> Sugar.Tag (OldName m) -> m (Sugar.Tag (NewName m))
toTagOf = tagName . opGetName Nothing MayBeAmbiguous

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (TagChoice a o) (TagChoice b o) where
    walk (TagChoice opts new) =
        TagChoice
        <$> (traverse . toInfo) (toTagOf Tag) opts
        <*> toInfo (toTagOf Tag) new

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (Hole a i o) (Hole b i o) where
    walk (Hole opts) =
        opRun <&>
        \run ->
        opts
        <&> Lens.mapped %~ (>>= run . (traverse . optionExpr) toExpression)
        & Hole

toTagRefOf ::
    MonadNaming m =>
    NameType -> Sugar.TagRef (OldName m) (IM m) o ->
    m (Sugar.TagRef (NewName m) (IM m) o)
toTagRefOf nameType (Sugar.TagRef info replace jumpTo) =
    Sugar.TagRef
    <$> toTagOf nameType info
    <*> (opRun <&> \run -> replace >>= run . walk)
    ?? jumpTo

withTagRef ::
    MonadNaming m =>
    IsUnambiguous -> NameType ->
    Sugar.TagRef (OldName m) (IM m) o ->
    CPS m (Sugar.TagRef (NewName m) (IM m) o)
withTagRef unambig nameType (Sugar.TagRef info replace jumpTo) =
    Sugar.TagRef
    <$> tagName (opWithName unambig nameType) info
    <*> liftCPS (opRun <&> \run -> replace >>= run . walk)
    ?? jumpTo

toOptionalTag ::
    MonadNaming f =>
    NameType -> OptionalTag (OldName f) (IM f) o ->
    f (OptionalTag (NewName f) (IM f) o)
toOptionalTag = Sugar.oTag . toTagRefOf

withOptionalTag ::
    MonadNaming m =>
    IsUnambiguous -> NameType ->
    OptionalTag (OldName m) (IM m) o ->
    CPS m (OptionalTag (NewName m) (IM m) o)
withOptionalTag unambig = Sugar.oTag . withTagRef unambig

instance ToBody AnnotatedArg where
    toBody (AnnotatedArg tag e) =
        AnnotatedArg
        <$> toTagOf Tag tag
        <*> toExpression e

instance ToBody LabeledApply where
    toBody app@LabeledApply{_aFunc, _aMOpArgs, _aAnnotatedArgs, _aPunnedArgs} =
        LabeledApply
        <$> toNode (Lens._Wrapped (toBinderVarRef (Just (funcSignature app)))) _aFunc
        <*> (traverse . morphTraverse1) toExpression _aMOpArgs
        <*> traverse toBody _aAnnotatedArgs
        <*> (traverse . pvVar) (toNode (Lens._Wrapped walk)) _aPunnedArgs

instance ToBody Fragment where
    toBody Fragment{_fExpr, _fHeal, _fTypeMismatch, _fOptions} =
        do
            newTypeMismatch <- Lens._Just walk _fTypeMismatch
            newExpr <- toExpression _fExpr
            run <- opRun
            pure Fragment
                { _fExpr = newExpr
                , _fTypeMismatch = newTypeMismatch
                , _fHeal
                , _fOptions =
                    _fOptions
                    <&> Lens.mapped %~ (>>= run . (traverse . optionExpr) toExpression)
                }

instance ToBody FragOpt where
    toBody (FragPostfix x) = traverse toExpression x <&> FragPostfix
    toBody (FragInject x) = walk x <&> FragInject
    toBody (FragWrapInRec x) = walk x <&> FragWrapInRec
    toBody (FragGetVar x) = walk x <&> FragGetVar
    toBody (FragOp x) = toBody x <&> FragOp
    toBody (FragToNom x) = walk x <&> FragToNom
    toBody (FragIf x) = toExpression x <&> FragIf
    toBody FragLam = pure FragLam

instance ToBody FragOperator where
    toBody (FragOperator f a) =
        FragOperator
        <$> toNode (Lens._Wrapped (toBinderVarRef Nothing)) f
        <*> toExpression a

instance
    (Walk m v0 v1, Walk m p0 p1, i ~ IM m, a ~ OldName m, b ~ NewName m) =>
    Walk m (Ann (Const p0) # Term v0 a i o) (Ann (Const p1) # Term v1 b i o) where
    walk = toExpression

instance (Walk m pa pb, i ~ IM m, a ~ OldName m, b ~ NewName m) => Walk m (TaggedItem a i o pa) (TaggedItem b i o pb) where
    walk ti@TaggedItem{_tiTag, _tiAddAfter, _tiValue} =
        (,,)
        <$> toTagRefOf Tag _tiTag
        <*> (opRun <&> \run -> _tiAddAfter >>= run . walk)
        <*> walk _tiValue
        <&> \(_tiTag, _tiAddAfter, _tiValue) -> ti{_tiTag,_tiValue,_tiAddAfter}

instance (Walk m pa pb, i ~ IM m, a ~ OldName m, b ~ NewName m) => Walk m (TaggedList a i o pa) (TaggedList b i o pb) where
    walk (TaggedList add items) =
        TaggedList
        <$> (opRun <&> \run -> add >>= run . walk)
        <*> (Lens._Just . SugarLens.taggedListBodyItems) walk items

instance ToBody Composite where
    toBody (Composite items punned tail_) =
        Composite
        <$> walk items
        <*> (traverse . pvVar) (toNode (Lens._Wrapped walk)) punned
        <*> _OpenComposite toExpression tail_

instance ToBody Nominal where
    toBody (Nominal t e) = Nominal <$> walk t <*> toExpression e

instance ToBody Else where
    toBody (SimpleElse x) = toBody x <&> SimpleElse
    toBody (ElseIf x) = toBody x <&> ElseIf

instance ToBody IfElse where
    toBody (IfElse i t e) = IfElse <$> toExpression i <*> toExpression t <*> toExpression e

instance ToBody PostfixFunc where
    toBody (PfCase x) = toBody x <&> PfCase
    toBody (PfFromNom x) = walk x <&> PfFromNom
    toBody (PfGetField x) = toTagRefOf Tag x <&> PfGetField

instance ToBody PostfixApply where
    toBody (PostfixApply a f) =
        PostfixApply <$> toExpression a <*> toExpression f

binderAmbiguity :: BinderMode -> IsUnambiguous
binderAmbiguity LightLambda = Unambiguous
binderAmbiguity _ = MayBeAmbiguous

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m, Walk m pa pb) =>
    Walk m (NullaryInject a i o # Annotated pa) (NullaryInject b i o # Annotated pb) where
    walk (NullaryInject t n) =
        do
            run <- opRun
            NullaryInject
                <$> toNode (Lens._Wrapped walk) t
                <*> toNode (pure . (Lens._Wrapped %~ (>>= run . walk))) n

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (TagRef a i o) (TagRef b i o) where
    walk = toTagRefOf Tag

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (Leaf a i o) (Leaf b i o) where
    walk =
        \case
        LeafInject       x -> x & walk <&> LeafInject
        LeafHole         x -> x & walk <&> LeafHole
        LeafGetVar       x -> x & walk <&> LeafGetVar
        LeafLiteral      x -> x & LeafLiteral & pure

instance ToBody Term where
    toBody =
        \case
        BodyRecord       x -> x & toBody <&> BodyRecord
        BodyIfElse       x -> x & toBody <&> BodyIfElse
        BodySimpleApply  x -> x & morphTraverse1 toExpression <&> BodySimpleApply
        BodyPostfixApply x -> x & toBody <&> BodyPostfixApply
        BodyLabeledApply x -> x & toBody <&> BodyLabeledApply
        BodyPostfixFunc  x -> x & toBody <&> BodyPostfixFunc
        BodyToNom        x -> x & toBody <&> BodyToNom
        BodyLam          x -> x & lamFunc (toFunction (binderAmbiguity (x ^. lamMode))) <&> BodyLam
        BodyFragment     x -> x & toBody <&> BodyFragment
        BodyNullaryInject x -> walk x <&> BodyNullaryInject
        BodyLeaf         x -> walk x <&> BodyLeaf

funcSignature :: LabeledApply v name i o a -> FunctionSignature
funcSignature apply =
    FunctionSignature
    { sIsOperator = Lens.has (aMOpArgs . Lens._Just) apply
    , sNormalArgs = apply ^.. aAnnotatedArgs . traverse . aaTag . tagVal & Set.fromList
    }

toExpression :: (MonadNaming m, Walk m v0 v1, Walk m a0 a1, ToBody e) => WalkExpr e m o v0 v1 a0 a1
toExpression = toNode toBody

withRecordParamInfo ::
    MonadNaming m =>
    IsUnambiguous ->
    RecordParamInfo (OldName m) (IM m) o ->
    CPS m (RecordParamInfo (NewName m) (IM m) o)
withRecordParamInfo unambig x@RecordParamInfo{_piTag, _piAddNext} =
    (,)
    <$> withTagRef unambig TaggedVar _piTag
    <*> liftCPS (opRun <&> \run -> _piAddNext >>= run . walk)
    <&> \(_piTag, _piAddNext) -> x{_piTag, _piAddNext}

withVarParamInfo ::
    MonadNaming m =>
    IsUnambiguous ->
    VarParamInfo (OldName m) (IM m) o ->
    CPS m (VarParamInfo (NewName m) (IM m) o)
withVarParamInfo unambig x@VarParamInfo{_vpiTag, _vpiAddNext} =
    (,)
    <$> withOptionalTag unambig TaggedVar _vpiTag
    <*> liftCPS (opRun <&> \run -> _vpiAddNext & _AddNext %~ (>>= run . walk))
    <&> \(_vpiTag, _vpiAddNext) -> x{_vpiTag, _vpiAddNext}

withFuncParam ::
    (MonadNaming m, Walk m v0 v1) =>
    (a -> CPS m b) ->
    (FuncParam v0, a) ->
    CPS m (FuncParam v1, b)
withFuncParam f (FuncParam pl varInfo, info) =
    (,)
    <$>
    ( FuncParam
        <$> liftCPS (walk pl)
        <*> pure varInfo
    ) <*> f info

withBinderParams ::
    (MonadNaming m, Walk m v0 v1) =>
    IsUnambiguous ->
    BinderParams v0 (OldName m) (IM m) o ->
    CPS m (BinderParams v1 (NewName m) (IM m) o)
withBinderParams _ (NullParam x) = withFuncParam pure x <&> NullParam
withBinderParams u (RecordParams xs) = traverse (withFuncParam (withRecordParamInfo u)) xs <&> RecordParams
withBinderParams u (VarParam x) = withFuncParam (withVarParamInfo u) x <&> VarParam

type Top t n i (o :: Type -> Type) p = t (Annotation (EvaluationScopes n i) n) n i o p

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m, Walk m pa pb) =>
    Walk m (Top DefinitionExpression a i o pa) (Top DefinitionExpression b i o pb)
     where
    walk (DefinitionExpression typ presMode content) =
        DefinitionExpression
        <$> walk typ
        <*> pure presMode
        <*> toExpression content

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m, Walk m pa pb) =>
    Walk m (Top DefinitionBody a i o pa) (Top DefinitionBody b i o pb) where
        walk (DefinitionBodyBuiltin bi) =
            bi & biType %%~ walk <&> DefinitionBodyBuiltin
        walk (DefinitionBodyExpression expr) =
            walk expr <&> DefinitionBodyExpression

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m, Walk m pa pb) =>
    Walk m (Top Definition a i o pa) (Top Definition b i o pb) where
    walk def@Definition{_drName, _drBody} =
        do
            -- NOTE: A global def binding is not considered a binder, as
            -- it exists everywhere, not just inside the binding
            _drName <- toOptionalTag GlobalDef _drName
            _drBody <- walk _drBody
            pure def{_drName, _drBody}

instance (a ~ OldName m, b ~ NewName m, IM m ~ i) => Walk m (NominalParam a i o) (NominalParam b i o) where
    walk = pName walk

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (NominalPane a i o) (NominalPane b i o) where
    walk nomPane@NominalPane{_npName, _npParams, _npBody} =
        do
            _npName <- toOptionalTag TaggedNominal _npName
            _npParams <- traverse walk _npParams
            _npBody <- Lens._Just walk _npBody
            pure nomPane{_npName, _npParams, _npBody}

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m, Walk m pa pb) =>
    Walk m (Top PaneBody a i o pa) (Top PaneBody b i o pb) where
    walk (PaneDefinition def) = walk def <&> PaneDefinition
    walk (PaneTag x) = PaneTag x & pure
    walk (PaneNominal x) = walk x <&> PaneNominal

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m) =>
    Walk m (Globals a i o) (Globals b i o) where
    walk (Globals d n t) =
        do
            run <- opRun
            let toGlobals x nameType =
                    x >>= run . (traverse . nrName) (opGetName Nothing MayBeAmbiguous nameType)
            Globals (toGlobals d GlobalDef) (toGlobals n TaggedNominal) (toGlobals t Tag) & pure

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m, Walk m pa pb) =>
    Walk m (Top WorkArea a i o pa) (Top WorkArea b i o pb) where
    walk WorkArea { _waPanes, _waRepl, _waGlobals } =
        do
            panes <- (traverse . paneBody) walk _waPanes
            repl <- replExpr (toNode toBody) _waRepl
            WorkArea panes repl <$> walk _waGlobals

instance Walk m a b => Walk m (ConvertPayload n a) (ConvertPayload n b) where
    walk = (pInput . userData) walk

toWorkArea ::
    MonadNaming m =>
    Top WorkArea (OldName m) (IM m) o
        (ConvertPayload n (Annotation (EvaluationScopes (OldName m) (IM m)) (OldName m), a)) ->
    m (Top WorkArea (NewName m) (IM m) o
        (ConvertPayload n (Annotation (EvaluationScopes (NewName m) (IM m)) (NewName m), a)))
toWorkArea = walk

toWorkAreaTest ::
    MonadNaming m =>
    Top WorkArea (OldName m) (IM m) o
        (Payload (Annotation (EvaluationScopes (OldName m) (IM m)) (OldName m)) o) ->
    m (Top WorkArea (NewName m) (IM m) o
        (Payload (Annotation (EvaluationScopes (NewName m) (IM m)) (NewName m)) o))
toWorkAreaTest = walk
