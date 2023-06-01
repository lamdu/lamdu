{-# LANGUAGE TypeFamilies, NamedFieldPuns, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Lamdu.Sugar.Names.Walk
    ( MonadNameWalk(..)
    , NameType(..), _GlobalDef, _TaggedVar, _TaggedNominal, _Tag
    , isGlobal
    , FunctionSignature(..), Disambiguator
    , IsUnambiguous(..)
    , Walk(..), toWorkArea, toWorkAreaTest
    ) where

import qualified Control.Lens as Lens
import           Data.Bitraversable (Bitraversable(..))
import           Data.Kind (Type)
import           Data.Property (Property)
import qualified Data.Set as Set
import           Hyper
import           Hyper.Class.Morph (morphTraverse1)
import           Hyper.Syntax (FuncType(..))
import qualified Lamdu.Calc.Type as T
import           Lamdu.Sugar.Names.NewTag (newTagName)
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Names.CPS (CPS(..), liftCPS)
import qualified Lamdu.Sugar.Types as Sugar
import           Lamdu.Sugar.Types hiding (Tag(..), Type)

import           Lamdu.Prelude

-- TODO: Maybe remove "TaggedNominal", make it a disambiguation context?
data NameType = Tag | TaggedNominal | GlobalDef | TaggedVar | TypeVar
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

data FunctionSignature = FunctionSignature
    { sIsOperator :: Bool
    , sNormalArgs :: Set T.Tag
    } deriving (Eq, Ord, Show)

type Disambiguator = FunctionSignature

data IsUnambiguous = Unambiguous | MayBeAmbiguous

class (Monad m, Monad (IM m)) => MonadNameWalk m where
    type OldName m
    type NewName m
    type IM m :: Type -> Type
    opRun :: m (m a -> IM m a)

    opWithName :: IsUnambiguous -> NameType -> OldName m -> CPS m (NewName m)
    opGetName :: Maybe Disambiguator -> IsUnambiguous -> NameType -> OldName m -> m (NewName m)
    opWithNewTag :: T.Tag -> Text -> m a -> m a

    tagSuffixes :: m TagSuffixes
    tagSuffixes = pure mempty

class Walk m s t where
    walk :: MonadNameWalk m => s -> m t

instance Walk m () () where walk = pure
instance Walk m (Property o ParamKind) (Property o ParamKind) where walk = pure

instance Walk m s t => Walk m (s, x, y) (t, x, y) where
    walk = _1 walk

binderVarType :: VarForm name m -> NameType
binderVarType (GetDefinition _) = GlobalDef
binderVarType _ = TaggedVar

instance (a ~ OldName m, b ~ NewName m) => Walk m (CompositeFields a # Annotated p) (CompositeFields b # Annotated p) where
    walk (CompositeFields fields mExt) =
        CompositeFields
        <$> traverse (bitraverse (toTagOf Tag) walk) fields
        <*> Lens._Just (opGetName Nothing MayBeAmbiguous TypeVar) mExt

instance (a ~ OldName m, b ~ NewName m) => Walk m (TId a) (TId b) where
    walk = tidName %%~ opGetName Nothing MayBeAmbiguous TaggedNominal

instance (a ~ OldName m, b ~ NewName m) => Walk m (Sugar.Type a # Annotated p) (Sugar.Type b # Annotated p) where
    walk (TVar tv) = opGetName Nothing MayBeAmbiguous TypeVar tv <&> TVar
    walk (TFun (FuncType a b)) = FuncType <$> walk a <*> walk b <&> TFun
    walk (TRecord composite) = TRecord <$> walk composite
    walk (TVariant composite) = TVariant <$> walk composite
    walk (TInst tid params) =
        TInst
        <$> walk tid
        <*> traverse (bitraverse (toTagOf Tag) walk) params

instance (a ~ OldName m, b ~ NewName m) => Walk m (Annotated p # Sugar.Type a) (Annotated p # Sugar.Type b) where
    walk (Ann (Const pl) x) = walk x <&> Ann (Const pl)

instance (a ~ OldName m, b ~ NewName m) => Walk m (Scheme a) (Scheme b) where
    walk (Scheme tvs typ) = Scheme tvs <$> walk typ

instance (a ~ OldName m, b ~ NewName m) => Walk m (DefinitionOutdatedType a o p) (DefinitionOutdatedType b o p) where
    walk (DefinitionOutdatedType whenUsed current useCur) =
        DefinitionOutdatedType <$> walk whenUsed <*> walk current ?? useCur

toBinderVarRef ::
    MonadNameWalk m =>
    Maybe Disambiguator ->
    GetVar (OldName m) o ->
    m (GetVar (NewName m) o)
toBinderVarRef mDisambig (GetVar name form goto var inline) =
    GetVar
    <$> opGetName mDisambig amb (binderVarType form) name
    <*> (_GetDefinition . _DefTypeChanged %%~ walk) form
    ?? goto
    ?? var
    ?? inline
    where
        amb | Lens.has _GetLightParam form = Unambiguous
            | otherwise = MayBeAmbiguous

instance (a ~ OldName m, b ~ NewName m) => Walk m (GetVar a o) (GetVar b o) where
    walk = toBinderVarRef Nothing

instance (a ~ OldName m, b ~ NewName m) => Walk m (ResInject a # Annotated p) (ResInject b # Annotated p) where
    walk (ResInject t v) = ResInject <$> toTagOf Tag t <*> (Lens._Just . annValue) walk v

instance (a ~ OldName m, b ~ NewName m) => Walk m (ResTable a # Annotated p) (ResTable b # Annotated p) where
    walk (ResTable t v) =
        ResTable <$> traverse (toTagOf Tag) t <*> (traverse . traverse . annValue) walk v

instance (a ~ OldName m, b ~ NewName m) => Walk m (Result a # Annotated p) (Result b # Annotated p) where
    walk =
        \case
        RFunc    x -> RFunc x & pure
        RError   x -> RError x & pure
        RBytes   x -> RBytes x & pure
        RFloat   x -> RFloat x & pure
        RText    x -> RText x & pure
        RChar    x -> RChar x & pure
        RArray   x -> (traverse . annValue) walk x <&> RArray
        RList    x -> annValue walk x <&> RList
        RTree    x -> (morphTraverse1 @_ @_ @(Result a) @(Result b) . annValue) walk x <&> RTree
        RTable   x -> walk x <&> RTable
        RRecord  x -> traverse (bitraverse (toTagOf Tag) (annValue walk)) x <&> RRecord
        RInject  x -> walk x <&> RInject

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (EvaluationScopes a i) (EvaluationScopes b i) where
    walk evalRes =
        opRun <&>
        \run ->
        evalRes <&> traverse . traverse %~ (>>= run . annValue walk)

instance (a ~ OldName m, b ~ NewName m, Walk m va vb) => Walk m (Annotation va a) (Annotation vb b) where
    walk AnnotationNone = pure AnnotationNone
    walk (AnnotationType typ) = walk typ <&> AnnotationType
    walk (AnnotationVal x) = walk x <&> AnnotationVal

instance Walk m va vb => Walk m (Payload va o) (Payload vb o) where
    walk = plAnnotation walk

toNode ::
    (MonadNameWalk m, Walk m pa pb) =>
    (ka # Annotated pa -> m (kb # Annotated pb)) ->
    Annotated pa # ka -> m (Annotated pb # kb)
toNode toV (Ann (Const pl) v) = Ann <$> (walk pl <&> Const) <*> toV v

type BodyW t v n (i :: Type -> Type) (o :: Type -> Type) a = t v n i o # Annotated a
type WalkBody t m o v0 v1 a0 a1 = BodyW t v0 (OldName m) (IM m) o a0 -> m (BodyW t v1 (NewName m) (IM m) o a1)

class ToBody t where
    toBody :: (MonadNameWalk m, Walk m v0 v1, Walk m a0 a1) => WalkBody t m o v0 v1 a0 a1

instance ToBody Let where
    toBody let_@Let{_lNames, _lBody, _lValue} =
        -- In "let x = let x = _"
        -- There isn't a real collision between the inner and outer "x"s
        -- because they don't live in the same scopes.
        -- However to avoid confusion we do treat it as a collision anyhow.
        (,) <$> toExpression _lValue <*> toExpression _lBody
        & unCPS (withParams MayBeAmbiguous _lNames)
        <&> \(_lNames, (_lValue, _lBody)) -> let_{_lNames, _lBody, _lValue}

instance ToBody Binder where
    toBody = bBody toBody

instance ToBody BinderBody where
    toBody (BinderLet l) = toBody l <&> BinderLet
    toBody (BinderTerm e) = toBody e <&> BinderTerm

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (AddParam a i o) (AddParam b i o) where
    walk x = opRun <&> \run -> x & _AddNext %~ (>>= run . walk)

toFunction :: (MonadNameWalk m, Walk m v0 v1, Walk m a0 a1) => IsUnambiguous -> WalkBody Function m o v0 v1 a0 a1
toFunction u func@Function{_fParams, _fBody} =
    unCPS (withParams u _fParams) (toExpression _fBody)
    <&> \(_fParams, _fBody) -> func{_fParams, _fBody}

instance ToBody AssignPlain where
    toBody = apBody toBody

instance ToBody Assignment where
    toBody =
        \case
        BodyPlain x -> toBody x <&> BodyPlain
        BodyFunction x -> toFunction MayBeAmbiguous x <&> BodyFunction

toTagOf :: MonadNameWalk m => NameType -> Sugar.Tag (OldName m) -> m (Sugar.Tag (NewName m))
toTagOf = tagName . opGetName Nothing MayBeAmbiguous

instance (a ~ OldName m, b ~ NewName m) => Walk m (TagChoice a o) (TagChoice b o) where
    walk (TagChoice opts new) =
        TagChoice
        <$> (traverse . toInfo) (toTagOf Tag) opts
        <*> toInfo (toTagOf Tag) new

walkOpts ::
    (ToBody t, MonadNameWalk m, a ~ OldName m, b ~ NewName m, i ~ IM m) =>
    m ((Query -> i [Option t (OldName m) i o]) -> Query -> i [Option t (NewName m) i o])
walkOpts =
    opRun <&>
    \run -> Lens.imapped %@~ (\query results -> results >>= run . traverse (onResult query))
    where
        onResult query opt =
            optionExpr toExpression opt &
            case opt ^. optionMNewTag of
            Nothing -> id
            Just tag -> opWithNewTag tag (newTagName (query ^. qSearchTerm))

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (Hole a i o) (Hole b i o) where
    walk h =
        Hole
        <$> (walkOpts <&> ((h ^. holeOptions) <&>))
        <*> tagSuffixes

toTagRefOf ::
    MonadNameWalk m =>
    NameType -> Sugar.TagRef (OldName m) (IM m) o ->
    m (Sugar.TagRef (NewName m) (IM m) o)
toTagRefOf nameType (Sugar.TagRef info replace jumpTo) =
    Sugar.TagRef
    <$> toTagOf nameType info
    <*> (opRun <&> \run -> replace >>= run . walk)
    ?? jumpTo

withTagRef ::
    MonadNameWalk m =>
    IsUnambiguous -> NameType ->
    Sugar.TagRef (OldName m) (IM m) o ->
    CPS m (Sugar.TagRef (NewName m) (IM m) o)
withTagRef unambig nameType (Sugar.TagRef info replace jumpTo) =
    Sugar.TagRef
    <$> tagName (opWithName unambig nameType) info
    <*> liftCPS (opRun <&> \run -> replace >>= run . walk)
    ?? jumpTo

toOptionalTag ::
    MonadNameWalk f =>
    NameType -> OptionalTag (OldName f) (IM f) o ->
    f (OptionalTag (NewName f) (IM f) o)
toOptionalTag = Sugar.oTag . toTagRefOf

withOptionalTag ::
    MonadNameWalk m =>
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
    toBody Fragment{_fExpr, _fHeal, _fTypeMismatch, _fOptions, _fOptApply} =
        do
            newTypeMismatch <- Lens._Just walk _fTypeMismatch
            newExpr <- toExpression _fExpr
            w <- walkOpts
            s <- tagSuffixes
            run <- opRun
            pure Fragment
                { _fExpr = newExpr
                , _fTypeMismatch = newTypeMismatch
                , _fHeal
                , _fOptions = _fOptions <&> w
                , _fOptApply = _fOptApply >>= run . optionExpr toExpression
                , _fTagSuffixes = s
                }

instance (a ~ OldName m, b ~ NewName m) => Walk m (TypeMismatch a) (TypeMismatch b) where
    walk (TypeMismatch t r) =
        TypeMismatch
        <$> walk t
        <*> _TypeVarSkolemEscape walk r

instance ToBody FragOpt where
    toBody (FragPostfix x) = traverse toExpression x <&> FragPostfix
    toBody (FragInject x) = walk x <&> FragInject
    toBody (FragWrapInRec x) = walk x <&> FragWrapInRec
    toBody (FragApplyFunc x) = walk x <&> FragApplyFunc
    toBody (FragOp x) = toBody x <&> FragOp
    toBody (FragToNom x) = walk x <&> FragToNom
    toBody (FragIf x) = toExpression x <&> FragIf
    toBody (FragArgument x) = toBody x <&> FragArgument
    toBody FragLam = pure FragLam
    toBody FragDefer = pure FragDefer

instance ToBody FragOperator where
    toBody (FragOperator f a t) =
        FragOperator
        <$> toNode (Lens._Wrapped (toBinderVarRef Nothing)) f
        <*> toExpression a
        <*> traverse (toTagOf Tag) t

instance ToBody HoleOpt where
    toBody (HoleBinder x) = toBody x <&> HoleBinder
    toBody (HoleVarsRecord x) = traverse (opGetName Nothing MayBeAmbiguous TaggedVar) x <&> HoleVarsRecord

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
        <*> _OpenCompositeTail toExpression tail_

instance ToBody Nominal where
    toBody (Nominal t e) = Nominal <$> walk t <*> toExpression e

instance ToBody Else where
    toBody (SimpleElse x) = toBody x <&> SimpleElse
    toBody (ElseIf x) = eIfElse toBody x <&> ElseIf

instance ToBody IfElse where
    toBody (IfElse i t e) = IfElse <$> toExpression i <*> toExpression t <*> toExpression e

instance ToBody PostfixFunc where
    toBody (PfCase x) = toBody x <&> PfCase
    toBody (PfFromNom x) = walk x <&> PfFromNom
    toBody (PfGetField x) = toTagRefOf Tag x <&> PfGetField

instance ToBody PostfixApply where
    toBody (PostfixApply a f) =
        PostfixApply <$> toExpression a <*> toExpression f

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
        BodyFragment     x -> x & toBody <&> BodyFragment
        BodyNullaryInject x -> walk x <&> BodyNullaryInject
        BodyLeaf         x -> walk x <&> BodyLeaf
        BodyLam x ->
            x
            & lamFunc (toFunction (if x ^. lamLightweight then Unambiguous else MayBeAmbiguous))
            <&> BodyLam

funcSignature :: LabeledApply v name i o a -> FunctionSignature
funcSignature apply =
    FunctionSignature
    { sIsOperator = Lens.has (aMOpArgs . Lens._Just) apply
    , sNormalArgs = apply ^.. aAnnotatedArgs . traverse . aaTag . tagVal & Set.fromList
    }

toExpression ::
    (MonadNameWalk m, Walk m v0 v1, Walk m a0 a1, ToBody e) =>
    Annotated a0 # e v0 (OldName m) (IM m) o -> m (Annotated a1 # e v1 (NewName m) (IM m) o)
toExpression = toNode toBody

withRecordParam ::
    (MonadNameWalk m, Walk m v0 v1) =>
    IsUnambiguous ->
    TaggedItem (OldName m) (IM m) o (LhsField (OldName m) v0) ->
    CPS m (TaggedItem (NewName m) (IM m) o (LhsField (NewName m) v1))
withRecordParam unambig (TaggedItem t d a v) =
    (`TaggedItem` d)
    <$> withTagRef unambig TaggedVar t
    <*> liftCPS (opRun <&> \run -> a >>= run . walk)
    <*> withLhsField unambig v

withLhsField ::
    (MonadNameWalk m, Walk m v0 v1) =>
    IsUnambiguous ->
    LhsField (OldName m) v0 ->
    CPS m (LhsField (NewName m) v1)
withLhsField unambig (LhsField v s) =
    LhsField
    <$> withFuncParam v
    <*> (Lens._Just . traverse)
        (bitraverse (tagName (opWithName unambig TaggedVar)) (withLhsField unambig)) s

withFuncParam ::
    (MonadNameWalk m, Walk m v0 v1) =>
    FuncParam v0 ->
    CPS m (FuncParam v1)
withFuncParam = fpAnnotation (liftCPS . walk)

withLhsRecord ::
    (MonadNameWalk m, Walk m v0 v1) =>
    IsUnambiguous ->
    TaggedList (OldName m) (IM m) o (LhsField (OldName m) v0) ->
    CPS m (TaggedList (NewName m) (IM m) o (LhsField (NewName m) v1))
withLhsRecord u (TaggedList addFirst items) =
    TaggedList
    <$> liftCPS (opRun <&> \run -> addFirst >>= run . walk)
    <*> (Lens._Just . SugarLens.taggedListBodyItems) (withRecordParam u) items

withParams ::
    (MonadNameWalk m, Walk m v0 v1) =>
    IsUnambiguous ->
    LhsNames (OldName m) (IM m) o v0 ->
    CPS m (LhsNames (NewName m) (IM m) o v1)
withParams u (LhsRecord r) = withLhsRecord u r <&> LhsRecord
withParams u (LhsVar v) =
    (\_vParam _vTag _vAddPrev _vAddNext -> LhsVar v{_vParam, _vTag, _vAddPrev, _vAddNext})
    <$> withFuncParam (v ^. vParam)
    <*> withOptionalTag u TaggedVar (v ^. vTag)
    <*> liftCPS (opRun <&> \run -> v ^. vAddPrev & _AddNext %~ (>>= run . walk))
    <*> liftCPS (opRun <&> \run -> v ^. vAddNext & _AddNext %~ (>>= run . walk))

type Top t n i (o :: Type -> Type) p = t (Annotation (EvaluationScopes n i) n) n i o p

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m, Walk m pa pb) =>
    Walk m (Top DefinitionExpression a i o pa) (Top DefinitionExpression b i o pb)
     where
    walk (DefinitionExpression typ presMode content varInfo res) =
        DefinitionExpression
        <$> walk typ
        <*> pure presMode
        <*> toExpression content
        ?? varInfo
        ?? res

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

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (NominalPane a i o) (NominalPane b i o) where
    walk nomPane@NominalPane{_npName, _npParams, _npBody} =
        do
            _npName <- toOptionalTag TaggedNominal _npName
            _npParams <- walk _npParams
            _npBody <- Lens._Just walk _npBody
            pure nomPane{_npName, _npParams, _npBody}

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m, Walk m pa pb) =>
    Walk m (Top PaneBody a i o pa) (Top PaneBody b i o pb) where
    walk (PaneDefinition def) = walk def <&> PaneDefinition
    walk (PaneTag x) = PaneTag x & pure
    walk (PaneNominal x) = walk x <&> PaneNominal

toGlobals ::
    (MonadNameWalk f, Traversable t, i ~ IM f) =>
    NameType -> i (t (NameRef (OldName f) a)) -> f (i (t (NameRef (NewName f) a)))
toGlobals nameType x =
    opRun <&>
    \run -> x >>= run . (traverse . nrName) (opGetName Nothing MayBeAmbiguous nameType)

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m) =>
    Walk m (Globals a i) (Globals b i) where
    walk (Globals d n t) =
        Globals <$> toGlobals GlobalDef d <*> toGlobals TaggedNominal n <*> toGlobals Tag t

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m, Walk m pa pb) =>
    Walk m (Top WorkArea a i o pa) (Top WorkArea b i o pb) where
    walk WorkArea { _waPanes, _waGlobals, _waOpenPane } =
        WorkArea
        <$> (traverse . paneBody) walk _waPanes
        <*> walk _waGlobals
        ?? _waOpenPane

toWorkArea ::
    MonadNameWalk m =>
    Top WorkArea (OldName m) (IM m) o
        (Annotation (EvaluationScopes (OldName m) (IM m)) (OldName m), a, b) ->
    m (Top WorkArea (NewName m) (IM m) o
        (Annotation (EvaluationScopes (NewName m) (IM m)) (NewName m), a, b))
toWorkArea = walk

toWorkAreaTest ::
    MonadNameWalk m =>
    Top WorkArea (OldName m) (IM m) o
        (Payload (Annotation (EvaluationScopes (OldName m) (IM m)) (OldName m)) o) ->
    m (Top WorkArea (NewName m) (IM m) o
        (Payload (Annotation (EvaluationScopes (NewName m) (IM m)) (NewName m)) o))
toWorkAreaTest = walk
