{-# LANGUAGE TypeFamilies, NamedFieldPuns, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Lamdu.Sugar.Names.Walk
    ( MonadNaming(..)
    , NameType(..), _GlobalDef, _TaggedVar, _TaggedNominal, _Tag
    , isGlobal
    , FunctionSignature(..), Disambiguator
    , IsUnambiguous(..)
    , NameConvertor, CPSNameConvertor
    , Walk(..), toExpression, toWorkArea
    ) where

import qualified Control.Lens as Lens
import qualified Data.Set as Set
import           Hyper.Class.Morph (morphTraverse1)
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

instance (a ~ OldName m, b ~ NewName m) => Walk m (ParamRef a o) (ParamRef b o) where
    walk p = (pNameRef . nrName) (opGetName Nothing (binderAmiguity (p ^. pBinderMode)) TaggedVar) p

binderVarType :: BinderVarForm name m -> NameType
binderVarType GetLet = TaggedVar
binderVarType (GetDefinition _) = GlobalDef

instance
    (a ~ OldName m, b ~ NewName m) =>
    Walk m (CompositeFields a (Annotated p # Type a)) (CompositeFields b (Annotated p # Type b)) where
    walk (CompositeFields fields mExt) =
        CompositeFields
        <$> traverse toField fields
        <*> Lens._Just (opGetName Nothing MayBeAmbiguous TypeVar) mExt
        where
            toField (tag, typ) = (,) <$> toTagOf Tag tag <*> walk typ

instance (a ~ OldName m, b ~ NewName m) => Walk m (TId a) (TId b) where
    walk = tidName %%~ opGetName Nothing MayBeAmbiguous TaggedNominal

instance (a ~ OldName m, b ~ NewName m) => Walk m (Type a # Annotated p) (Type b # Annotated p) where
    walk (TVar tv) = opGetName Nothing MayBeAmbiguous TypeVar tv <&> TVar
    walk (TFun (FuncType a b)) = FuncType <$> walk a <*> walk b <&> TFun
    walk (TRecord composite) = TRecord <$> walk composite
    walk (TVariant composite) = TVariant <$> walk composite
    walk (TInst tid params) =
        TInst <$> walk tid <*> traverse f params
        where
            f (k, v) = (,) <$> opGetName Nothing MayBeAmbiguous TypeVar k <*> walk v

instance (a ~ OldName m, b ~ NewName m) => Walk m (Annotated p # Type a) (Annotated p # Type b) where
    walk (Ann (Const pl) x) = walk x <&> Ann (Const pl)

instance (a ~ OldName m, b ~ NewName m) => Walk m (Scheme a) (Scheme b) where
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
    RRecord  x -> walk x <&> RRecord
    RInject  x -> riTag (toTagOf Tag) x <&> RInject
    <&> (>>= traverse f)

instance (a ~ OldName m, b ~ NewName m) => Walk m (ResVal a) (ResVal b) where
    walk = resBody (toResBody walk)

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (EvaluationScopes a i) (EvaluationScopes b i) where
    walk evalRes =
        opRun <&>
        \run ->
        evalRes <&> traverse . traverse %~ (>>= run . walk)

toAnnotation ::
    MonadNaming m =>
    (v0 -> m v1) ->
    Annotation v0 (OldName m) ->
    m (Annotation v1 (NewName m))
toAnnotation _ AnnotationNone = pure AnnotationNone
toAnnotation _ (AnnotationType typ) = walk typ <&> AnnotationType
toAnnotation v (AnnotationVal x) = v x <&> AnnotationVal

type Pl v n = Payload (Annotation v n)

toPayload :: MonadNaming m => (v0 -> m v1) -> Pl v0 (OldName m) o -> m (Pl v1 (NewName m) o)
toPayload  = plAnnotation . toAnnotation

toNode ::
    MonadNaming m =>
    (v0 -> m v1) ->
    (ka # Annotated (Pl v0 (OldName m) o, p) ->
     m (kb # Annotated (Pl v1 (NewName m) o, p))) ->
    Annotated (Pl v0 (OldName m) o, p) # ka ->
    m (Annotated (Pl v1 (NewName m) o, p) # kb)
toNode toVal toV (Ann (Const pl) v) =
    Ann
    <$> (_1 (toPayload toVal) pl <&> Const)
    <*> toV v

type BodyW v t n m o a = Body t (Annotation v n) n (IM m) o a
type WalkBody t v0 v1 m o a = (v0 -> m v1) -> BodyW v0 t (OldName m) m o a -> m (BodyW v1 t (NewName m) m o a)

class ToBody t where
    toBody :: MonadNaming m => WalkBody t v0 v1 m o a

instance ToBody Let where
    toBody v let_@Let{_lName, _lBody, _lValue} =
        do
            (_lName, _lBody) <-
                unCPS (withTagRef MayBeAmbiguous TaggedVar _lName)
                (toExpression v _lBody)
            _lValue <- toExpression v _lValue
            pure let_{_lName, _lBody, _lValue}

instance ToBody Binder where
    toBody v (BinderLet l) = toBody v l <&> BinderLet
    toBody v (BinderTerm e) = toBody v e <&> BinderTerm

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (AddFirstParam a i o) (AddFirstParam b i o) where
    walk = _PrependParam walk

toFunction :: MonadNaming m => IsUnambiguous -> WalkBody Function v0 v1 m o a
toFunction u v func@Function{_fParams, _fBody, _fAddFirstParam} =
    (\(_fParams, _fBody) _fAddFirstParam ->
         func{_fParams, _fBody, _fAddFirstParam})
    <$> unCPS (withBinderParams u v _fParams) (toExpression v _fBody)
    <*> walk _fAddFirstParam

instance ToBody AssignPlain where
    toBody v AssignPlain{_apBody, _apAddFirstParam} =
        (\_apBody _apAddFirstParam -> AssignPlain{_apBody, _apAddFirstParam})
        <$> toBody v _apBody
        <*> walk _apAddFirstParam

type ExprW t v n m o a = Expr t (Annotation v n) n (IM m) o a
type WalkExpr t v0 v1 m o a = (v0 -> m v1) -> ExprW t v0 (OldName m) m o a -> m (ExprW t v1 (NewName m) m o a)

instance ToBody Assignment where
    toBody v =
        \case
        BodyPlain x -> toBody v x <&> BodyPlain
        BodyFunction x -> toFunction MayBeAmbiguous v x <&> BodyFunction

toTagOf :: MonadNaming m => NameType -> Sugar.Tag (OldName m) -> m (Sugar.Tag (NewName m))
toTagOf nameType = tagName (opGetName Nothing MayBeAmbiguous nameType)

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (TagChoice a i o p) (TagChoice b i o p) where
    walk (TagChoice opts new anon) =
        (,) <$> opRun <*> opRun
        <&>
        \(run0, run1) ->
        TagChoice
        { _tcOptions = opts >>= run0 . (traverse . toInfo) (toTagOf Tag)
        , _tcNewTag = new >>= run1 . toInfo (toTagOf Tag)
        , _tcAnon = anon
        }

toTagRefOf ::
    MonadNaming m =>
    NameType -> Sugar.TagRef (OldName m) (IM m) o ->
    m (Sugar.TagRef (NewName m) (IM m) o)
toTagRefOf nameType (Sugar.TagRef info actions jumpTo) =
    Sugar.TagRef
    <$> toTagOf nameType info
    <*> walk actions
    ?? jumpTo

withTagRef ::
    MonadNaming m =>
    IsUnambiguous -> NameType ->
    Sugar.TagRef (OldName m) (IM m) o ->
    CPS m (Sugar.TagRef (NewName m) (IM m) o)
withTagRef unambig nameType (Sugar.TagRef info actions jumpTo) =
    Sugar.TagRef
    <$> tagName (opWithName unambig nameType) info
    <*> liftCPS (walk actions)
    ?? jumpTo

instance ToBody AnnotatedArg where
    toBody v (AnnotatedArg tag e) =
        AnnotatedArg
        <$> toTagOf Tag tag
        <*> toExpression v e

instance ToBody LabeledApply where
    toBody v app@LabeledApply{_aFunc, _aMOpArgs, _aAnnotatedArgs, _aPunnedArgs} =
        LabeledApply
        <$> toNode v (Lens._Wrapped (toBinderVarRef (Just (funcSignature app)))) _aFunc
        <*> (traverse . morphTraverse1 . toExpression) v _aMOpArgs
        <*> (traverse . toBody) v _aAnnotatedArgs
        <*> (traverse . pvVar) (toNode v (Lens._Wrapped walk)) _aPunnedArgs

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
        option ^. hoSearchTerms >>= traverse . traverse %%~ run1 . opGetName Nothing MayBeAmbiguous TaggedVar
    , _hoResults = option ^. hoResults <&> _2 %~ (>>= holeResultConverted (run0 . toExpression pure))
    }

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (Hole a i o) (Hole b i o) where
    walk hole =
        (,) <$> opRun <*> opRun
        <&>
        \(r0, r1) ->
        hole & Sugar.holeOptions . Lens.mapped . Lens.mapped . Lens.mapped %~ toHoleOption r0 r1

instance ToBody Fragment where
    toBody v Fragment{_fExpr, _fHeal, _fTypeMismatch, _fOptions} =
        do
            newTypeMismatch <- Lens._Just walk _fTypeMismatch
            newExpr <- toExpression v _fExpr
            h <- walk _fOptions
            pure Fragment
                { _fExpr = newExpr
                , _fTypeMismatch = newTypeMismatch
                , _fOptions = h
                , _fHeal
                }

instance ToBody CompositeItem where
    toBody v (CompositeItem del tag e) =
        CompositeItem del
        <$> toTagRefOf Tag tag
        <*> toExpression v e

instance ToBody Composite where
    toBody v (Composite items punned tail_ addItem) =
        Composite
        <$> traverse (toBody v) items
        <*> (traverse . pvVar) (toNode v (Lens._Wrapped walk)) punned
        <*> _OpenComposite (toExpression v) tail_
        <*> walk addItem

instance ToBody Nominal where
    toBody v (Nominal t e) = Nominal <$> walk t <*> toExpression v e

instance ToBody Else where
    toBody v (SimpleElse x) = toBody v x <&> SimpleElse
    toBody v (ElseIf x) = toBody v x <&> ElseIf

instance ToBody IfElse where
    toBody v (IfElse i t e) = IfElse <$> toExpression v i <*> toExpression v t <*> toExpression v e

instance ToBody PostfixFunc where
    toBody v (PfCase x) = toBody v x <&> PfCase
    toBody _ (PfFromNom x) = walk x <&> PfFromNom
    toBody _ (PfGetField x) = toTagRefOf Tag x <&> PfGetField

instance ToBody PostfixApply where
    toBody v (PostfixApply a f) =
        PostfixApply <$> toExpression v a <*> toExpression v f

binderAmiguity :: BinderMode -> IsUnambiguous
binderAmiguity LightLambda = Unambiguous
binderAmiguity _ = MayBeAmbiguous

toNullaryInject ::
    MonadNaming m =>
    (v0 -> m v1) ->
    NullaryInject (OldName m) (IM m) o # Annotated (Payload (Annotation v0 (OldName m)) o, a) ->
    m (NullaryInject (NewName m) (IM m) o # Annotated (Payload (Annotation v1 (NewName m)) o, a))
toNullaryInject v (NullaryInject t n) =
    NullaryInject
    <$> toTagRefOf Tag t
    <*> toNode v (Lens._Wrapped walk) n

instance (a ~ OldName m, b ~ NewName m, i ~ IM m) => Walk m (Leaf a i o) (Leaf b i o) where
    walk =
        \case
        LeafInject       x -> x & toTagRefOf Tag <&> LeafInject
        LeafHole         x -> x & walk <&> LeafHole
        LeafGetVar       x -> x & walk <&> LeafGetVar
        LeafLiteral      x -> x & LeafLiteral & pure
        LeafPlaceHolder    -> pure LeafPlaceHolder

instance ToBody Term where
    toBody v =
        \case
        BodyRecord       x -> x & toBody v <&> BodyRecord
        BodyIfElse       x -> x & toBody v <&> BodyIfElse
        BodySimpleApply  x -> x & (morphTraverse1 . toExpression) v <&> BodySimpleApply
        BodyPostfixApply x -> x & toBody v <&> BodyPostfixApply
        BodyLabeledApply x -> x & toBody v <&> BodyLabeledApply
        BodyPostfixFunc  x -> x & toBody v <&> BodyPostfixFunc
        BodyToNom        x -> x & toBody v <&> BodyToNom
        BodyLam          x -> x & lamFunc (toFunction (binderAmiguity (x ^. lamMode)) v) <&> BodyLam
        BodyFragment     x -> x & toBody v <&> BodyFragment
        BodyNullaryInject x -> x & toNullaryInject v <&> BodyNullaryInject
        BodyLeaf         x -> walk x <&> BodyLeaf

funcSignature :: LabeledApply v name i o a -> FunctionSignature
funcSignature apply =
    FunctionSignature
    { sIsOperator = Lens.has (aMOpArgs . Lens._Just) apply
    , sNormalArgs = apply ^.. aAnnotatedArgs . traverse . aaTag . tagVal & Set.fromList
    }

toExpression :: (MonadNaming m, ToBody e) => WalkExpr e v0 v1 m o a
toExpression = toNode <*> toBody

withParamInfo ::
    MonadNaming m =>
    IsUnambiguous ->
    ParamInfo (OldName m) (IM m) o ->
    CPS m (ParamInfo (NewName m) (IM m) o)
withParamInfo unambig (ParamInfo tag fpActions) =
    ParamInfo
    <$> withTagRef unambig TaggedVar tag
    <*> liftCPS ((fpAddNext . Sugar._AddNext) walk fpActions)

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
    IsUnambiguous ->
    (v0 -> m v1) ->
    SugarElem BinderParams v0 (OldName m) m o ->
    CPS m (SugarElem BinderParams v1 (NewName m) m o)
withBinderParams _ v (NullParam x) = withFuncParam v pure x <&> NullParam
withBinderParams u v (Params xs) = traverse (withFuncParam v (withParamInfo u)) xs <&> Params

type Top t n i o p = t (Annotation (EvaluationScopes n i) n) n i o (Pl (EvaluationScopes n i) n o, p)

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m) =>
    Walk m (Top DefinitionExpression a i o p) (Top DefinitionExpression b i o p)
     where
    walk (DefinitionExpression typ presMode content) =
        DefinitionExpression
        <$> walk typ
        <*> pure presMode
        <*> toExpression walk content

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m) =>
    Walk m (Top DefinitionBody a i o p) (Top DefinitionBody b i o p) where
        walk (DefinitionBodyBuiltin bi) =
            bi & biType %%~ walk <&> DefinitionBodyBuiltin
        walk (DefinitionBodyExpression expr) =
            walk expr <&> DefinitionBodyExpression

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m) =>
    Walk m (Top Definition a i o p) (Top Definition b i o p) where
    walk def@Definition{_drName, _drBody} =
        do
            -- NOTE: A global def binding is not considered a binder, as
            -- it exists everywhere, not just inside the binding
            _drName <- toTagRefOf GlobalDef _drName
            _drBody <- walk _drBody
            pure def{_drName, _drBody}

instance (a ~ OldName m, b ~ NewName m) => Walk m (TagPane a o) (TagPane b o) where
    walk (TagPane tag i18n setSymbol setName) =
        toTagOf Tag tag <&> \x -> TagPane x i18n setSymbol setName

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m) =>
    Walk m (Top PaneBody a i o p) (Top PaneBody b i o p) where
    walk (PaneDefinition def) = walk def <&> PaneDefinition
    walk (PaneTag x) = walk x <&> PaneTag

instance
    (a ~ OldName m, b ~ NewName m, i ~ IM m) =>
    Walk m (Top WorkArea a i o p) (Top WorkArea b i o p) where
    walk WorkArea { _waPanes, _waRepl, _waGlobals } =
        do
            run <- opRun
            panes <- (traverse . paneBody) walk _waPanes
            repl <- replExpr (toNode walk (toBody walk)) _waRepl
            let globals = _waGlobals >>= run . toGlobals
            WorkArea panes repl globals & pure
        where
            toGlobals = (traverse . nrName) (opGetName Nothing MayBeAmbiguous GlobalDef)

toWorkArea :: MonadNaming m => Top WorkArea (OldName m) (IM m) o p -> m (Top WorkArea (NewName m) (IM m) o p)
toWorkArea = walk
