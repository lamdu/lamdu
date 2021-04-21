{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Lamdu.Sugar.Convert.Hole
    ( convert
      -- Used by Convert.Fragment:
    , Preconversion, ResultGen
    , ResultProcessor(..)
    , mkOptions, detachValIfNeeded, mkHoleSearchTerms, loadNewDeps, loadDeps
    , mkResult
    , mkOption, addWithoutDups
    , assertSuccessfulInfer
    , genLamVar
    ) where

import           Control.Applicative (Alternative(..))
import qualified Control.Lens as Lens
import           Control.Monad (filterM)
import           Control.Monad.ListT (ListT(..))
import           Control.Monad.Once (OnceT, _OnceT, MonadOnce(..), onceList)
import           Control.Monad.State (State, StateT(..), mapStateT)
import qualified Control.Monad.State as State
import qualified Data.ByteString.Extended as BS
import qualified Data.List.Class as ListClass
import qualified Data.Map as Map
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import           Data.Semigroup (Endo)
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import qualified Data.UUID as UUID
import           Hyper
import           Hyper.Infer
import           Hyper.Recurse (wrap, unwrap)
import           Hyper.Type.AST.FuncType (FuncType(..))
import           Hyper.Type.AST.Nominal (NominalDecl, nScheme)
import           Hyper.Type.AST.Row (freExtends)
import           Hyper.Type.AST.Scheme (sTyp)
import           Hyper.Type.Functor (_F)
import           Hyper.Type.Prune (Prune(..))
import           Hyper.Unify (Unify(..), BindingDict(..), unify)
import           Hyper.Unify.Binding (UVar)
import           Hyper.Unify.Term (UTerm(..), UTermBody(..))
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Cache as Cache
import           Lamdu.Calc.Definition (Deps(..), depsGlobalTypes, depsNominals)
import           Lamdu.Calc.Identifier (Identifier(..))
import           Lamdu.Calc.Infer (InferState, runPureInfer, PureInfer)
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Term.Eq (couldEq)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.IRef (HRef(..), DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import           Lamdu.Sugar.Convert.Binder (convertBinder)
import           Lamdu.Sugar.Convert.Binder.Params (mkVarInfo)
import qualified Lamdu.Sugar.Convert.Completions as Completions
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, convertPayloads)
import           Lamdu.Sugar.Convert.Hole.ResultScore (resultScore)
import qualified Lamdu.Sugar.Convert.Hole.Suggest as Suggest
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified Lamdu.Sugar.Lens as SugarLens
import           Lamdu.Sugar.Types
import           Revision.Deltum.Hyper (Write(..))
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction
import           Text.PrettyPrint.HughesPJClass (prettyShow)

import           Lamdu.Prelude

type T = Transaction

type Preconversion m a =
    Ann (Input.Payload m a) # V.Term ->
    Ann (Input.Payload m ()) # V.Term

type ResultGen m = StateT InferState (ListT (OnceT (T m)))

convert ::
    (Monad m, Monoid a, Typeable m) =>
    ConvertM.PositionInfo -> Input.Payload m a # V.Term ->
    ConvertM m (ExpressionU EvalPrep m a)
convert posInfo holePl =
    mkOptions posInfo holeResultProcessor holePl
    <&> Lens.mapped . Lens.mapped . Lens.mapped %~ snd
    >>= ConvertM.convertOnce
    <&> BodyLeaf . LeafHole . Hole
    >>= addActions (Const ()) holePl
    <&> annotation . pActions . delete .~ CannotDelete
    <&> annotation . pActions . mApply .~ Nothing

data ResultProcessor m = forall a. ResultProcessor
    { rpEmptyPl :: a
    , rpPostProcess ::
        Ann (Write m :*: InferResult UVar) # V.Term ->
        ResultGen m (Ann ((Const a :*: Write m) :*: InferResult UVar) # V.Term)
    , rpPreConversion :: Preconversion m a
    }

holeResultProcessor :: Monad m => ResultProcessor m
holeResultProcessor =
    ResultProcessor
    { rpEmptyPl = ()
    , rpPostProcess = pure . (hflipped %~ hmap (const (_1 %~ (Const () :*:))))
    , rpPreConversion = id
    }

mkHoleSearchTerms :: Monad m => ConvertM.Context m -> Input.Payload m a # V.Term -> Val () -> T m [HoleTerm InternalName]
mkHoleSearchTerms ctx pl x =
    case x ^. hVal of
    V.BLeaf (V.LVar v) ->
        case lookup v (pl ^. Input.localsInScope) of
        Nothing -> taggedName Nothing v <&> (:[]) . HoleGetDef
        Just t
            | v `elem` recordParams -> pure [HoleParamsRecord]
            | otherwise -> mkVarInfo t <&> Just >>= (`taggedName` v) <&> (:[]) . HoleName
    V.BLeaf (V.LFromNom n) ->
        taggedName Nothing n <&> (:[]) . HoleFromNom <&> (<> [HoleIf | n == Builtins.boolTid])
    V.BLeaf V.LRecEmpty -> pure [HoleRecord]
    V.BLeaf V.LAbsurd -> pure [HoleEmptyCase]
    V.BLeaf V.LHole -> pure []
    V.BLeaf V.LLiteral{} -> pure [] -- Literals not created from hole results
    V.BLeaf (V.LInject i) -> pure [HoleInject (nameWithoutContext i)]
    V.BLeaf (V.LGetField f) -> pure [HoleGetField (nameWithoutContext f)]
    V.BToNom (V.ToNom n b) -> (taggedName Nothing n <&> (:[]) . HoleName) <> mkHoleSearchTerms ctx pl b
    V.BApp (V.App (Ann _ V.BLam{}) _) -> pure [HoleLet]
    V.BApp (V.App (Ann _ (V.BLeaf (V.LGetField f))) (Ann _ (V.BLeaf (V.LVar v))))
        | v `elem` recordParams -> pure [HoleName (nameWithoutContext f)]
    V.BApp a -> mkHoleSearchTerms ctx pl (a ^. V.appFunc)
    V.BLam{} -> pure [HoleLambda]
    V.BRecExtend{} -> pure [HoleRecord]
    V.BCase{} -> pure [HoleCase]
    where
        recordParams =
            ctx ^.. ConvertM.scScopeInfo . ConvertM.siTagParamInfos . traverse . ConvertM._TagFieldParam
            <&> ConvertM.tpiFromParameters

mkOption ::
    (Monad m, Typeable m) =>
    HoleResultTier -> ConvertM.Context m -> ResultProcessor m ->
    Input.Payload m a # V.Term -> Val () ->
    OnceT (T m) (HoleOption InternalName (OnceT (T m)) (T m))
mkOption tier sugarContext resultProcessor holePl x =
    HoleOption
    <$> (lift Transaction.newKey <&> EntityId.EntityId)
    <*> once (lift (mkHoleSearchTerms sugarContext holePl x))
    <*> onceList (mkResults tier resultProcessor sugarContext holePl x)

mkHoleSuggesteds ::
    (Monad m, Typeable m) =>
    ConvertM.Context m -> ResultProcessor m ->
    Input.Payload m a # V.Term ->
    OnceT (T m) [(Val (), HoleOption InternalName (OnceT (T m)) (T m))]
mkHoleSuggesteds sugarContext resultProcessor holePl =
    holePl ^. Input.inferredTypeUVar
    & Completions.suggestForType genLamVar
    & runPureInfer (holePl ^. Input.inferScope) inferCtx
    -- TODO: Change ConvertM to be stateful rather than reader on the
    -- sugar context, to collect union-find updates.
    -- TODO: use a specific monad here that has no Either
    & assertSuccessfulInfer
    & fst
    & sequenceA & lift
    <&> Lens.mapped . hflipped %~ hmap (const (const (Const ()))) -- TODO: "Avoid re-inferring known type here"
    >>= traverse
        (\x -> mkOption HoleResultSuggested sugarContext resultProcessor holePl x <&> (,) x)
    where
        inferCtx = sugarContext ^. ConvertM.scInferContext

strip :: Recursively HFunctor h => Ann a # h -> Pure # h
strip = unwrap (const (^. hVal))

addWithoutDups ::
    [(Val (), HoleOption i o a)] ->
    [(Val (), HoleOption i o a)] ->
    [(Val (), HoleOption i o a)]
addWithoutDups new old
    | null nonHoleNew = old
    | otherwise = nonHoleNew ++ filter (not . equivalentToNew) old
    where
        equivalentToNew x =
            any (couldEq (strip (x ^. _1))) (nonHoleNew ^.. Lens.traverse . _1 <&> strip)
        nonHoleNew = filter (Lens.nullOf (_1 . ExprLens.valHole)) new

isLiveGlobal :: Monad m => DefI m -> T m Bool
isLiveGlobal defI =
    Anchors.assocDefinitionState defI
    & Property.getP
    <&> (== LiveDefinition)

getListing ::
    Monad m =>
    (Anchors.CodeAnchors f -> MkProperty' (T m) (Set a)) ->
    ConvertM.Context f -> T m [a]
getListing anchor sugarContext =
    sugarContext ^. Anchors.codeAnchors
    & anchor & Property.getP <&> Set.toList

getNominals :: Monad m => ConvertM.Context m -> T m [(T.NominalId, Pure # NominalDecl T.Type)]
getNominals sugarContext =
    getListing Anchors.tids sugarContext
    >>= traverse (\nomId -> (,) nomId <$> Load.nominal nomId)
    <&> map (Lens.sequenceAOf _2) <&> (^.. traverse . Lens._Just)

getGlobals :: Monad m => ConvertM.Context m -> T m [DefI m]
getGlobals sugarContext =
    getListing Anchors.globals sugarContext >>= filterM isLiveGlobal

getTags :: Monad m => ConvertM.Context m -> T m [T.Tag]
getTags = getListing Anchors.tags

mkNominalOptions :: [(T.NominalId, Pure # NominalDecl T.Type)] -> [HPlain V.Term]
mkNominalOptions nominals =
    do
        (tid, Pure nominal) <- nominals
        mkDirectNoms tid ++ mkToNomInjections tid nominal
    where
        mkDirectNoms tid =
            [ V.BLeafP (V.LFromNom tid) `V.BAppP` V.BLeafP V.LHole
            , V.BLeafP V.LHole & V.BToNomP tid
            ]
        mkToNomInjections tid nominal =
            nominal ^..
            nScheme . sTyp . _Pure . T._TVariant .
            T.flatRow . freExtends >>= Map.keys
            <&> V.BLeafP . V.LInject
            <&> (`V.BAppP` V.BLeafP V.LHole)
            <&> V.BToNomP tid

genLamVar :: Monad m => T m V.Var
genLamVar = Transaction.newKey <&> V.Var . Identifier . BS.strictify . UUID.toByteString

mkBaseForms :: Monad m => ConvertM.PositionInfo -> T m [HPlain V.Term]
mkBaseForms posInfo =
    [ genLamVar <&> \v -> V.BLamP v Pruned (V.BLeafP V.LHole)
    , V.BLeafP V.LAbsurd & pure
    ] <>
    [ genLamVar <&> \v -> V.BLamP v Pruned (V.BLeafP V.LHole) `V.BAppP` V.BLeafP V.LHole
    | posInfo == ConvertM.BinderPos
    ] & sequenceA

mkOptions ::
    (Monad m, Typeable m) =>
    ConvertM.PositionInfo -> ResultProcessor m ->
    Input.Payload m a # V.Term ->
    ConvertM m (OnceT (T m) (OptionFilter -> [(Val (), HoleOption InternalName (OnceT (T m)) (T m))]))
mkOptions posInfo resultProcessor holePl =
    Lens.view id
    <&>
    \sugarContext ->
    do
        nominalOptions <-
            getNominals sugarContext & lift <&> mkNominalOptions
            <&> Lens.mapped %~ (,) HoleResultGlobal
        globals <- getGlobals sugarContext & lift
        tags <- getTags sugarContext & lift
        suggesteds <- mkHoleSuggesteds sugarContext resultProcessor holePl
        baseForms <-
            mkBaseForms posInfo & lift
            <&> Lens.mapped %~ (,) HoleResultSyntax
        let mk l =
                l
                <&> _2 %~ wrap (const (Ann (Const ()))) . (^. hPlain)
                & traverse (\(tier, x) -> mkOption tier sugarContext resultProcessor holePl x <&> (,) x)
                <&> addWithoutDups suggesteds
        let globs = globals <&> (,) HoleResultGlobal . V.BLeafP . V.LVar . ExprIRef.globalId
        let common = globs <> nominalOptions <> baseForms
        let locals =
                holePl ^. Input.localsInScope
                <&> fst
                >>= getLocalScopeGetVars sugarContext
                <&> (,) (HoleResultLocal 0) -- TODO: inner scopes more preferred
        base <- locals <> common & mk
        injs <- tags <&> (,) HoleResultSyntax . V.BLeafP . V.LInject & mk
        dots <- (tags <&> (,) HoleResultSyntax . V.BLeafP . V.LGetField) <> common & mk
        pure (
            \case
            OptsNormal -> base
            OptsDot -> dots
            OptsInject -> injs
            )

-- TODO: Generalize into a separate module?
loadDeps :: Monad m => [V.Var] -> [T.NominalId] -> T m Deps
loadDeps vars noms =
    Deps
    <$> (traverse loadVar vars <&> Map.fromList)
    <*> (traverse loadNom noms <&> Map.fromList . ListClass.catMaybes)
    where
        loadVar globalId =
            ExprIRef.defI globalId & Transaction.readIRef
            <&> (^. Def.defType) <&> (,) globalId
        loadNom nomId =
            Load.nominal nomId
            <&> Lens.mapped %~ (,) nomId

type Getting' r a = Lens.Getting a r a
type Folding' r a = Lens.Getting (Endo [a]) r a

-- TODO: Generalize into a separate module?
loadNewDeps ::
    forall m a k.
    Monad m => Deps -> V.Scope # k -> Ann a # V.Term -> T m Deps
loadNewDeps currentDeps scope x =
    loadDeps newDepVars newNoms
    <&> mappend currentDeps
    where
        scopeVars = scope ^. V.scopeVarTypes & Map.keysSet
        newDeps ::
            Ord r =>
            Getting' Deps (Map r x) -> Folding' (Ann a # V.Term) r -> [r]
        newDeps depsLens valLens =
            Set.fromList (x ^.. valLens) `Set.difference` Map.keysSet (currentDeps ^. depsLens)
            & Set.toList
        newDepVars = newDeps depsGlobalTypes (ExprLens.valGlobals scopeVars)
        newNoms = newDeps depsNominals ExprLens.valNominals

assertSuccessfulInfer ::
    HasCallStack =>
    Either (Pure # T.TypeError) a -> a
assertSuccessfulInfer = either (error . prettyShow) id

loadInfer ::
    Monad m =>
    ConvertM.Context m -> V.Scope # UVar ->
    Ann a # V.Term ->
    T m (Deps, Either (Pure # T.TypeError)
        ((Ann (a :*: InferResult UVar) # V.Term, V.Scope # UVar), InferState))
loadInfer sugarContext scope v =
    loadNewDeps sugarDeps scope v
    <&>
    \deps ->
    ( deps
    , memoInfer (Definition.Expr v deps)
        & runPureInfer scope (sugarContext ^. ConvertM.scInferContext)
    )
    where
        memoInfer = Cache.infer (sugarContext ^. ConvertM.scCacheFunctions)
        sugarDeps = sugarContext ^. ConvertM.scFrozenDeps . Property.pVal

getLocalScopeGetVars :: ConvertM.Context m -> V.Var -> [HPlain V.Term]
getLocalScopeGetVars sugarContext par
    | sugarContext ^. ConvertM.scScopeInfo . ConvertM.siNullParams . Lens.contains par = []
    | otherwise = (fieldTags <&> V.LGetField <&> V.BLeafP <&> (`V.BAppP` var)) <> [var]
    where
        var = V.LVar par & V.BLeafP
        fieldTags =
            sugarContext ^@..
            ConvertM.scScopeInfo . ConvertM.siTagParamInfos .>
            ( Lens.itraversed <.
                ConvertM._TagFieldParam . Lens.to ConvertM.tpiFromParameters ) <.
                Lens.filtered (== par)
            <&> fst

randomizeVars ::
    Monad m =>
    (a # V.Term -> Bool) -> Ann a # V.Term -> T m (Ann a # V.Term)
randomizeVars p (Ann a b) =
    case b of
    V.BLam (V.TypedLam _ t i) | p a ->
        V.TypedLam
        <$> genLamVar
        <*> pure t
        <*> randomizeVars p i
        <&> V.BLam
    _ ->
        htraverse
        ( \case
            HWitness V.W_Term_Term -> randomizeVars p
            _ -> pure
        ) b
    <&> Ann a

-- Hack: Randomize variables until Fragments redesign
randomizeNewVars :: Monad m => Ann (Write n :*: a) # V.Term -> T m (Ann (Write n :*: a) # V.Term)
randomizeNewVars = randomizeVars (Lens.has (_1 . ExprIRef._WriteNew))

-- | Runs inside a forked transaction
writeResult ::
    Monad m =>
    InferState -> HRef m # V.Term ->
    Ann ((Const a :*: Write m) :*: InferResult UVar) # V.Term ->
    T m (Ann (Input.Payload m a) # V.Term)
writeResult inferCtx holeStored inferredVal =
    do
        writtenExpr <-
            inferredVal
            & hflipped %~ hmap (\_ ((Const a :*: w) :*: i) -> w :*: i :*: Const a)
            & randomizeNewVars
            >>= ExprIRef.writeRecursively
        (holeStored ^. ExprIRef.setIref) (writtenExpr ^. hAnn . _1)
        ExprIRef.toHRefs (holeStored ^. ExprIRef.setIref) writtenExpr
            & hflipped %~ hmap (\_ (w :*: i :*: Const a) -> (w :*: Const a) :*: i)
            & inferUVarsApplyBindings
            & runPureInfer () inferCtx
            & assertSuccessfulInfer
            & fst
            & hflipped %~ hmap (const toPayload)
            & pure
    where
        toPayload ((stored :*: Const a) :*: inferRes) =
            -- TODO: Evaluate hole results instead of Map.empty's?
            Input.Payload
            { Input._varRefsOfLambda = [] -- TODO
            , Input._userData = a
            , Input._inferRes = inferRes
            , Input._inferScope = V.emptyScope -- TODO: HACK
            , Input._stored = stored
            , Input._entityId = eId
            , Input._localsInScope = []
            }
            where
                eId = stored ^. ExprIRef.iref . _F & IRef.uuid & EntityId.EntityId

detachValIfNeeded ::
    (a # V.Term) ->
    UVar # T.Type ->
    Ann (a :*: InferResult UVar) # V.Term ->
    -- TODO: PureInfer?
    State InferState (Ann (a :*: InferResult UVar) # V.Term)
detachValIfNeeded defPl holeType x =
    do
        unifyRes <-
            do
                r <- unify holeType xType
                -- Verify occurs checks.
                -- TODO: share with applyBindings that happens for sugaring.
                s <- State.get
                _ <- inferUVarsApplyBindings x
                r <$ State.put s
            & liftPureInfer
        let mkFragmentExpr =
                FuncType xType holeType & T.TFun
                & UTermBody mempty & UTerm & newVar binding
                <&> \funcType ->
                let func = V.BLeaf V.LHole & Ann (defPl :*: inferResult # funcType)
                in  func `V.App` x & V.BApp & Ann (defPl :*: inferResult # holeType)
        case unifyRes of
            Right{} -> pure x
            Left{} ->
                liftPureInfer mkFragmentExpr
                <&> assertSuccessfulInfer
    where
        xType = x ^. hAnn . _2 . inferResult
        liftPureInfer ::
            PureInfer () a -> State InferState (Either (Pure # T.TypeError) a)
        liftPureInfer act =
            do
                st <- Lens.use id
                runPureInfer () st act
                    & Lens._Right %%~ \(r, newSt) -> r <$ (id .= newSt)

mkResultVals ::
    Monad m =>
    ConvertM.Context m -> V.Scope # UVar -> Val () ->
    ResultGen m (Deps, Ann (Write m :*: InferResult UVar) # V.Term)
mkResultVals sugarContext scope seed =
    -- TODO: This uses state from context but we're in StateT.
    -- This is a mess..
    loadInfer sugarContext scope seed & txn & lift
    >>=
    \case
    (_, Left{}) -> empty
    (newDeps, Right ((i, _), newInferState)) ->
        do
            id .= newInferState
            form <-
                Suggest.termTransformsWithModify (lift (lift genLamVar)) scope (Const () :*:) (^. _2) i
            pure (newDeps, form & hflipped %~ hmap (const (_1 .~ WriteNew)))
    where
        txn = lift . lift

mkResult ::
    (Monad m, Typeable m) =>
    Preconversion m a -> T m () ->
    Input.Payload m b # V.Term ->
    Ann ((Const a :*: Write m) :*: InferResult UVar) # V.Term ->
    ConvertM m (OnceT (T m) (HoleResult InternalName (OnceT (T m)) (T m)))
mkResult preConversion updateDeps holePl x =
    do
        sugarContext <- Lens.view id
        postProcess <- ConvertM.postProcessAssert
        do
            updateDeps
            writeResult (sugarContext ^. ConvertM.scInferContext)
                (holePl ^. Input.stored) x
            <&> preConversion
            & lift
            <&> Input.preprocess
                    (holePl ^. Input.inferScope)
                        -- TODO: this is kind of wrong
                        -- The scope for a proper term should be from after loading its infer deps
                        -- But that's only necessary for suggesting hole results?
                        -- And we are in a hole result here
                    (holePl ^. Input.localsInScope)
            <&> convertBinder
            <&> fmap convertPayloads
            >>= ConvertM.run sugarContext
            <&> SugarLens.hAnnotations @EvalPrep @(Annotation () InternalName) .~ AnnotationNone
            & _OnceT %~ mapStateT (fmap (\((fConverted, s), forkedChanges) -> ((fConverted, forkedChanges), s)) . Transaction.fork)
            & once & join
            <&>
            ( \(fConverted, forkedChanges) ->
                HoleResult
                { _holeResultConverted = fConverted
                , _holeResultPick =
                    do
                        Transaction.merge forkedChanges
                        postProcess
                }
            ) & ConvertM.convertOnce

toStateT :: Applicative m => State s a -> StateT s m a
toStateT = mapStateT $ \(Lens.Identity act) -> pure act

toScoredResults ::
    (Monad m, Typeable m) =>
    HoleResultTier -> a -> Preconversion m a -> ConvertM.Context m ->
    Input.Payload m dummy # V.Term ->
    StateT InferState (ListT (OnceT (T m))) (Deps, Ann ((Const a :*: Write m) :*: InferResult UVar) # V.Term) ->
    ListT (OnceT (T m)) (HoleResultScore, OnceT (T m) (HoleResult InternalName (OnceT (T m)) (T m)))
toScoredResults tier emptyPl preConversion sugarContext holePl act =
    act
    >>= _2 %%~
        toStateT .
        detachValIfNeeded
            (Const emptyPl :*: WriteNew)
            (holePl ^. Input.inferredTypeUVar)
    & (`runStateT` (sugarContext ^. ConvertM.scInferContext))
    >>=
    \((newDeps, x), inferCtx) ->
    do
        let newSugarContext =
                sugarContext
                & ConvertM.scInferContext .~ inferCtx
                & ConvertM.scFrozenDeps . Property.pVal .~ newDeps
            updateDeps = newDeps & sugarContext ^. ConvertM.scFrozenDeps . Property.pSet
        r <-
            mkResult preConversion updateDeps holePl x & ConvertM.run newSugarContext
            >>= once
            & lift
        pure
            ( inferUVarsApplyBindings x
                <&> hflipped %~ hmap
                    ( Proxy @(Recursively (InferOfConstraint HFunctor)) #*#
                        \w ->
                        withDict (recursively (p0 w)) $
                        withDict (inferOfConstraint (Proxy @HFunctor) w) $
                        (hflipped %~ hmap (const (^. _1))) . (^. _2)
                    )
                & runPureInfer () inferCtx
                & assertSuccessfulInfer
                & fst
                & resultScore tier
            , r
            )
    where
        p0 :: proxy h -> Proxy (InferOfConstraint HFunctor h)
        p0 _ = Proxy

mapListItemTail :: (l a -> k a) -> ListClass.ListItem l a -> ListClass.ListItem k a
mapListItemTail _ ListClass.Nil = ListClass.Nil
mapListItemTail f (ListClass.Cons h t) = ListClass.Cons h (f t)

mapListT ::
    Functor m =>
    (m (ListClass.ListItem (ListT n) a) -> n (ListClass.ListItem (ListT n) a)) ->
    ListT m a -> ListT n a
mapListT f (ListT l) = l <&> mapListItemTail (mapListT f) & f & ListT

mkResults ::
    (Monad m, Typeable m) =>
    HoleResultTier -> ResultProcessor m -> ConvertM.Context m ->
    Input.Payload m dummy # V.Term -> Val () ->
    ListT (OnceT (T m))
    ( HoleResultScore
    , OnceT (T m) (HoleResult InternalName (OnceT (T m)) (T m))
    )
mkResults tier (ResultProcessor emptyPl postProcess preConversion) sugarContext holePl base =
    mkResultVals sugarContext (holePl ^. Input.inferScope) base
    >>= _2 %%~ postProcess
    & toScoredResults tier emptyPl preConversion sugarContext holePl
    & mapListT (join . once)
