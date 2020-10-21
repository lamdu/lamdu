{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Lamdu.Sugar.Convert.Hole
    ( convert
      -- Used by Convert.Fragment:
    , Preconversion, ResultGen
    , ResultProcessor(..)
    , mkOptions, detachValIfNeeded, sugar, loadNewDeps
    , mkResult
    , mkOption, addWithoutDups
    , assertSuccessfulInfer
    ) where

import           Control.Applicative (Alternative(..))
import qualified Control.Lens as Lens
import           Control.Monad ((>=>), filterM)
import           Control.Monad.ListT (ListT)
import           Control.Monad.Once (OnceT, _OnceT)
import           Control.Monad.State (State, StateT(..), mapStateT, evalState, state)
import qualified Control.Monad.State as State
import           Control.Monad.Transaction (transaction)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Binary as Binary
import           Data.Bits (xor)
import qualified Data.ByteString.Extended as BS
import qualified Data.List.Class as ListClass
import qualified Data.Map as Map
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import           Data.Semigroup (Endo)
import qualified Data.Set as Set
import qualified Data.UUID as UUID
import           Hyper
import           Hyper.Infer
import           Hyper.Recurse (Recursive(..), wrap, unwrap)
import           Hyper.Type.AST.FuncType (FuncType(..))
import           Hyper.Type.AST.Nominal (NominalDecl, nScheme)
import           Hyper.Type.AST.Row (freExtends)
import           Hyper.Type.AST.Scheme (sTyp)
import           Hyper.Type.Functor (F(..), _F)
import           Hyper.Type.Prune (Prune(..))
import           Hyper.Unify (Unify(..), BindingDict(..), unify)
import           Hyper.Unify.Binding (UVar)
import           Hyper.Unify.Term (UTerm(..), UTermBody(..))
import qualified Lamdu.Annotations as Annotations
import qualified Lamdu.Cache as Cache
import           Lamdu.Calc.Definition (Deps(..), depsGlobalTypes, depsNominals)
import           Lamdu.Calc.Infer (InferState, runPureInfer, PureInfer)
import qualified Lamdu.Calc.Lens as ExprLens
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Term.Eq (couldEq)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.GenIds as GenIds
import           Lamdu.Expr.IRef (HRef(..), ValI, DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import           Lamdu.Sugar.Annotations (neverShowAnnotations, alwaysShowAnnotations)
import qualified Lamdu.Sugar.Config as Config
import           Lamdu.Sugar.Convert.Binder (convertBinder)
import qualified Lamdu.Sugar.Convert.Completions as Completions
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, convertPayloads)
import           Lamdu.Sugar.Convert.Hole.ResultScore (resultScore)
import qualified Lamdu.Sugar.Convert.Hole.Suggest as Suggest
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Revision.Deltum.Hyper (Write(..))
import           Revision.Deltum.IRef (IRef)
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction
import           System.Random (random)
import qualified System.Random.Extended as Random
import           Text.PrettyPrint.HughesPJClass (prettyShow)

import           Lamdu.Prelude

type T = Transaction

type Preconversion m a =
    Ann (Input.Payload m a) # V.Term ->
    Ann (Input.Payload m ()) # V.Term

type ResultGen m = StateT InferState (ListT (OnceT (T m)))

convert ::
    (Monad m, Monoid a) =>
    ConvertM.PositionInfo -> Input.Payload m a # V.Term ->
    ConvertM m (ExpressionU (Annotation EvalPrep InternalName) m a)
convert posInfo holePl =
    mkOptions posInfo holeResultProcessor holePl
    <&> Lens.mapped . Lens.mapped %~ snd
    <&> (`Hole` Nothing) <&> BodyHole
    >>= addActions (Const ()) holePl
    <&> annotation . pActions . mSetToHole .~ Nothing

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

mkOption ::
    Monad m =>
    ConvertM.Context m -> ResultProcessor m ->
    Input.Payload m a # V.Term -> Val () ->
    OnceT (T m) (HoleOption (Annotation EvalPrep InternalName) InternalName (OnceT (T m)) (T m))
mkOption sugarContext resultProcessor holePl x =
    pure HoleOption
    { _hoEntityId =
        x
        & ExprLens.valLeafs . V._LLiteral . V.primData .~ mempty
        & show
        & Random.randFunc
        & EntityId.EntityId
    , _hoSugaredBaseExpr = sugar sugarContext holePl x
    , _hoResults = mkResults resultProcessor sugarContext holePl x
    }

mkHoleSuggesteds ::
    Monad m =>
    ConvertM.Context m -> ResultProcessor m ->
    Input.Payload m a # V.Term ->
    OnceT (T m) [(Val (), HoleOption (Annotation EvalPrep InternalName) InternalName (OnceT (T m)) (T m))]
mkHoleSuggesteds sugarContext resultProcessor holePl =
    holePl ^. Input.inferredTypeUVar
    & Completions.suggestForType
    & runPureInfer (holePl ^. Input.inferScope) inferCtx

    -- TODO: Change ConvertM to be stateful rather than reader on the
    -- sugar context, to collect union-find updates.
    -- TODO: use a specific monad here that has no Either
    & assertSuccessfulInfer
    & fst
    <&> hflipped %~ hmap (const (const (Const ()))) -- TODO: "Avoid re-inferring known type here"
    & traverse
        (\x -> mkOption sugarContext resultProcessor holePl x <&> (,) x)
    where
        inferCtx = sugarContext ^. ConvertM.scInferContext

strip :: Recursively HFunctor h => Ann a # h -> Pure # h
strip = unwrap (const (^. hVal))

addWithoutDups ::
    [(Val (), HoleOption v i o a)] ->
    [(Val (), HoleOption v i o a)] ->
    [(Val (), HoleOption v i o a)]
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
            <&> (`V.BInjectP` V.BLeafP V.LHole)
            <&> V.BToNomP tid

mkOptions ::
    Monad m =>
    ConvertM.PositionInfo -> ResultProcessor m ->
    Input.Payload m a # V.Term ->
    ConvertM m (OnceT (T m) [(Val (), HoleOption (Annotation EvalPrep InternalName) InternalName (OnceT (T m)) (T m))])
mkOptions posInfo resultProcessor holePl =
    Lens.view id
    <&>
    \sugarContext ->
    do
        nominalOptions <- getNominals sugarContext & lift <&> mkNominalOptions
        globals <- getGlobals sugarContext & lift
        tags <- getTags sugarContext & lift
        suggesteds <- mkHoleSuggesteds sugarContext resultProcessor holePl
        concat
            [ holePl ^. Input.localsInScope >>= getLocalScopeGetVars sugarContext
            , globals <&> V.BLeafP . V.LVar . ExprIRef.globalId
            , tags <&> (`V.BInjectP` V.BLeafP V.LHole)
            , nominalOptions
            , [ V.BLamP "NewLambda" Pruned (V.BLeafP V.LHole)
              , V.BLeafP V.LAbsurd
              ]
            , [ V.BLamP "NewLambda" Pruned (V.BLeafP V.LHole) `V.BAppP` V.BLeafP V.LHole
              | posInfo == ConvertM.BinderPos
              ]
            ]
            <&> wrap (const (Ann (Const ()))) . (^. hPlain)
            & traverse (\x -> mkOption sugarContext resultProcessor holePl x <&> (,) x)
            <&> addWithoutDups suggesteds

-- TODO: Generalize into a separate module?
loadDeps :: Monad m => [V.Var] -> [T.NominalId] -> T m Deps
loadDeps vars noms =
    Deps
    <$> (traverse loadVar vars <&> Map.fromList)
    <*> (traverse loadNom noms <&> Map.fromList)
    where
        loadVar globalId =
            ExprIRef.defI globalId & Transaction.readIRef
            <&> (^. Def.defType) <&> (,) globalId
        loadNom nomId =
            Load.nominal nomId
            <&> fromMaybe (error "Opaque nominal used!")
            <&> (,) nomId

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
            Getting' Deps (Map r x) -> Folding' (Ann (Const ()) # V.Term) r -> [r]
        newDeps depsLens valLens =
            Set.fromList ((x & hflipped %~ hmap (const (const (Const ())))) ^.. valLens)
            `Set.difference` Map.keysSet (currentDeps ^. depsLens)
            & Set.toList
        newDepVars = newDeps depsGlobalTypes (ExprLens.valGlobals scopeVars)
        newNoms = newDeps depsNominals ExprLens.valNominals

-- Unstored and without eval results.
-- Used for hole's base exprs, to perform sugaring and get names from sugared exprs.
-- TODO: We shouldn't need to perform sugaring for base exprs, and this should be removed.
prepareUnstoredPayloads ::
    Ann (InferResult (Pure :*: UVar) :*: Const (EntityId, a)) # V.Term ->
    Ann (Input.Payload m a) # V.Term
prepareUnstoredPayloads v =
    v & hflipped %~ hmap (const mk) & Input.preparePayloads
    where
        mk (inferPl :*: Const (eId, x)) =
            Input.PreparePayloadInput
            { Input.ppEntityId = eId
            , Input.ppMakePl =
                \varRefs ->
                Input.Payload
                { Input._varRefsOfLambda = varRefs
                , Input._userData = x
                , Input._localsInScope = []
                , Input._inferRes = inferPl
                , Input._inferScope = V.emptyScope
                , Input._entityId = eId
                , Input._stored =
                    HRef
                    (_F # IRef.unsafeFromUUID fakeStored)
                    (error "stored output of base expr used!")
                }
            }
            where
                -- TODO: Which code reads this?
                EntityId.EntityId fakeStored = eId

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

sugar ::
    (Monad m, Monoid a) =>
    ConvertM.Context m -> Input.Payload m dummy # V.Term -> Val a ->
    OnceT (T m) (Expr Binder (Annotation EvalPrep InternalName) InternalName (OnceT (T m)) (T m) a)
sugar sugarContext holePl v =
    do
        (val, inferCtx) <-
            loadInfer sugarContext scope v
            <&> snd
            <&> assertSuccessfulInfer
            <&>
            ( \((term, topLevelScope), inferCtx) ->
                ( inferUVarsApplyBindings term
                    & runPureInfer () inferCtx
                    & assertSuccessfulInfer
                    & fst
                    & hflipped %~ hmap (const mkPayload)
                    & EntityId.randomizeExprAndParams
                        (Random.genFromHashable (holePl ^. Input.entityId))
                    & prepareUnstoredPayloads
                    & Input.initScopes topLevelScope (holePl ^. Input.localsInScope)
                , inferCtx
                )
            ) & transaction & lift
        convertBinder val
            <&> hflipped %~ hmap (const (Lens._Wrapped %~ (,) neverShowAnnotations))
            >>= convertPayloads
            & ConvertM.run
                (sugarContext
                    & ConvertM.scInferContext .~ inferCtx
                    & ConvertM.scAnnotationsMode .~ Annotations.None
                )
    where
        scope = holePl ^. Input.inferScope
        mkPayload (Const x :*: inferPl) =
            _HFunc # \(Const entityId) -> inferPl :*: Const (entityId, x)

getLocalScopeGetVars :: ConvertM.Context m -> V.Var -> [HPlain V.Term]
getLocalScopeGetVars sugarContext par
    | sugarContext ^. ConvertM.scScopeInfo . ConvertM.siNullParams . Lens.contains par = []
    | otherwise = (fieldTags <&> V.BGetFieldP var) <> [var]
    where
        var = V.LVar par & V.BLeafP
        fieldTags =
            ( sugarContext ^@..
                ConvertM.scScopeInfo . ConvertM.siTagParamInfos .>
                ( Lens.itraversed <.
                    ConvertM._TagFieldParam . Lens.to ConvertM.tpiFromParameters ) <.
                    Lens.filtered (== par)
            ) <&> fst

-- | Runs inside a forked transaction
writeResult ::
    Monad m =>
    Preconversion m a -> InferState -> HRef m # V.Term ->
    Ann ((Const a :*: Write m) :*: InferResult UVar) # V.Term ->
    T m (Ann (Input.Payload m ()) # V.Term)
writeResult preConversion inferCtx holeStored inferredVal =
    do
        writtenExpr <-
            inferUVarsApplyBindings inferredVal
            & runPureInfer () inferCtx
            & assertSuccessfulInfer
            & fst
            & hflipped %~ hmap (\_ ((Const a :*: w) :*: i) -> w :*: i :*: Const a)
            & writeExprMStored (holeStored ^. ExprIRef.iref)
            <&> ExprIRef.toHRefs (holeStored ^. ExprIRef.setIref)
            <&> hflipped %~ hmap (const toPayload)
            <&> Input.preparePayloads
        (holeStored ^. ExprIRef.setIref) (writtenExpr ^. hAnn . Input.stored . ExprIRef.iref)
        preConversion writtenExpr & pure
    where
        toPayload (stored :*: inferRes :*: Const a) =
            -- TODO: Evaluate hole results instead of Map.empty's?
            Input.PreparePayloadInput
            { Input.ppEntityId = eId
            , Input.ppMakePl =
                \varRefs ->
                Input.Payload
                { Input._varRefsOfLambda = varRefs
                , Input._userData = a
                , Input._inferRes = inferRes
                , Input._inferScope = V.emptyScope -- TODO: HACK
                , Input._stored = stored
                , Input._entityId = eId
                , Input._localsInScope = []
                }
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
                Suggest.termTransformsWithModify scope (Const () :*:) (^. _2) i
                & mapStateT ListClass.fromList
            pure (newDeps, form & hflipped %~ hmap (const (_1 .~ WriteNew)))
    where
        txn = lift . lift

mkResult ::
    Monad m =>
    Preconversion m a -> T m () ->
    Input.Payload m b # V.Term ->
    Ann ((Const a :*: Write m) :*: InferResult UVar) # V.Term ->
    ConvertM m (OnceT (T m) (HoleResult (Annotation EvalPrep InternalName) InternalName (OnceT (T m)) (T m)))
mkResult preConversion updateDeps holePl x =
    do
        sugarContext <- Lens.view id
        postProcess <- ConvertM.postProcessAssert
        let showAnn
                | sugarContext ^. ConvertM.scConfig . Config.showAllAnnotations = alwaysShowAnnotations
                | otherwise = neverShowAnnotations
        do
            updateDeps
            writeResult preConversion (sugarContext ^. ConvertM.scInferContext)
                (holePl ^. Input.stored) x
            & lift
            <&> Input.initScopes
                    (holePl ^. Input.inferScope)
                        -- TODO: this is kind of wrong
                        -- The scope for a proper term should be from after loading its infer deps
                        -- But that's only necessary for suggesting hole results?
                        -- And we are in a hole result here
                    (holePl ^. Input.localsInScope)
            <&> (convertBinder >=> convertPayloads . (hflipped %~ hmap (const (Lens._Wrapped %~ (,) showAnn))))
            >>= ConvertM.run (sugarContext & ConvertM.scAnnotationsMode .~ Annotations.None)
            & _OnceT %~ mapStateT (fmap (\((fConverted, s), forkedChanges) -> ((fConverted, forkedChanges), s)) . Transaction.fork)
            <&>
            ( \(fConverted, forkedChanges) ->
                HoleResult
                { _holeResultConverted = fConverted
                , _holeResultPick =
                    do
                        Transaction.merge forkedChanges
                        postProcess
                }
            ) & pure

toStateT :: Applicative m => State s a -> StateT s m a
toStateT = mapStateT $ \(Lens.Identity act) -> pure act

toScoredResults ::
    (Monad f, Monad m) =>
    a -> Preconversion m a -> ConvertM.Context m ->
    Input.Payload m dummy # V.Term ->
    StateT InferState f (Deps, Ann ((Const a :*: Write m) :*: InferResult UVar) # V.Term) ->
    f (HoleResultScore, OnceT (T m) (HoleResult (Annotation EvalPrep InternalName) InternalName (OnceT (T m)) (T m)))
toScoredResults emptyPl preConversion sugarContext holePl act =
    act
    >>= _2 %%~
        toStateT .
        detachValIfNeeded
            (Const emptyPl :*: WriteNew)
            (holePl ^. Input.inferredTypeUVar)
    & (`runStateT` (sugarContext ^. ConvertM.scInferContext))
    <&> \((newDeps, x), inferCtx) ->
    let newSugarContext =
            sugarContext
            & ConvertM.scInferContext .~ inferCtx
            & ConvertM.scFrozenDeps . Property.pVal .~ newDeps
        updateDeps = newDeps & sugarContext ^. ConvertM.scFrozenDeps . Property.pSet
    in  ( inferUVarsApplyBindings x
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
            & resultScore
        , mkResult preConversion updateDeps holePl x & ConvertM.run newSugarContext & join
        )
    where
        p0 :: proxy h -> Proxy (InferOfConstraint HFunctor h)
        p0 _ = Proxy

mkResults ::
    Monad m =>
    ResultProcessor m -> ConvertM.Context m ->
    Input.Payload m dummy # V.Term -> Val () ->
    ListT (OnceT (T m))
    ( HoleResultScore
    , OnceT (T m) (HoleResult (Annotation EvalPrep InternalName) InternalName (OnceT (T m)) (T m))
    )
mkResults (ResultProcessor emptyPl postProcess preConversion) sugarContext holePl base =
    mkResultVals sugarContext (holePl ^. Input.inferScope) base
    >>= _2 %%~ postProcess
    & toScoredResults emptyPl preConversion sugarContext holePl

xorBS :: ByteString -> ByteString -> ByteString
xorBS x y = BS.pack $ BS.zipWith xor x y

randomizeNonStoredParamIds ::
    Random.StdGen ->
    Ann (ExprIRef.Write m :*: a) # V.Term ->
    Ann (ExprIRef.Write m :*: a) # V.Term
randomizeNonStoredParamIds gen =
    GenIds.randomizeParamIdsG id nameGen Map.empty
    where
        nameGen = GenIds.onNgMakeName f $ GenIds.randomNameGen gen
        f n _        prevEntityId (ExprIRef.ExistingRef{} :*: _) = (prevEntityId, n)
        f _ prevFunc prevEntityId pl@(ExprIRef.WriteNew :*: _) = prevFunc prevEntityId pl

randomizeNonStoredRefs ::
    ByteString ->
    Random.StdGen ->
    Ann (ExprIRef.Write m :*: a) # V.Term ->
    Ann (F (IRef m) :*: a) # V.Term
randomizeNonStoredRefs uniqueIdent gen v =
    evalState (hflipped (htraverse (const (_1 f))) v) gen
    where
        f :: Write m # h -> State Random.StdGen (F (IRef m) # h)
        f ExprIRef.WriteNew =
            state random
            <&> UUID.toByteString <&> BS.strictify
            <&> xorBS uniqueIdent
            <&> BS.lazify <&> UUID.fromByteString
            <&> fromMaybe (error "cant parse UUID")
            <&> IRef.unsafeFromUUID <&> F
        f (ExprIRef.ExistingRef x) = pure x

writeExprMStored ::
    Monad m =>
    ValI m ->
    Ann (ExprIRef.Write m :*: a) # V.Term ->
    T m (Ann (F (IRef m) :*: a) # V.Term)
writeExprMStored exprIRef exprMStorePoint =
    result <$ writeAll result
    where
        result =
            randomizeNonStoredParamIds genParamIds exprMStorePoint
            & randomizeNonStoredRefs uniqueIdent genRefs
        writeAll ::
            forall m t a.
            ExprIRef.HStore m t =>
            Ann (F (IRef m) :*: a) # t -> T m ()
        writeAll (Ann (dst :*: _) body) =
            withDict (recurse (Proxy @(ExprIRef.HStore m t))) $
            do
                ExprIRef.writeValI dst (hmap (const (^. hAnn . _1)) body)
                htraverse_ (Proxy @(ExprIRef.HStore m) #> writeAll) body
        uniqueIdent =
            Binary.encode
            ( exprMStorePoint & hflipped %~ hmap (const (^. _1))
            , exprIRef
            )
            & SHA256.hashlazy
        (genParamIds, genRefs) = Random.genFromHashable uniqueIdent & Random.split
