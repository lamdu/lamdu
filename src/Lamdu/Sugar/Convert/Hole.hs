{-# LANGUAGE ExistentialQuantification, TupleSections #-}
module Lamdu.Sugar.Convert.Hole
    ( convert
      -- Used by Convert.Fragment:
    , StorePoint, ResultVal, Preconversion, ResultGen
    , ResultProcessor(..)
    , mkOptions, detachValIfNeeded, sugar, loadNewDeps
    , mkResult
    , mkOption, addWithoutDups
    , BaseExpr(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad ((>=>), filterM)
import           Control.Monad.ListT (ListT)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.State (StateT(..), mapStateT, evalState, state)
import qualified Control.Monad.Trans.State as State
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Binary as Binary
import           Data.Bits (xor)
import qualified Data.ByteString.Extended as BS
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.Functor.Identity (Identity(..))
import qualified Data.List.Class as ListClass
import qualified Data.Map as Map
import           Data.Property (MkProperty')
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Data.Text.Encoding (encodeUtf8)
import           Data.Tree.Diverse (Node, Ann(..), ann, annotations)
import qualified Data.UUID as UUID
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import qualified Lamdu.Calc.Lens as ExprLens
import qualified Lamdu.Calc.Pure as P
import           Lamdu.Calc.Term (Val)
import qualified Lamdu.Calc.Term as V
import           Lamdu.Calc.Term.Eq (couldEq)
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.Nominal as N
import           Lamdu.Calc.Type.Scheme (mono)
import qualified Lamdu.Calc.Type.Scheme as CalcScheme
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Expr.GenIds as GenIds
import           Lamdu.Expr.IRef (ValP, ValI, DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Trans as InferT
import           Lamdu.Infer.Unify (unify)
import           Lamdu.Infer.Update (Update, update)
import qualified Lamdu.Infer.Update as Update
import           Lamdu.Sugar.Annotations (neverShowAnnotations)
import           Lamdu.Sugar.Convert.Binder (convertBinder)
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, convertPayload)
import           Lamdu.Sugar.Convert.Hole.ResultScore (resultScore)
import qualified Lamdu.Sugar.Convert.Hole.Suggest as Suggest
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction
import           System.Random (random)
import qualified System.Random.Extended as Random

import           Lamdu.Prelude

type T = Transaction

type StorePoint m a = (Maybe (ValI m), a)

type ResultVal m a = Val (Infer.Payload, StorePoint m a)

type Preconversion m a = Val (Input.Payload m a) -> Val (Input.Payload m ())

type ResultGen m = StateT Infer.Context (ListT (T m))

convert :: (Monad m, Monoid a) => Input.Payload m a -> ConvertM m (ExpressionU m a)
convert holePl =
    Hole
    <$> mkOptions holeResultProcessor holePl
    <*> mkLiteralOptions holePl
    <*> pure Nothing
    <&> BodyHole
    >>= addActions [] holePl
    <&> ann . pActions . mSetToHole .~ Nothing

data BaseExpr = SuggestedExpr (Val Infer.Payload) | SeedExpr (Val ())

getBaseExprVal :: BaseExpr -> Val ()
getBaseExprVal (SuggestedExpr v) = v & annotations .~ ()
getBaseExprVal (SeedExpr v) = v

data ResultProcessor m = forall a. ResultProcessor
    { rpEmptyPl :: a
    , rpPostProcess :: ResultVal m () -> ResultGen m (ResultVal m a)
    , rpPreConversion :: Preconversion m a
    }

holeResultProcessor :: Monad m => ResultProcessor m
holeResultProcessor =
    ResultProcessor
    { rpEmptyPl = ()
    , rpPostProcess = pure
    , rpPreConversion = id
    }

mkOption ::
    Monad m =>
    ConvertM.Context m -> ResultProcessor m -> Input.Payload m a -> BaseExpr ->
    HoleOption InternalName (T m) (T m)
mkOption sugarContext resultProcessor holePl x =
    HoleOption
    { _hoVal = baseExpr
    , _hoSugaredBaseExpr = sugar sugarContext holePl baseExpr
    , _hoResults = mkResults resultProcessor sugarContext holePl x
    }
    where
        baseExpr = getBaseExprVal x

mkHoleSuggesteds ::
    Monad m =>
    ConvertM.Context m -> ResultProcessor m -> Input.Payload m a ->
    [HoleOption InternalName (T m) (T m)]
mkHoleSuggesteds sugarContext resultProcessor holePl =
    holePl ^. Input.inferred
    & Suggest.value
    <&> SuggestedExpr
    <&> mkOption sugarContext resultProcessor holePl

addWithoutDups ::
    [HoleOption i o a] -> [HoleOption i o a] -> [HoleOption i o a]
addWithoutDups new old
    | null nonHoleNew = old
    | otherwise = nonHoleNew ++ filter (not . equivalentToNew) old
    where
        equivalentToNew x =
            any (couldEq (x ^. hoVal)) (nonHoleNew ^.. Lens.traverse . hoVal)
        nonHoleNew = filter (Lens.nullOf (hoVal . ExprLens.valHole)) new

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
    sugarContext ^. ConvertM.scCodeAnchors
    & anchor & Property.getP <&> Set.toList

getNominals :: Monad m => ConvertM.Context m -> T m [(T.NominalId, N.Nominal)]
getNominals sugarContext =
    getListing Anchors.tids sugarContext
    >>= traverse (\nomId -> (,) nomId <$> Load.nominal nomId)

getGlobals :: Monad m => ConvertM.Context m -> T m [DefI m]
getGlobals sugarContext =
    getListing Anchors.globals sugarContext >>= filterM isLiveGlobal

getTags :: Monad m => ConvertM.Context m -> T m [T.Tag]
getTags = getListing Anchors.tags

locals :: ConvertM.Context m -> Input.Payload f a -> [V.Var]
locals sugarContext holePl =
    holePl ^. Input.inferredScope
    & Infer.scopeToTypeMap
    & Map.keys
    & filter (not . isRecursiveRef)
    where
        recursiveVar =
            sugarContext
            ^? ConvertM.scScopeInfo . ConvertM.siRecursiveRef . Lens._Just .
            ConvertM.rrDefI . Lens.to ExprIRef.globalId
        isRecursiveRef varId = recursiveVar == Just varId

mkNominalOptions :: [(T.NominalId, N.Nominal)] -> [Val ()]
mkNominalOptions nominals =
    do
        (tid, nominal) <- nominals
        mkDirectNoms tid ++ mkToNomInjections tid nominal
    where
        mkDirectNoms tid =
            do
                f <- [V.BFromNom, V.BToNom]
                [ V.Nom tid P.hole & f & Ann () ]
        mkToNomInjections tid nominal =
            do
                (tag, _typ) <-
                    nominal ^..
                    N.nomType . N._NominalType . CalcScheme.schemeType . T._TVariant .
                    ExprLens.compositeFields
                let inject = V.Inject tag P.hole & V.BInject & Ann ()
                [ inject & V.Nom tid & V.BToNom & Ann () ]

mkOptions ::
    Monad m =>
    ResultProcessor m -> Input.Payload m a ->
    ConvertM m (T m [HoleOption InternalName (T m) (T m)])
mkOptions resultProcessor holePl =
    Lens.view id
    <&>
    \sugarContext ->
    do
        nominalOptions <- getNominals sugarContext <&> mkNominalOptions
        globals <- getGlobals sugarContext
        tags <- getTags sugarContext
        concat
            [ locals sugarContext holePl
                & concatMap (getLocalScopeGetVars sugarContext)
            , globals <&> P.var . ExprIRef.globalId
            , tags <&> (`P.inject` P.hole)
            , nominalOptions
            , [ P.abs "NewLambda" P.hole
              , P.recEmpty
              , P.absurd
              , P.abs "NewLambda" P.hole P.$$ P.hole
              ]
            ]
            <&> SeedExpr
            <&> mkOption sugarContext resultProcessor holePl
            & addWithoutDups (mkHoleSuggesteds sugarContext resultProcessor holePl)
            & pure

-- TODO: Generalize into a separate module?
loadDeps :: Monad m => [V.Var] -> [T.NominalId] -> T m Infer.Dependencies
loadDeps vars noms =
    Infer.Deps
    <$> (traverse loadVar vars <&> Map.fromList)
    <*> (traverse loadNom noms <&> Map.fromList)
    where
        loadVar globalId =
            ExprIRef.defI globalId & Transaction.readIRef
            <&> (^. Def.defType) <&> (,) globalId
        loadNom nomId = Load.nominal nomId <&> (,) nomId

-- TODO: Generalize into a separate module?
loadNewDeps ::
    Monad m => Infer.Dependencies -> Infer.Scope -> Val a -> T m Infer.Dependencies
loadNewDeps currentDeps scope x =
    loadDeps newDepVars newNoms
    <&> mappend currentDeps
    where
        scopeVars = Infer.scopeToTypeMap scope & Map.keysSet
        newDeps depsLens valLens =
            Set.fromList (x ^.. valLens)
            `Set.difference` Map.keysSet (currentDeps ^. depsLens)
            & Set.toList
        newDepVars = newDeps Infer.depsGlobalTypes (ExprLens.valGlobals scopeVars)
        newNoms = newDeps Infer.depsNominals ExprLens.valNominals

-- Unstored and without eval results (e.g: hole result)
prepareUnstoredPayloads ::
    Val (Infer.Payload, EntityId, a) ->
    Val (Input.Payload m a)
prepareUnstoredPayloads v =
    v & annotations %~ mk & Input.preparePayloads
    where
        mk (inferPl, eId, x) =
            ( eId
            , \varRefs ->
              Input.Payload
              { Input._varRefsOfLambda = varRefs
              , Input._userData = x
              , Input._inferred = inferPl
              , Input._entityId = eId
              , Input._stored = error "TODO: Nothing stored?!"
              , Input._evalResults =
                  CurAndPrev Input.emptyEvalResults Input.emptyEvalResults
              }
            )

sugar ::
    (Monad m, Monoid a) =>
    ConvertM.Context m -> Input.Payload m dummy -> Val a ->
    T m (Node (Ann (Payload InternalName (T m) (T m) a)) (Binder InternalName (T m) (T m)))
sugar sugarContext holePl v =
    v
    & annotations %~ mkPayload
    & (EntityId.randomizeExprAndParams . Random.genFromHashable)
        (holePl ^. Input.entityId)
    & prepareUnstoredPayloads
    & convertBinder
    <&> annotations %~ (,) neverShowAnnotations
    >>= annotations (convertPayload Input.None)
    & ConvertM.run sugarContext
    where
        mkPayload x entityId = (fakeInferPayload, entityId, x)
        -- A fake Infer payload we use to sugar the base expressions.
        -- Currently it is a function type because
        -- otherwise sugaring of lambdas crashes.
        fakeInferPayload =
            Infer.Payload
            { Infer._plType = T.TFun (T.TVar "fakeInput") (T.TVar "fakeOutput")
            , Infer._plScope = holePl ^. Input.inferred . Infer.plScope
            }

mkLiteralOptions ::
    Monad m =>
    Input.Payload m a ->
    ConvertM m (Literal Identity -> T m (HoleResultScore, T m (HoleResult InternalName (T m) (T m))))
mkLiteralOptions holePl =
    Lens.view id
    <&>
    \sugarContext ->
    let mk updateDeps x =
            pure
            ( HoleResultScore 0 []
            , fixedVal & annotations %~ convPl
                & mkResult id sugarContext updateDeps (holePl ^. Input.stored)
            )
            where
                typ = x ^. ann
                fixedVal
                    | inferredType == typ || Lens.has T._TVar inferredType = x
                    | otherwise =
                        V.Apply (Ann (T.TFun typ inferredType) (V.BLeaf V.LHole)) x
                        & V.BApp
                        & Ann inferredType
        addTextDep = Property.pureModify (sugarContext ^. ConvertM.scFrozenDeps) (<> textDep)
    in
    \case
    LiteralNum (Identity x) -> PrimVal.Float x & literalExpr & mk (pure ())
    LiteralBytes (Identity x) -> PrimVal.Bytes x & literalExpr & mk (pure ())
    LiteralText (Identity x) ->
        encodeUtf8 x
        & PrimVal.Bytes
        & literalExpr
        & V.Nom Builtins.textTid
        & V.BToNom
        & Ann (T.TInst Builtins.textTid mempty)
        & mk addTextDep
    where
        literalExpr v =
            V.LLiteral prim & V.BLeaf & Ann (T.TInst (prim ^. V.primType) mempty)
            where
                prim = PrimVal.fromKnown v
        emptyPl = (Nothing, ())
        convPl t = (Infer.Payload t Infer.emptyScope, emptyPl)
        inferredType = holePl ^. Input.inferred . Infer.plType
        textDep =
            mempty
            { Infer._depsNominals =
                Map.singleton Builtins.textTid
                N.Nominal
                { N._nomType = T.TInst Builtins.bytesTid mempty & mono & N.NominalType
                , N._nomParams = mempty
                }
            }

getLocalScopeGetVars :: ConvertM.Context m -> V.Var -> [Val ()]
getLocalScopeGetVars sugarContext par
    | sugarContext ^. ConvertM.scScopeInfo . ConvertM.siNullParams . Lens.contains par = []
    | otherwise = map mkFieldParam fieldTags ++ [var]
    where
        var = V.LVar par & V.BLeaf & Ann ()
        fieldTags =
            ( sugarContext ^@..
                ConvertM.scScopeInfo . ConvertM.siTagParamInfos .>
                ( Lens.itraversed <.
                    ConvertM._TagFieldParam . Lens.to ConvertM.tpiFromParameters ) <.
                    Lens.filtered (== par)
            ) <&> fst
        mkFieldParam tag = V.GetField var tag & V.BGetField & Ann ()

-- | Runs inside a forked transaction
writeResult ::
    Monad m =>
    Preconversion m a -> ValP m -> ResultVal m a ->
    T m (Val (Input.Payload m ()))
writeResult preConversion holeStored inferredVal =
    do
        writtenExpr <-
            inferredVal
            & annotations %~ intoStorePoint
            & writeExprMStored (Property.value holeStored)
            <&> ExprIRef.addProperties (holeStored ^. Property.pSet)
            <&> annotations %~ toPayload
            <&> Input.preparePayloads
            <&> annotations %~ snd
        (holeStored ^. Property.pSet) (writtenExpr ^. ann . _1 . Property.pVal)
        writtenExpr & annotations %~ snd & preConversion & pure
    where
        intoStorePoint (inferred, (mStorePoint, a)) =
            (mStorePoint, (inferred, Lens.has Lens._Just mStorePoint, a))
        toPayload (stored, (inferred, wasStored, a)) =
            -- TODO: Evaluate hole results instead of Map.empty's?
            ( eId
            , \varRefs ->
              ( wasStored
              , ( stored
                , Input.Payload
                  { Input._varRefsOfLambda = varRefs
                  , Input._userData = a
                  , Input._inferred = inferred
                  , Input._evalResults = CurAndPrev noEval noEval
                  , Input._stored = stored
                  , Input._entityId = eId
                  }
                )
              )
            )
            where
                eId = Property.value stored & EntityId.ofValI
        noEval = Input.EvalResultsForExpr Map.empty Map.empty

exceptTtoListT :: Monad m => ExceptT err m a -> ListT m a
exceptTtoListT = ListClass.joinL . fmap (ListClass.fromList . (^.. Lens._Right)) . runExceptT

detachValIfNeeded ::
    Monad m =>
    a -> T.Type -> Val (Infer.Payload, a) ->
    StateT Infer.Context m (Val (Infer.Payload, a))
detachValIfNeeded emptyPl holeType x =
    do
        unifyResult <-
            unify holeType (x ^. ann . _1 . Infer.plType) & liftInfer
        updated <- Update.inferredVal x & liftUpdate
        case unifyResult of
            Right{} -> pure updated
            Left{} -> detachVal emptyPl holeType updated & liftUpdate
    where
        liftUpdate = State.gets . Update.run
        liftInfer = stateEitherSequence . Infer.run

detachVal ::
    a -> T.Type -> Val (Infer.Payload, a) ->
    Update (Val (Infer.Payload, a))
detachVal emptyPl resultType x =
    update resultType <&> mk
    where
        mk updatedType =
            V.Apply func x & V.BApp
            & Ann (plSameScope updatedType, emptyPl)
        plSameScope typ = inferPl & Infer.plType .~ typ
        inferPl = x ^. ann . _1
        func = V.BLeaf V.LHole & Ann (plSameScope funcType, emptyPl)
        funcType = T.TFun (inferPl ^. Infer.plType) resultType

stateEitherSequence :: Monad m => StateT s (Either l) r -> StateT s m (Either l r)
stateEitherSequence (StateT f) =
    StateT $ \s0 ->
    pure $
    case f s0 of
    Right (r, s1) -> (Right r, s1)
    Left l -> (Left l, s0)

mkResultVals ::
    Monad m =>
    ConvertM.Context m -> Infer.Scope -> BaseExpr ->
    ResultGen m (Infer.Dependencies, ResultVal m ())
mkResultVals sugarContext scope base =
    case base of
    SeedExpr seed ->
        do
            (seedDeps, inferResult) <-
                do
                    seedDeps <- loadTheNewDeps seed
                    inferResult <-
                        Infer.infer seedDeps scope seed & InferT.liftInfer
                        <&> annotations . _2 %~ \() -> emptyPl
                    pure (seedDeps, inferResult)
                & mapStateT exceptTtoListT
            form <- Suggest.applyForms (transaction . Load.nominal) emptyPl inferResult
            newDeps <- loadNewDeps seedDeps scope form & transaction
            pure (newDeps, form)
    SuggestedExpr sugg ->
        (,)
        <$> mapStateT exceptTtoListT (loadTheNewDeps sugg)
        ?? (sugg & annotations %~ (, (Nothing, ())))
    where
        transaction = lift . lift
        emptyPl = (Nothing, ())
        loadTheNewDeps expr =
            loadNewDeps (sugarContext ^. ConvertM.scFrozenDeps . Property.pVal)
            scope expr & InferT.liftInner

mkResult ::
    Monad m =>
    Preconversion m a -> ConvertM.Context m -> T m () -> ValP m ->
    ResultVal m a ->
    T m (HoleResult InternalName (T m) (T m))
mkResult preConversion sugarContext updateDeps stored x =
    do
        updateDeps
        writeResult preConversion stored x
        <&> (convertBinder >=> annotations (convertPayload Input.None) . (annotations %~ (,) neverShowAnnotations))
        >>= ConvertM.run sugarContext
        & Transaction.fork
        <&> \(fConverted, forkedChanges) ->
        HoleResult
        { _holeResultConverted = fConverted
        , _holeResultPick =
            do
                Transaction.merge forkedChanges
                -- TODO: Remove this 'run', mkResult to be wholly in ConvertM
                ConvertM.run sugarContext ConvertM.postProcessAssert & join
        }

toScoredResults ::
    (Monad f, Monad m) =>
    a -> Preconversion m a -> ConvertM.Context m -> Input.Payload m dummy ->
    StateT Infer.Context f
    (Infer.Dependencies, ResultVal m a) ->
    f ( HoleResultScore
      , T m (HoleResult InternalName (T m) (T m))
      )
toScoredResults emptyPl preConversion sugarContext holePl act =
    act
    >>= _2 %%~ detachValIfNeeded (Nothing, emptyPl) typ
    & (`runStateT` (sugarContext ^. ConvertM.scInferContext))
    <&> \((newDeps, x), inferContext) ->
    let newSugarContext =
            sugarContext
            & ConvertM.scInferContext .~ inferContext
            & ConvertM.scFrozenDeps . Property.pVal .~ newDeps
        updateDeps = newDeps & sugarContext ^. ConvertM.scFrozenDeps . Property.pSet
    in  ( resultScore (x & annotations %~ fst)
        , mkResult preConversion newSugarContext updateDeps stored x
        )
    where
        stored = holePl ^. Input.stored
        typ = holePl ^. Input.inferred . Infer.plType

mkResults ::
    Monad m =>
    ResultProcessor m -> ConvertM.Context m -> Input.Payload m dummy -> BaseExpr ->
    ListT (T m)
    ( HoleResultScore
    , T m (HoleResult InternalName (T m) (T m))
    )
mkResults (ResultProcessor emptyPl postProcess preConversion) sugarContext holePl base =
    mkResultVals sugarContext (holePl ^. Input.inferred . Infer.plScope) base
    >>= _2 %%~ postProcess
    & toScoredResults emptyPl preConversion sugarContext holePl

xorBS :: ByteString -> ByteString -> ByteString
xorBS x y = BS.pack $ BS.zipWith xor x y

randomizeNonStoredParamIds ::
    Random.StdGen -> Val (StorePoint m a) -> Val (StorePoint m a)
randomizeNonStoredParamIds gen =
    GenIds.randomizeParamIdsG id nameGen Map.empty $ \_ _ pl -> pl
    where
        nameGen = GenIds.onNgMakeName f $ GenIds.randomNameGen gen
        f n _        prevEntityId (Just _, _) = (prevEntityId, n)
        f _ prevFunc prevEntityId pl@(Nothing, _) = prevFunc prevEntityId pl

randomizeNonStoredRefs ::
    ByteString -> Random.StdGen -> Val (StorePoint m a) -> Val (StorePoint m a)
randomizeNonStoredRefs uniqueIdent gen v =
    evalState ((annotations . _1) f v) gen
    where
        f Nothing =
            state random
            <&> UUID.toByteString <&> BS.strictify
            <&> xorBS uniqueIdent
            <&> BS.lazify <&> UUID.fromByteString
            <&> fromMaybe (error "cant parse UUID")
            <&> IRef.unsafeFromUUID <&> Just
        f (Just x) = Just x & pure

writeExprMStored ::
    Monad m => ValI m -> Val (StorePoint m a) -> T m (Val (ValI m, a))
writeExprMStored exprIRef exprMStorePoint =
    exprMStorePoint
    & randomizeNonStoredParamIds genParamIds
    & randomizeNonStoredRefs uniqueIdent genRefs
    & ExprIRef.writeValWithStoredSubexpressions
    where
        uniqueIdent =
            Binary.encode
            ( exprMStorePoint & annotations %~ fst
            , exprIRef
            )
            & SHA256.hashlazy
        (genParamIds, genRefs) = Random.genFromHashable uniqueIdent & Random.split

