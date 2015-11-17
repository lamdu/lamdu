{-# LANGUAGE NoImplicitPrelude, ConstraintKinds, OverloadedStrings, RankNTypes #-}
module Lamdu.Sugar.Convert.Hole
    ( convert, convertCommon
    , mkHoleOption, mkHoleOptionFromInjected, addSuggestedOptions
    , BaseExpr(..)
    ) where

import           Prelude.Compat

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (join, void, liftM)
import           Control.Monad.ListT (ListT)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Either (EitherT(..))
import           Control.Monad.Trans.State (StateT(..), mapStateT, evalStateT)
import qualified Control.Monad.Trans.State as State
import           Control.MonadA (MonadA)
import           Data.Binary.Utils (encodeS)
import           Data.CurAndPrev (CurAndPrev(..))
import qualified Data.List.Class as ListClass
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import           Data.Store.Guid (Guid)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Expr.GenIds as GenIds
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.IRef.Infer as IRefInfer
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Pure as P
import           Lamdu.Expr.Type (Type(..))
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import           Lamdu.Infer.Unify (unify)
import           Lamdu.Infer.Update (Update, update)
import qualified Lamdu.Infer.Update as Update
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.OrderTags (orderType)
import           Lamdu.Sugar.Types
import           Lamdu.Sugar.Convert.Hole.ResultScore (resultScore)
import qualified Lamdu.Sugar.Convert.Hole.Suggest as Suggest
import qualified System.Random as Random
import           System.Random.Utils (genFromHashable)
import           Text.PrettyPrint.HughesPJClass (pPrint, prettyShow)

type T = Transaction

type ExprStorePoint m a = Val (Maybe (ExprIRef.ValI m), a)

convert ::
    (MonadA m, Monoid a) =>
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convert exprPl =
    convertCommon Nothing exprPl
    <&> rPayload . plActions . Lens._Just . setToHole .~ AlreadyAHole

convertCommon ::
    (MonadA m, Monoid a) =>
    Maybe (Val (Input.Payload m a)) -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convertCommon mInjectedArg exprPl =
    mkHole mInjectedArg exprPl
    <&> BodyHole
    >>= addActions exprPl
    <&> rPayload . plActions . Lens._Just . wrap .~ WrapNotAllowed

mkHoleOptionFromInjected ::
    MonadA m =>
    ConvertM.Context m ->
    Input.Payload m a -> ExprIRef.ValIProperty m ->
    Val (Type, Maybe (Input.Payload m a)) -> HoleOption Guid m
mkHoleOptionFromInjected sugarContext exprPl stored val =
    HoleOption
    { _hoVal = baseExpr
    , _hoSugaredBaseExpr = sugar sugarContext exprPl baseExpr
    , _hoResults =
        do
            (result, inferContext) <-
                mkHoleResultValInjected exprPl val
                & (`runStateT` (sugarContext ^. ConvertM.scInferContext))
            let newSugarContext = sugarContext & ConvertM.scInferContext .~ inferContext
            return
                ( resultScore (fst <$> result)
                , mkHoleResult newSugarContext (exprPl ^. Input.entityId) stored result
                )
        <&> return & ListClass.joinL
    }
    where
        baseExpr = pruneExpr val
        pruneExpr (Val (_, Just{}) _) = P.hole
        pruneExpr (Val _ b) = b <&> pruneExpr & Val ()

data BaseExpr = SuggestedExpr (Val Infer.Payload) | SeedExpr (Val ())

getBaseExprVal :: BaseExpr -> Val ()
getBaseExprVal (SuggestedExpr v) = void v
getBaseExprVal (SeedExpr v) = v

mkHoleOption ::
    MonadA m => ConvertM.Context m ->
    Maybe (Val (Input.Payload m a)) ->
    Input.Payload m a -> ExprIRef.ValIProperty m ->
    BaseExpr -> HoleOption Guid m
mkHoleOption sugarContext mInjectedArg exprPl stored val =
    HoleOption
    { _hoVal = v
    , _hoSugaredBaseExpr = sugar sugarContext exprPl v
    , _hoResults = mkHoleResults mInjectedArg sugarContext exprPl stored val
    }
    where
        v = getBaseExprVal val

mkHoleSuggesteds ::
    MonadA m =>
    ConvertM.Context m -> Maybe (Val (Input.Payload m a)) ->
    Input.Payload m a -> ExprIRef.ValIProperty m -> [HoleOption Guid m]
mkHoleSuggesteds sugarContext mInjectedArg exprPl stored =
    exprPl ^. Input.inferred
    & Suggest.value
    <&> SuggestedExpr
    <&> mkHoleOption sugarContext mInjectedArg exprPl stored

addSuggestedOptions ::
    MonadA m =>
    [HoleOption Guid m] -> [HoleOption Guid m] -> [HoleOption Guid m]
addSuggestedOptions suggesteds options
    | null nonTrivial = options
    | otherwise = nonTrivial ++ filter (not . equivalentToSuggested) options
    where
        equivalentToSuggested x =
            any (V.alphaEq (x ^. hoVal)) (nonTrivial ^.. Lens.traverse . hoVal)
        nonTrivial = filter (Lens.nullOf (hoVal . ExprLens.valHole)) suggesteds

mkOptions ::
    MonadA m => ConvertM.Context m ->
    Maybe (Val (Input.Payload m a)) ->
    Input.Payload m a -> ExprIRef.ValIProperty m ->
    T m [HoleOption Guid m]
mkOptions sugarContext mInjectedArg exprPl stored =
    do
        nominalTids <-
            sugarContext ^. ConvertM.scCodeAnchors
            & Anchors.tids & Transaction.getP
        globals <-
            sugarContext ^. ConvertM.scCodeAnchors
            & Anchors.globals & Transaction.getP
        concat
            [ exprPl ^. Input.inferredScope
                & Infer.scopeToTypeMap
                & Map.keys
                & concatMap (getLocalScopeGetVars sugarContext)
            , globals
                & ( case sugarContext ^. ConvertM.scDefI of
                    Nothing -> id
                    Just defI -> filter (/= defI)
                  )
                <&> P.global . ExprIRef.globalId
            , do
                nominalTid <- nominalTids
                f <- [V.BFromNom, V.BToNom]
                [ V.Nom nominalTid P.hole & f & V.Val () ]
            , [ P.abs "NewLambda" P.hole, P.recEmpty, P.absurd
              , P.inject Builtins.nilTag P.recEmpty & P.toNom Builtins.listTid
              ]
            ]
            <&> SeedExpr
            <&> mkHoleOption sugarContext mInjectedArg exprPl stored
            & return

mkWritableHoleActions ::
    (MonadA m) =>
    Maybe (Val (Input.Payload m a)) ->
    Input.Payload m a -> ExprIRef.ValIProperty m ->
    ConvertM m (HoleActions Guid m)
mkWritableHoleActions mInjectedArg exprPl stored = do
    sugarContext <- ConvertM.readContext
    pure HoleActions
        { _holeOptions =
            mkOptions sugarContext mInjectedArg exprPl stored
            <&> addSuggestedOptions
                (mkHoleSuggesteds sugarContext mInjectedArg exprPl stored)
        , _holeOptionLiteralNum =
            return . mkHoleOption sugarContext mInjectedArg exprPl stored .
            SeedExpr . Val () . V.BLeaf .
            V.LLiteral . V.Literal Builtins.floatId . encodeS
        , _holeGuid = UniqueId.toGuid $ ExprIRef.unValI $ Property.value stored
        }

-- Ignoring alpha-renames:
consistentExprIds :: EntityId -> Val (Guid -> EntityId -> a) -> Val a
consistentExprIds = EntityId.randomizeExprAndParams . genFromHashable

infer :: MonadA m => Infer.Payload -> Val a -> IRefInfer.M m (Val (Infer.Payload, a))
infer holeInferred =
    IRefInfer.loadInferScope scopeAtHole
    where
        scopeAtHole = holeInferred ^. Infer.plScope

inferAssertSuccess ::
    MonadA m => ConvertM.Context m -> Infer.Payload ->
    Val a -> T m (Val (Infer.Payload, a))
inferAssertSuccess sugarContext holeInferred val =
    val
    & infer holeInferred
    & (`evalStateT` (sugarContext ^. ConvertM.scInferContext))
    & runEitherT
    <&> either
        (error . ("infer error on suggested val in hole: " ++) .
         show . pPrint) id

sugar ::
    (MonadA m, Monoid a) =>
    ConvertM.Context m -> Input.Payload m dummy -> Val a -> T m (ExpressionU m a)
sugar sugarContext exprPl val =
    inferAssertSuccess sugarContext holeInferred val
    <&> Lens.mapped %~ mkPayload
    <&> (EntityId.randomizeExprAndParams . genFromHashable)
        (exprPl ^. Input.entityId)
    <&> ConvertM.convertSubexpression
    >>= ConvertM.run sugarContext
    where
        mkPayload (pl, x) = Input.mkUnstoredPayload x pl
        holeInferred = exprPl ^. Input.inferred

mkHole ::
    (MonadA m, Monoid a) =>
    Maybe (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (Hole Guid m (ExpressionU m a))
mkHole mInjectedArg exprPl = do
    mActions <- traverse (mkWritableHoleActions mInjectedArg exprPl) (exprPl ^. Input.mStored)
    pure Hole
        { _holeMActions = mActions
        , _holeMArg = Nothing
        }

getLocalScopeGetVars :: MonadA m => ConvertM.Context m -> V.Var -> [Val ()]
getLocalScopeGetVars sugarContext par
    | sugarContext ^. ConvertM.scScopeInfo . ConvertM.siNullParams . Lens.contains par = []
    | otherwise = map mkFieldParam fieldTags ++ [var]
    where
        var = Val () (V.BLeaf (V.LVar par))
        fieldTags =
            ( sugarContext ^@..
                ConvertM.scScopeInfo . ConvertM.siTagParamInfos .>
                ( Lens.itraversed <.
                    Lens.to ConvertM.tpiFromParameters ) <.
                    Lens.filtered (== par)
            ) <&> fst
        mkFieldParam tag = V.GetField var tag & V.BGetField & Val ()

type HoleResultVal m a = Val (Infer.Payload, (Maybe (ExprIRef.ValI m), a))

markNotInjected :: HoleResultVal n () -> HoleResultVal n IsInjected
markNotInjected val = val <&> _2 . _2 .~ NotInjected

writeConvertTypeChecked ::
    (MonadA m, Monoid a) =>
    EntityId -> ConvertM.Context m -> ExprIRef.ValIProperty m ->
    HoleResultVal m a ->
    T m
    ( ExpressionU m a
    , Val (Input.Payload m a)
    , Val (ExprIRef.ValIProperty m, Input.Payload m a)
    )
writeConvertTypeChecked holeEntityId sugarContext holeStored inferredVal = do
    -- With the real stored guids:
    writtenExpr <-
        inferredVal
        <&> intoStorePoint
        & writeExprMStored (Property.value holeStored)
        <&> ExprIRef.addProperties (Property.set holeStored)
        <&> fmap toPayload
    let -- Replace the entity ids with consistent ones:

        -- The sugar convert must apply *inside* the forked transaction
        -- upon the *written* expr because we actually make use of the
        -- resulting actions (e.g: press ',' on a list hole result).
        -- However, the written expr goes crazy with new guids every time.
        --
        -- So, we do something a bit odd: Take the written expr with
        -- its in-tact stored allowing actions to be built correctly
        -- but replace the Input.entityId with determinstic/consistent
        -- pseudo-random generated ones that preserve proper
        -- animations and cursor navigation. The guids are kept as
        -- metadata anchors.

        makeConsistentPayload (False, (_, pl)) _guid entityId = pl
            & Input.entityId .~ entityId
        makeConsistentPayload (True, (_, pl)) _ _ = pl
        consistentExpr =
            writtenExpr
            <&> makeConsistentPayload
            & consistentExprIds holeEntityId
    converted <-
        consistentExpr
        & ConvertM.convertSubexpression
        & ConvertM.run sugarContext
    return
        ( converted
        , consistentExpr
        , writtenExpr <&> snd
        )
    where
        intoStorePoint (inferred, (mStorePoint, a)) =
            (mStorePoint, (inferred, Lens.has Lens._Just mStorePoint, a))
        toPayload (stored, (inferred, wasStored, a)) =
            -- TODO: Evaluate hole results instead of Map.empty's?
            ( wasStored
            , ( stored
              , Input.mkPayload a inferred (CurAndPrev noEval noEval) stored
              )
            )
        noEval = Input.EvalResultsForExpr Map.empty Map.empty

idTranslations ::
    Val (EntityId, Type) ->
    Val EntityId ->
    [(EntityId, EntityId)]
idTranslations consistentExpr dest
    | V.alphaEq (void src) (void dest)
        = concat
            [ pairUp V.payload
            , pairUp params
            , pairUpTags ExprLens._BRecExtend EntityId.ofRecExtendTag
            , pairUpTags ExprLens._BGetField EntityId.ofGetFieldTag
            , pairUpTags ExprLens._BCase EntityId.ofCaseTag
            , pairUpTags ExprLens._BInject EntityId.ofInjectTag
            , pairUpLambdaRecordParams (consistentExpr <&> snd) dest
            ]
    | otherwise =
          error $ "Hole.idTranslations of mismatching expressions: " ++
          show (void src) ++ " " ++ show (void dest)
    where
        pairUpLambdaRecordParams aVal bVal =
            case (aVal, bVal) of
            (V.Val srcType (V.BAbs (V.Lam avar _)),
              V.Val _ (V.BAbs (V.Lam bvar _)))
                -- TODO: Use a _TRecord prism alternative that verifies the
                -- record is closed
                -> [ ( EntityId.ofLambdaTagParam avar tag
                          , EntityId.ofLambdaTagParam bvar tag
                          )
                      | tag <-
                              srcType ^..
                              ExprLens._TFun . _1 . ExprLens._TRecord . ExprLens.compositeTags
                      ] ++ recurse
            _ -> recurse
            where
                recurse =
                    zipWith pairUpLambdaRecordParams
                    (aVal ^.. V.body . Lens.folded)
                    (bVal ^.. V.body . Lens.folded)
                    & concat
        src = consistentExpr <&> fst
        pairUp l = zip (src ^.. ExprLens.subExprs . l) (dest ^.. ExprLens.subExprs . l)
        pairUpTags prism toEntityId =
            pairUp $
            Lens.filtered (Lens.has (V.body . prism)) . V.payload . Lens.to toEntityId
        params =
            V.body . ExprLens._BAbs . V.lamParamId .
            Lens.to EntityId.ofLambdaParam

eitherTtoListT :: Monad m => EitherT err m a -> ListT m a
eitherTtoListT = ListClass.joinL . liftM (ListClass.fromList . (^.. Lens._Right)) . runEitherT

eitherToListT :: Monad m => Either t a -> ListT m a
eitherToListT (Left _) = mempty
eitherToListT (Right x) = return x

applyForms ::
    MonadA m =>
    a -> Val (Infer.Payload, a) ->
    StateT Infer.Context (ListT (T m)) (Val (Infer.Payload, a))
applyForms _ v@(Val _ V.BAbs {}) = return v
applyForms empty val =
    case inferPl ^. Infer.plType of
    TVar tv
        | any (`Lens.has` val)
            [ ExprLens.valVar
            , ExprLens.valGetField . V.getFieldRecord . ExprLens.valVar
            ] ->
            -- a variable that's compatible with a function type
            return val <|>
            do
                arg <- freshVar "af"
                res <- freshVar "af"
                let varTyp = TFun arg res
                unify varTyp (TVar tv)
                    & Infer.run & mapStateT assertSuccess
                return $ Val (plSameScope res) $ V.BApp $ V.Apply val $
                    Val (plSameScope arg) (V.BLeaf V.LHole)
        where
            assertSuccess (Left err) =
                fail $
                "Unify of a tv with function type should always succeed, but failed: " ++
                prettyShow err
            assertSuccess (Right x) = return x
            freshVar = Infer.run . Infer.freshInferredVar (inferPl ^. Infer.plScope)
            scope = inferPl ^. Infer.plScope
            plSameScope t = (Infer.Payload t scope, empty)
    TRecord{} | Lens.has ExprLens.valVar val ->
        -- A "params record" (or just a let item which is a record..)
        return val
    _ ->
        val & V.payload . _1 . Infer.plType %%~ orderType
        <&> Suggest.fillHoles empty
        >>= Suggest.valueConversion IRefInfer.loadNominal empty
        <&> mapStateT ListClass.fromList
        & lift & lift & join
    where
        inferPl = val ^. V.payload . _1

holeWrapIfNeeded ::
    Monad m =>
    Type -> Val (Infer.Payload, (Maybe a, IsInjected)) ->
    StateT Infer.Context m (Val (Infer.Payload, (Maybe a, IsInjected)))
holeWrapIfNeeded holeType val =
    do
        unifyResult <-
            unify holeType (val ^. V.payload . _1 . Infer.plType) & liftInfer
        updated <- Update.inferredVal val & liftUpdate
        case unifyResult of
            Right{} -> return updated
            Left{} -> holeWrap holeType updated & liftUpdate
    where
        liftUpdate = State.gets . Update.run
        liftInfer = stateEitherSequence . Infer.run

holeWrap ::
    Type -> Val (Infer.Payload, (Maybe a, IsInjected)) ->
    Update (Val (Infer.Payload, (Maybe a, IsInjected)))
holeWrap resultType val =
    update resultType <&> mk
    where
        mk updatedType =
            V.Apply func val & V.BApp
            & Val (plSameScope updatedType, emptyPl)
        plSameScope typ = inferPl & Infer.plType .~ typ
        inferPl = val ^. V.payload . _1
        func = Val (plSameScope funcType, emptyPl) $ V.BLeaf V.LHole
        funcType = TFun (inferPl ^. Infer.plType) resultType
        emptyPl = (Nothing, NotInjected)

replaceEachUnwrappedHole :: Applicative f => (a -> f (Val a)) -> Val a -> [f (Val a)]
replaceEachUnwrappedHole replaceHole =
    map fst . filter snd . (`runStateT` False) . go
    where
        go oldVal@(Val x body) = do
            alreadyReplaced <- State.get
            if alreadyReplaced
                then return (pure oldVal)
                else
                    case body of
                    V.BLeaf V.LHole ->
                        join $ lift
                            [ replace x
                            , return $ pure oldVal
                            ]
                    V.BApp (V.Apply (Val f (V.BLeaf V.LHole)) arg@(Val _ (V.BLeaf V.LHole))) ->
                        join $ lift
                            [ replace f
                                <&> fmap (Val x . V.BApp . (`V.Apply` arg))
                            , return $ pure oldVal
                            ]
                    _ -> traverse go body <&> fmap (Val x) . sequenceA
        replace x =
            do
                State.put True
                return $ replaceHole x

stateEitherSequence :: Monad m => StateT s (Either l) r -> StateT s m (Either l r)
stateEitherSequence (StateT f) =
    StateT $ \s0 ->
    case f s0 of
    Right (r, s1) -> return (Right r, s1)
    Left l -> return (Left l, s0)

holeResultsInject ::
    Monad m =>
    Val (Input.Payload n a) -> HoleResultVal n () ->
    StateT Infer.Context (ListT m) (HoleResultVal n IsInjected)
holeResultsInject injectedArg val =
    do
        (Monoid.First (Just injectPointPl), filledVal) <-
            val
            & markNotInjected
            & replaceEachUnwrappedHole inject
            & ListClass.fromList
            & lift
        unify injectedType (injectPointPl ^. _1 . Infer.plType)
            & Infer.run
            & mapStateT eitherToListT
        return filledVal
    where
        onInjectedPayload pl =
                ( pl ^. Input.inferred
                , (pl ^? Input.mStored . Lens._Just . Property.pVal, NotInjected)
                )
        inject pl =
                ( Monoid.First (Just pl)
                , injectedArg
                    <&> onInjectedPayload
                    & V.payload . _2 . _2 .~ Injected
                )
        injectedType = injectedArg ^. V.payload . Input.inferredType

mkHoleResultValInjected ::
    MonadA m =>
    Input.Payload m dummy ->
    Val (Type, Maybe (Input.Payload m a)) ->
    StateT Infer.Context (T m) (HoleResultVal m IsInjected)
mkHoleResultValInjected exprPl val =
    val <&> onPl
    & holeWrapIfNeeded (inferred ^. Infer.plType)
    where
        inferred = exprPl ^. Input.inferred
        onPl (typ, mInputPl) =
            ( inferred & Infer.plType .~ typ
            , case mInputPl of
              Nothing -> (Nothing, NotInjected)
              Just inputPl ->
                (inputPl ^. Input.mStored <&> Property.value, Injected)
            )

mkHoleResultVals ::
    MonadA m =>
    Maybe (Val (Input.Payload m a)) ->
    Input.Payload m dummy ->
    BaseExpr ->
    StateT Infer.Context (ListT (T m)) (HoleResultVal m IsInjected)
mkHoleResultVals mInjectedArg exprPl base =
    case base of
    SeedExpr seed ->
        infer inferred seed
        & mapStateT eitherTtoListT
        <&> Lens.traversed . _2 %~ (,) Nothing
        >>= applyForms (Nothing, ())
    SuggestedExpr sugg ->
        sugg & Lens.traversed %~ flip (,) (Nothing, ()) & return
    >>= maybe (return . markNotInjected) holeResultsInject mInjectedArg
    >>= holeWrapIfNeeded (inferred ^. Infer.plType)
    where
        inferred = exprPl ^. Input.inferred

mkHoleResult ::
    MonadA m =>
    ConvertM.Context m -> EntityId ->
    ExprIRef.ValIProperty m -> HoleResultVal m IsInjected ->
    T m (HoleResult Guid m)
mkHoleResult sugarContext entityId stored val =
    do
        ((fConverted, fConsistentExpr, fWrittenExpr), forkedChanges) <-
            Transaction.fork $
            writeConvertTypeChecked entityId
            sugarContext stored val
        return
            HoleResult
            { _holeResultConverted = fConverted
            , _holeResultPick =
                mkPickedResult fConsistentExpr fWrittenExpr <$ Transaction.merge forkedChanges
            }
    where
        mkPickedResult consistentExpr writtenExpr =
            PickedResult
            { _prIdTranslation =
                idTranslations
                ( consistentExpr <&>
                    \input ->
                    ( input ^. Input.entityId
                    , input ^. Input.inferredType
                    ) )
                (writtenExpr <&> EntityId.ofValI . Property.value . fst)
            }

mkHoleResults ::
    MonadA m =>
    Maybe (Val (Input.Payload m a)) ->
    ConvertM.Context m ->
    Input.Payload m dummy -> ExprIRef.ValIProperty m ->
    BaseExpr ->
    ListT (T m) (HoleResultScore, T m (HoleResult Guid m))
mkHoleResults mInjectedArg sugarContext exprPl stored base =
    do
        (val, inferContext) <-
            mkHoleResultVals mInjectedArg exprPl base
            & (`runStateT` (sugarContext ^. ConvertM.scInferContext))
        let newSugarContext = sugarContext & ConvertM.scInferContext .~ inferContext
        return
            ( resultScore (fst <$> val)
            , mkHoleResult newSugarContext (exprPl ^. Input.entityId) stored val
            )

randomizeNonStoredParamIds ::
    Random.StdGen -> ExprStorePoint m a -> ExprStorePoint m a
randomizeNonStoredParamIds gen =
    GenIds.randomizeParamIdsG id nameGen Map.empty $ \_ _ pl -> pl
    where
        nameGen = GenIds.onNgMakeName f $ GenIds.randomNameGen gen
        f n _        prevEntityId (Just _, _) = (prevEntityId, n)
        f _ prevFunc prevEntityId pl@(Nothing, _) = prevFunc prevEntityId pl

writeExprMStored ::
    MonadA m =>
    ExprIRef.ValI m ->
    ExprStorePoint m a ->
    T m (Val (ExprIRef.ValI m, a))
writeExprMStored exprIRef exprMStorePoint =
    do
        key <- Transaction.newKey
        exprMStorePoint
            & randomizeNonStoredParamIds (genFromHashable key)
            & ExprIRef.writeValWithStoredSubexpressions exprIRef
