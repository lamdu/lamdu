{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.Sugar.Convert.Hole
    ( convert
      -- Used by Convert.GetVar:
    , fragmentVar
      -- Used by Convert.Fragment:
    , mkOptions
    , mkHoleOption, mkHoleOptionFromFragment, addSuggestedOptions
    , BaseExpr(..)
    ) where

import           Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import           Control.Monad (filterM)
import           Control.Monad.ListT (ListT)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.State (StateT(..), mapStateT, evalState, state)
import qualified Control.Monad.Trans.State as State
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Binary as Binary
import           Data.Bits (xor)
import qualified Data.ByteString as SBS
import           Data.ByteString.Utils (lazifyBS, strictifyBS)
import           Data.CurAndPrev (CurAndPrev(..))
import           Data.Functor.Identity (Identity(..))
import qualified Data.List.Class as ListClass
import qualified Data.Map as Map
import qualified Data.Property as Property
import qualified Data.Set as Set
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.UUID as UUID
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import           Lamdu.Calc.Type (Type(..))
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.Nominal as N
import           Lamdu.Calc.Type.Scheme (schemeType, mono)
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val(..))
import qualified Lamdu.Calc.Val.Annotated as Val
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Def
import qualified Lamdu.Expr.GenIds as GenIds
import           Lamdu.Expr.IRef (ValIProperty, ValI(..), DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Infer.Trans as InferT
import           Lamdu.Infer.Unify (unify)
import           Lamdu.Infer.Update (Update, update)
import qualified Lamdu.Infer.Update as Update
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import           Lamdu.Sugar.Convert.Hole.ResultScore (resultScore)
import qualified Lamdu.Sugar.Convert.Hole.Suggest as Suggest
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.OrderTags (orderType)
import           Lamdu.Sugar.Types
import qualified Revision.Deltum.IRef as IRef
import           Revision.Deltum.Transaction (Transaction)
import qualified Revision.Deltum.Transaction as Transaction
import           System.Random (random)
import qualified System.Random as Random
import           System.Random.Utils (genFromHashable)
import           Text.PrettyPrint.HughesPJClass (prettyShow)

import           Lamdu.Prelude

type T = Transaction

type ExprStorePoint m a = Val (Maybe (ValI m), a)

convert :: Monad m => Input.Payload m a -> ConvertM m (ExpressionU m a)
convert exprPl =
    Hole
    <$> mkOptions Nothing exprPl
    <*> mkLiteralOptions exprPl
    <*> pure Nothing
    <&> BodyHole
    >>= addActions exprPl
    <&> rPayload . plActions . mSetToHole .~ Nothing

mkHoleOptionFromFragment ::
    Monad m =>
    ConvertM.Context m ->
    Input.Payload m a ->
    Val (Type, Maybe (Input.Payload m a)) ->
    HoleOption (T m) (Expression InternalName (T m) ())
mkHoleOptionFromFragment sugarContext exprPl val =
    HoleOption
    { _hoVal = baseExpr
    , _hoSugaredBaseExpr = sugar sugarContext exprPl baseExpr
    , _hoResults =
        do
            (result, inferContext) <-
                mkHoleResultValFragment exprPl val
                & (`runStateT` (sugarContext ^. ConvertM.scInferContext))
            let depsProp = sugarContext ^. ConvertM.scFrozenDeps
            newDeps <-
                loadNewDeps (depsProp ^. Property.pVal)
                (exprPl ^. Input.inferred . Infer.plScope) val
            let newSugarContext =
                    sugarContext
                    & ConvertM.scInferContext .~ inferContext
                    & ConvertM.scFrozenDeps . Property.pVal .~ newDeps
            let updateDeps = Property.set depsProp newDeps
            pure
                ( resultScore (result <&> fst)
                , mkHoleResult newSugarContext updateDeps (exprPl ^. Input.stored) result
                )
        <&> pure & ListClass.joinL
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
    Monad m =>
    ConvertM.Context m ->
    Maybe (Val (Input.Payload m a)) ->
    Input.Payload m a ->
    BaseExpr ->
    HoleOption (T m) (Expression InternalName (T m) ())
mkHoleOption sugarContext mFragment exprPl val =
    HoleOption
    { _hoVal = v
    , _hoSugaredBaseExpr = sugar sugarContext exprPl v
    , _hoResults = mkHoleResults mFragment sugarContext exprPl val
    }
    where
        v = getBaseExprVal val

mkHoleSuggesteds ::
    Monad m =>
    ConvertM.Context m -> Maybe (Val (Input.Payload m a)) -> Input.Payload m a ->
    [HoleOption (T m) (Expression InternalName (T m) ())]
mkHoleSuggesteds sugarContext mFragment exprPl =
    exprPl ^. Input.inferred
    & Suggest.value
    <&> SuggestedExpr
    <&> mkHoleOption sugarContext mFragment exprPl

addSuggestedOptions :: [HoleOption m a] -> [HoleOption m a] -> [HoleOption m a]
addSuggestedOptions suggesteds options
    | null nonTrivial = options
    | otherwise = nonTrivial ++ filter (not . equivalentToSuggested) options
    where
        equivalentToSuggested x =
            any (Val.couldEq (x ^. hoVal)) (nonTrivial ^.. Lens.traverse . hoVal)
        nonTrivial = filter (Lens.nullOf (hoVal . ExprLens.valHole)) suggesteds

isLiveGlobal :: Monad m => DefI m -> T m Bool
isLiveGlobal defI =
    Anchors.assocDefinitionState defI
    & Transaction.getP
    <&> (== LiveDefinition)

getListing ::
    Monad m =>
    (Anchors.CodeAnchors f -> Transaction.MkProperty m (Set a)) ->
    ConvertM.Context f -> Transaction m [a]
getListing anchor sugarContext =
    sugarContext ^. ConvertM.scCodeAnchors
    & anchor & Transaction.getP <&> Set.toList

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
locals sugarContext exprPl =
    exprPl ^. Input.inferredScope
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
                [ V.Nom tid P.hole & f & Val () ]
        mkToNomInjections tid nominal =
            do
                (tag, _typ) <-
                    nominal ^..
                    N.nomType . N._NominalType . schemeType . T._TVariant .
                    ExprLens.compositeFields
                let inject = V.Inject tag P.hole & V.BInject & Val ()
                [ inject & V.Nom tid & V.BToNom & Val () ]

mkOptions ::
    Monad m =>
    Maybe (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (T m [HoleOption (T m) (Expression InternalName (T m) ())])
mkOptions mFragment exprPl =
    Lens.view id
    <&>
    \sugarContext ->
    do
        nominalOptions <- getNominals sugarContext <&> mkNominalOptions
        globals <- getGlobals sugarContext
        tags <- getTags sugarContext
        concat
            [ locals sugarContext exprPl
                & concatMap (getLocalScopeGetVars sugarContext)
            , globals <&> P.var . ExprIRef.globalId
            , tags <&> (`P.inject` P.hole)
            , nominalOptions
            , [ P.abs "NewLambda" P.hole
              , P.recEmpty
              , P.absurd
              ]
            ]
            <&> SeedExpr
            <&> mkHoleOption sugarContext mFragment exprPl
            & addSuggestedOptions (mkHoleSuggesteds sugarContext mFragment exprPl)
            & pure

loadDeps :: Monad m => [V.Var] -> [T.NominalId] -> T m Infer.Dependencies
loadDeps vars noms =
    Infer.Deps
    <$> (mapM loadVar vars <&> Map.fromList)
    <*> (mapM loadNom noms <&> Map.fromList)
    where
        loadVar globalId =
            ExprIRef.defI globalId & Transaction.readIRef
            <&> (^. Def.defType) <&> (,) globalId
        loadNom nomId = Load.nominal nomId <&> (,) nomId

loadNewDeps ::
    Monad m => Infer.Dependencies -> Infer.Scope -> Val a -> T m Infer.Dependencies
loadNewDeps currentDeps scope val =
    loadDeps newDepVars newNoms
    <&> mappend currentDeps
    where
        scopeVars = Infer.scopeToTypeMap scope & Map.keysSet
        newDeps depsLens valLens =
            Set.fromList (val ^.. valLens)
            `Set.difference` Map.keysSet (currentDeps ^. depsLens)
            & Set.toList
        newDepVars = newDeps Infer.depsGlobalTypes (ExprLens.valGlobals scopeVars)
        newNoms = newDeps Infer.depsNominals ExprLens.valNominals

-- Unstored and without eval results (e.g: hole result)
prepareUnstoredPayloads ::
    Val (Infer.Payload, EntityId, a) ->
    Val (Input.Payload m a)
prepareUnstoredPayloads val =
    val <&> mk & Input.preparePayloads
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
    T m (Expression InternalName (T m) a)
sugar sugarContext exprPl val =
    val
    <&> mkPayload
    & (EntityId.randomizeExprAndParams . genFromHashable)
        (exprPl ^. Input.entityId)
    & prepareUnstoredPayloads
    & ConvertM.convertSubexpression
    & ConvertM.run sugarContext
    <&> Lens.mapped %~ (^. pUserData)
    where
        mkPayload x entityId = (fakeInferPayload, entityId, x)
        -- A fake Infer payload we use to sugar the base expressions.
        -- Currently it is a function type because
        -- otherwise sugaring of lambdas crashes.
        fakeInferPayload =
            Infer.Payload
            { Infer._plType = TFun (TVar "fakeInput") (TVar "fakeOutput")
            , Infer._plScope = exprPl ^. Input.inferred . Infer.plScope
            }

mkLiteralOptions ::
    Monad m =>
    Input.Payload m a ->
    ConvertM m (Literal Identity -> T m (HoleResultScore, T m (HoleResult (T m) (Expression InternalName (T m) ()))))
mkLiteralOptions exprPl =
    Lens.view id
    <&>
    \sugarContext ->
    let mkOption updateDeps val =
            pure
            ( HoleResultScore 0 []
            , fixedVal <&> convPl
                & mkHoleResult sugarContext updateDeps (exprPl ^. Input.stored)
            )
            where
                typ = val ^. Val.payload
                fixedVal
                    | inferredType == typ || Lens.has T._TVar inferredType = val
                    | otherwise =
                        V.Apply (Val (T.TFun typ inferredType) (V.BLeaf V.LHole)) val
                        & V.BApp
                        & Val inferredType
        addTextDep = Property.pureModify (sugarContext ^. ConvertM.scFrozenDeps) (<> textDep)
    in
    \case
    LiteralNum (Identity x) -> PrimVal.Float x & literalExpr & mkOption (pure ())
    LiteralBytes (Identity x) -> PrimVal.Bytes x & literalExpr & mkOption (pure ())
    LiteralText (Identity x) ->
        encodeUtf8 x
        & PrimVal.Bytes
        & literalExpr
        & V.Nom Builtins.textTid
        & V.BToNom
        & Val (T.TInst Builtins.textTid mempty)
        & mkOption addTextDep
    where
        literalExpr v =
            V.LLiteral prim & V.BLeaf & Val (T.TInst (prim ^. V.primType) mempty)
            where
                prim = PrimVal.fromKnown v
        convPl t = (Infer.Payload t Infer.emptyScope, (Nothing, NotFragment))
        inferredType = exprPl ^. Input.inferred . Infer.plType
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
        var = Val () (V.BLeaf (V.LVar par))
        fieldTags =
            ( sugarContext ^@..
                ConvertM.scScopeInfo . ConvertM.siTagParamInfos .>
                ( Lens.itraversed <.
                    ConvertM._TagFieldParam . Lens.to ConvertM.tpiFromParameters ) <.
                    Lens.filtered (== par)
            ) <&> fst
        mkFieldParam tag = V.GetField var tag & V.BGetField & Val ()

data IsFragment = IsFragment | NotFragment
type HoleResultVal m a = Val (Infer.Payload, (Maybe (ValI m), a))

markNotFragment :: HoleResultVal n () -> HoleResultVal n IsFragment
markNotFragment val = val <&> _2 . _2 .~ NotFragment

-- TODO: Unify type according to IsFragment, avoid magic var
fragmentVar :: V.Var
fragmentVar = "HOLE FRAGMENT EXPR"

replaceFragment :: Val (Input.Payload m IsFragment) -> Val (Input.Payload m ())
replaceFragment (Val pl body) =
    case pl ^. Input.userData of
    IsFragment -> V.LVar fragmentVar & V.BLeaf
    NotFragment -> body & traverse %~ replaceFragment
    & Val (void pl)

writeConvertTypeChecked ::
    Monad m =>
    ConvertM.Context m -> ValIProperty m ->
    HoleResultVal m IsFragment ->
    T m (ExpressionU m ())
writeConvertTypeChecked sugarContext holeStored inferredVal =
    do
        writtenExpr <-
            inferredVal
            <&> intoStorePoint
            & writeExprMStored (Property.value holeStored)
            <&> ExprIRef.addProperties (Property.set holeStored)
            <&> fmap snd . Input.preparePayloads . fmap toPayload
        Property.set holeStored (writtenExpr ^. Val.payload . _1 . Property.pVal)
        replaceFragment (writtenExpr <&> snd)
            & ConvertM.convertSubexpression
            & ConvertM.run sugarContext
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

exceptToListT :: Monad m => Either t a -> ListT m a
exceptToListT (Left _) = mempty
exceptToListT (Right x) = pure x

applyForms ::
    Monad m =>
    a -> Val (Infer.Payload, a) ->
    StateT Infer.Context (ListT (T m)) (Val (Infer.Payload, a))
applyForms _ v@(Val _ V.BLam {}) = pure v
applyForms _ v@(Val pl0 (V.BInject (V.Inject tag (Val pl1 (V.BLeaf V.LHole))))) =
    pure (Val pl0 (V.BInject (V.Inject tag (Val pl1 (V.BLeaf V.LRecEmpty)))))
    <|> pure v
applyForms empty val =
    case inferPl ^. Infer.plType of
    TVar tv
        | any (`Lens.has` val)
            [ ExprLens.valVar
            , ExprLens.valGetField . V.getFieldRecord . ExprLens.valVar
            ] ->
            -- a variable that's compatible with a function type
            pure val <|>
            do
                arg <- freshVar "af"
                res <- freshVar "af"
                let varTyp = TFun arg res
                unify varTyp (TVar tv)
                    & Infer.run & mapStateT assertSuccess
                pure $ Val (plSameScope res) $ V.BApp $ V.Apply val $
                    Val (plSameScope arg) (V.BLeaf V.LHole)
        where
            assertSuccess (Left err) =
                fail $
                "Unify of a tv with function type should always succeed, but failed: " ++
                prettyShow err
            assertSuccess (Right x) = pure x
            freshVar = Infer.run . Infer.freshInferredVar (inferPl ^. Infer.plScope)
            scope = inferPl ^. Infer.plScope
            plSameScope t = (Infer.Payload t scope, empty)
    TRecord{} | Lens.has ExprLens.valVar val ->
        -- A "params record" (or just a let item which is a record..)
        pure val
    _ ->
        val & Val.payload . _1 . Infer.plType %%~ orderType
        <&> Suggest.fillHoles empty
        >>= Suggest.valueConversion Load.nominal empty
        <&> mapStateT ListClass.fromList
        & lift & lift & join
    where
        inferPl = val ^. Val.payload . _1

detachValIfNeeded ::
    Monad m =>
    Type -> Val (Infer.Payload, (Maybe a, IsFragment)) ->
    StateT Infer.Context m (Val (Infer.Payload, (Maybe a, IsFragment)))
detachValIfNeeded holeType val =
    do
        unifyResult <-
            unify holeType (val ^. Val.payload . _1 . Infer.plType) & liftInfer
        updated <- Update.inferredVal val & liftUpdate
        case unifyResult of
            Right{} -> pure updated
            Left{} -> detachVal holeType updated & liftUpdate
    where
        liftUpdate = State.gets . Update.run
        liftInfer = stateEitherSequence . Infer.run

detachVal ::
    Type -> Val (Infer.Payload, (Maybe a, IsFragment)) ->
    Update (Val (Infer.Payload, (Maybe a, IsFragment)))
detachVal resultType val =
    update resultType <&> mk
    where
        mk updatedType =
            V.Apply func val & V.BApp
            & Val (plSameScope updatedType, emptyPl)
        plSameScope typ = inferPl & Infer.plType .~ typ
        inferPl = val ^. Val.payload . _1
        func = Val (plSameScope funcType, emptyPl) $ V.BLeaf V.LHole
        funcType = TFun (inferPl ^. Infer.plType) resultType
        emptyPl = (Nothing, NotFragment)

emplaceInHoles :: Applicative f => (a -> f (Val a)) -> Val a -> [f (Val a)]
emplaceInHoles replaceHole =
    map fst . filter snd . (`runStateT` False) . go
    where
        go oldVal@(Val x body) =
            do
                alreadyReplaced <- State.get
                if alreadyReplaced
                    then pure (pure oldVal)
                    else
                        case body of
                        V.BLeaf V.LHole ->
                            join $ lift
                                [ replace x
                                , pure (pure oldVal)
                                ]
                        V.BApp (V.Apply (Val f (V.BLeaf V.LHole)) arg@(Val _ (V.BLeaf V.LHole))) ->
                            join $ lift
                                [ replace f
                                    <&> fmap (Val x . V.BApp . (`V.Apply` arg))
                                , pure (pure oldVal)
                                ]
                        _ -> traverse go body <&> fmap (Val x) . sequenceA
        replace x = replaceHole x <$ State.put True

stateEitherSequence :: Monad m => StateT s (Either l) r -> StateT s m (Either l r)
stateEitherSequence (StateT f) =
    StateT $ \s0 ->
    pure $
    case f s0 of
    Right (r, s1) -> (Right r, s1)
    Left l -> (Left l, s0)

holeResultsEmplaceFragment ::
    Monad m =>
    Val (Input.Payload n a) -> HoleResultVal n () ->
    StateT Infer.Context (ListT m) (HoleResultVal n IsFragment)
holeResultsEmplaceFragment rawFragmentExpr val =
    markNotFragment val
    & emplaceInHoles emplace
    & ListClass.fromList
    & lift
    & join
    where
        emplace pl =
            ListClass.fromList
            [ fragmentExpr
              <$ (mapStateT exceptToListT . Infer.run . unify fragmentType)
                  (fst pl ^. Infer.plType)
            , V.Apply
                (Val (fst pl & Infer.plType %~ (`T.TFun` fragmentType), (Nothing, NotFragment)) (V.BLeaf V.LHole))
                fragmentExpr
                & V.BApp & Val (fst pl, (Nothing, NotFragment))
                & pure
            ]
            & lift
            & join
            & mapStateT (ListClass.take 1)
        fragmentExpr = rawFragmentExpr <&> onFragmentPayload
        onFragmentPayload pl =
            ( pl ^. Input.inferred
            , (Just (pl ^. Input.stored . Property.pVal), IsFragment)
            )
        fragmentType = rawFragmentExpr ^. Val.payload . Input.inferredType

mkHoleResultValFragment ::
    Monad m =>
    Input.Payload m dummy ->
    Val (Type, Maybe (Input.Payload m a)) ->
    StateT Infer.Context (T m) (HoleResultVal m IsFragment)
mkHoleResultValFragment exprPl val =
    val <&> onPl
    & detachValIfNeeded (inferred ^. Infer.plType)
    where
        inferred = exprPl ^. Input.inferred
        onPl (typ, mInputPl) =
            ( inferred & Infer.plType .~ typ
            , case mInputPl of
              Nothing -> (Nothing, NotFragment)
              Just inputPl ->
                (inputPl ^. Input.stored & Property.value & Just, IsFragment)
            )

mkHoleResultVals ::
    Monad m =>
    Transaction.Property m Infer.Dependencies ->
    Maybe (Val (Input.Payload m a)) ->
    Input.Payload m dummy ->
    BaseExpr ->
    StateT Infer.Context (ListT (T m)) (Infer.Dependencies, HoleResultVal m IsFragment)
mkHoleResultVals frozenDeps mFragment exprPl base =
    case base of
    SeedExpr seed ->
        do
            (seedDeps, inferResult) <-
                do
                    seedDeps <- loadTheNewDeps seed
                    inferResult <-
                        Infer.infer seedDeps scope seed & InferT.liftInfer
                        <&> Lens.traversed . _2 %~ (,) Nothing
                    pure (seedDeps, inferResult)
                & mapStateT exceptTtoListT
            form <- applyForms (Nothing, ()) inferResult
            newDeps <- loadNewDeps seedDeps scope form & lift & lift
            pure (newDeps, form)
    SuggestedExpr sugg ->
        (,)
        <$> mapStateT exceptTtoListT (loadTheNewDeps sugg)
        ?? (sugg & Lens.traversed %~ flip (,) (Nothing, ()))
    >>= _2 %%~ post
    where
        loadTheNewDeps expr =
            loadNewDeps (frozenDeps ^. Property.pVal) scope expr
            & InferT.liftInner
        scope = inferred ^. Infer.plScope
        inferred = exprPl ^. Input.inferred
        post x =
            x
            & maybe (pure . markNotFragment) holeResultsEmplaceFragment mFragment
            >>= detachValIfNeeded (inferred ^. Infer.plType)

mkHoleResult ::
    Monad m =>
    ConvertM.Context m -> Transaction m () ->
    ValIProperty m -> HoleResultVal m IsFragment ->
    T m (HoleResult (T m) (Expression InternalName (T m) ()))
mkHoleResult sugarContext updateDeps stored val =
    do
        updateDeps
        writeConvertTypeChecked sugarContext stored val
    & Transaction.fork
    <&> \(fConverted, forkedChanges) ->
    HoleResult
    { _holeResultConverted = fConverted <&> (^. pUserData)
    , _holeResultPick =
        do
            Transaction.merge forkedChanges
            sugarContext ^. ConvertM.scPostProcessRoot & void
    }

mkHoleResults ::
    Monad m =>
    Maybe (Val (Input.Payload m a)) ->
    ConvertM.Context m ->
    Input.Payload m dummy ->
    BaseExpr ->
    ListT (T m)
    ( HoleResultScore
    , T m (HoleResult (T m) (Expression InternalName (T m) ()))
    )
mkHoleResults mFragment sugarContext exprPl base =
    mkHoleResultVals (sugarContext ^. ConvertM.scFrozenDeps)
    mFragment exprPl base
    & (`runStateT` (sugarContext ^. ConvertM.scInferContext))
    <&> \((newDeps, val), inferContext) ->
    let newSugarContext =
            sugarContext
            & ConvertM.scInferContext .~ inferContext
            & ConvertM.scFrozenDeps . Property.pVal .~ newDeps
        updateDeps = newDeps & sugarContext ^. ConvertM.scFrozenDeps . Property.pSet
    in  ( resultScore (val <&> fst)
        , mkHoleResult newSugarContext updateDeps (exprPl ^. Input.stored) val
        )

xorBS :: ByteString -> ByteString -> ByteString
xorBS x y = SBS.pack $ SBS.zipWith xor x y

randomizeNonStoredParamIds ::
    Random.StdGen -> ExprStorePoint m a -> ExprStorePoint m a
randomizeNonStoredParamIds gen =
    GenIds.randomizeParamIdsG id nameGen Map.empty $ \_ _ pl -> pl
    where
        nameGen = GenIds.onNgMakeName f $ GenIds.randomNameGen gen
        f n _        prevEntityId (Just _, _) = (prevEntityId, n)
        f _ prevFunc prevEntityId pl@(Nothing, _) = prevFunc prevEntityId pl

randomizeNonStoredRefs :: ByteString -> Random.StdGen -> ExprStorePoint m a -> ExprStorePoint m a
randomizeNonStoredRefs uniqueIdent gen val =
    evalState ((traverse . _1) f val) gen
    where
        f Nothing =
            state random
            <&> UUID.toByteString <&> strictifyBS
            <&> xorBS uniqueIdent
            <&> lazifyBS <&> UUID.fromByteString
            <&> fromMaybe (error "cant parse UUID")
            <&> IRef.unsafeFromUUID <&> ValI <&> Just
        f (Just x) = Just x & pure

writeExprMStored ::
    Monad m =>
    ValI m ->
    ExprStorePoint m a ->
    T m (Val (ValI m, a))
writeExprMStored exprIRef exprMStorePoint =
    exprMStorePoint
    & randomizeNonStoredParamIds genParamIds
    & randomizeNonStoredRefs uniqueIdent genRefs
    & ExprIRef.writeValWithStoredSubexpressions
    where
        uniqueIdent =
            Binary.encode
            ( exprMStorePoint <&> fst
            , exprIRef
            )
            & SHA256.hashlazy
        (genParamIds, genRefs) = genFromHashable uniqueIdent & Random.split
