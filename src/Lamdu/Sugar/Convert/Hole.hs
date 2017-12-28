{-# LANGUAGE LambdaCase, NoImplicitPrelude, ConstraintKinds, OverloadedStrings, RankNTypes #-}
module Lamdu.Sugar.Convert.Hole
    ( convert

      -- Used by Convert.GetVar:
    , injectVar
      -- Used by Convert.Apply(wrapper hole):
    , convertCommon
    , mkHoleOption, mkHoleOptionFromInjected, addSuggestedOptions
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
import qualified Data.Set as Set
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.UUID as UUID
import           Data.UUID.Types (UUID)
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Builtins.PrimVal as PrimVal
import           Lamdu.Calc.Type (Type(..))
import qualified Lamdu.Calc.Type as T
import qualified Lamdu.Calc.Type.Nominal as N
import           Lamdu.Calc.Type.Scheme (schemeType)
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
import qualified Lamdu.Expr.Pure as Pure
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
import           System.Random (random)
import qualified System.Random as Random
import           System.Random.Utils (genFromHashable)
import           Text.PrettyPrint.HughesPJClass (prettyShow)

import           Lamdu.Prelude

type T = Transaction

type ExprStorePoint m a = Val (Maybe (ValI m), a)

convert :: Monad m => Input.Payload m a -> ConvertM m (ExpressionU m a)
convert exprPl =
    convertCommon Nothing exprPl
    <&> rPayload . plActions . delete .~ CannotDelete

convertCommon ::
    Monad m =>
    Maybe (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ExpressionU m a)
convertCommon mInjectedArg exprPl =
    mkHole mInjectedArg exprPl
    <&> BodyHole
    >>= addActions exprPl

mkHoleOptionFromInjected ::
    Monad m =>
    ConvertM.Context m ->
    Input.Payload m a -> ValIProperty m ->
    Val (Type, Maybe (Input.Payload m a)) ->
    HoleOption (T m) (Expression UUID (T m) ())
mkHoleOptionFromInjected sugarContext exprPl stored val =
    HoleOption
    { _hoVal = baseExpr
    , _hoSugaredBaseExpr = sugar sugarContext exprPl baseExpr
    , _hoResults =
        do
            (result, inferContext) <-
                mkHoleResultValInjected exprPl val
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
            return
                ( resultScore (result <&> fst)
                , mkHoleResult newSugarContext updateDeps stored result
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
    Monad m => ConvertM.Context m ->
    Maybe (Val (Input.Payload m a)) ->
    Input.Payload m a -> ValIProperty m ->
    BaseExpr -> HoleOption (T m) (Expression UUID (T m) ())
mkHoleOption sugarContext mInjectedArg exprPl stored val =
    HoleOption
    { _hoVal = v
    , _hoSugaredBaseExpr = sugar sugarContext exprPl v
    , _hoResults = mkHoleResults mInjectedArg sugarContext exprPl stored val
    }
    where
        v = getBaseExprVal val

mkHoleSuggesteds ::
    Monad m =>
    ConvertM.Context m -> Maybe (Val (Input.Payload m a)) ->
    Input.Payload m a -> ValIProperty m -> [HoleOption (T m) (Expression UUID (T m) ())]
mkHoleSuggesteds sugarContext mInjectedArg exprPl stored =
    exprPl ^. Input.inferred
    & Suggest.value
    <&> SuggestedExpr
    <&> mkHoleOption sugarContext mInjectedArg exprPl stored

addSuggestedOptions ::
    [HoleOption (T m) (Expression UUID (T m) ())] -> [HoleOption (T m) (Expression UUID (T m) ())] -> [HoleOption (T m) (Expression UUID (T m) ())]
addSuggestedOptions suggesteds options
    | null nonTrivial = options
    | otherwise = nonTrivial ++ filter (not . equivalentToSuggested) options
    where
        equivalentToSuggested x =
            any (Val.alphaEq (x ^. hoVal)) (nonTrivial ^.. Lens.traverse . hoVal)
        nonTrivial = filter (Lens.nullOf (hoVal . ExprLens.valHole)) suggesteds

isLiveGlobal :: Monad m => DefI m -> T m Bool
isLiveGlobal defI =
    Anchors.assocDefinitionState defI
    & Transaction.getP
    <&> (== LiveDefinition)

getNominals :: Monad m => ConvertM.Context m -> T m [(T.NominalId, N.Nominal)]
getNominals sugarContext =
    sugarContext ^. ConvertM.scCodeAnchors
    & Anchors.tids & Transaction.getP <&> Set.toList
    >>= traverse (\nomId -> (,) nomId <$> Load.nominal nomId)

getGlobals :: Monad m => ConvertM.Context m -> T m [DefI m]
getGlobals sugarContext =
    sugarContext ^. ConvertM.scCodeAnchors
    & Anchors.globals & Transaction.getP <&> Set.toList
    >>= filterM isLiveGlobal

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
        mkDirectWrappers tid ++ mkToNomInjections tid nominal
    where
        mkDirectWrappers tid =
            do
                f <- [V.BFromNom, V.BToNom]
                [ V.Nom tid P.hole & f & Val () ]
        mkToNomInjections tid nominal =
            do
                (tag, _typ) <-
                    nominal ^..
                    N.nomType . N._NominalType . schemeType . T._TSum .
                    ExprLens.compositeFields
                let inject = V.Inject tag P.hole & V.BInject & Val ()
                [ inject & V.Nom tid & V.BToNom & Val () ]

mkOptions ::
    Monad m =>
    Maybe (Val (Input.Payload m a)) -> Input.Payload m a -> ValIProperty m ->
    ConvertM m (T m [HoleOption (T m) (Expression UUID (T m) ())])
mkOptions mInjectedArg exprPl stored =
    ConvertM.readContext
    <&>
    \sugarContext ->
    do
        nominalOptions <- getNominals sugarContext <&> mkNominalOptions
        globals <- getGlobals sugarContext
        concat
            [ locals sugarContext exprPl
                & concatMap (getLocalScopeGetVars sugarContext)
            , globals <&> P.var . ExprIRef.globalId
            , nominalOptions
            , [ P.abs "NewLambda" P.hole ]
            , [ P.recEmpty ]
            , [ P.absurd ]
            ]
            <&> SeedExpr
            <&> mkHoleOption sugarContext mInjectedArg exprPl stored
            & addSuggestedOptions
              (mkHoleSuggesteds sugarContext mInjectedArg exprPl stored)
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
    T m (Expression UUID (T m) a)
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

mkLeafActions ::
    Monad m =>
    Maybe (Val (Input.Payload m a)) -> Input.Payload m a -> ValIProperty m ->
    ConvertM m (LeafHoleActions (T m) (Expression UUID (T m) ()))
mkLeafActions mInjectedArg exprPl stored =
    do
        sugarContext <- ConvertM.readContext
        let mkOption =
                pure . mkHoleOption sugarContext mInjectedArg exprPl stored . SeedExpr
        let mkLiteralOption =
                mkOption . Val () . V.BLeaf . V.LLiteral . PrimVal.fromKnown
        pure LeafHoleActions
            { _holeOptionLiteral =
                \case
                LiteralNum (Identity x) -> PrimVal.Float x & mkLiteralOption
                LiteralBytes (Identity x) -> PrimVal.Bytes x & mkLiteralOption
                LiteralText (Identity x) ->
                    encodeUtf8 x
                    & PrimVal.Bytes
                    & PrimVal.fromKnown
                    & V.LLiteral & Pure.leaf
                    & Pure.toNom Builtins.textTid
                    & mkOption
            }

mkHole ::
    Monad m =>
    Maybe (Val (Input.Payload m a)) ->
    Input.Payload m a ->
    ConvertM m (Hole (T m) (Expression UUID (T m) ()) (ExpressionU m a))
mkHole mInjectedArg exprPl =
    do
        options <- mkOptions mInjectedArg exprPl (exprPl ^. Input.stored)
        leafActions <- mkLeafActions mInjectedArg exprPl (exprPl ^. Input.stored)
        pure Hole
            { _holeOptions = options
            , _holeKind = LeafHole leafActions
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

data IsInjected = Injected | NotInjected
type HoleResultVal m a = Val (Infer.Payload, (Maybe (ValI m), a))

markNotInjected :: HoleResultVal n () -> HoleResultVal n IsInjected
markNotInjected val = val <&> _2 . _2 .~ NotInjected

-- TODO: Unify type according to IsInjected, avoid magic var
injectVar :: V.Var
injectVar = "HOLE INJECT EXPR"

replaceInjected :: Val (Input.Payload m IsInjected) -> Val (Input.Payload m ())
replaceInjected (Val pl body) =
    case pl ^. Input.userData of
    Injected -> V.LVar injectVar & V.BLeaf
    NotInjected -> body & traverse %~ replaceInjected
    & Val (void pl)

writeConvertTypeChecked ::
    Monad m =>
    ConvertM.Context m -> ValIProperty m ->
    HoleResultVal m IsInjected ->
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
        replaceInjected (writtenExpr <&> snd)
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
exceptToListT (Right x) = return x

applyForms ::
    Monad m =>
    a -> Val (Infer.Payload, a) ->
    StateT Infer.Context (ListT (T m)) (Val (Infer.Payload, a))
applyForms _ v@(Val _ V.BLam {}) = return v
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
        val & Val.payload . _1 . Infer.plType %%~ orderType
        <&> Suggest.fillHoles empty
        >>= Suggest.valueConversion Load.nominal empty
        <&> mapStateT ListClass.fromList
        & lift & lift & join
    where
        inferPl = val ^. Val.payload . _1

holeWrapIfNeeded ::
    Monad m =>
    Type -> Val (Infer.Payload, (Maybe a, IsInjected)) ->
    StateT Infer.Context m (Val (Infer.Payload, (Maybe a, IsInjected)))
holeWrapIfNeeded holeType val =
    do
        unifyResult <-
            unify holeType (val ^. Val.payload . _1 . Infer.plType) & liftInfer
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
        inferPl = val ^. Val.payload . _1
        func = Val (plSameScope funcType, emptyPl) $ V.BLeaf V.LHole
        funcType = TFun (inferPl ^. Infer.plType) resultType
        emptyPl = (Nothing, NotInjected)

replaceEachUnwrappedHole :: Applicative f => (a -> f (Val a)) -> Val a -> [f (Val a)]
replaceEachUnwrappedHole replaceHole =
    map fst . filter snd . (`runStateT` False) . go
    where
        go oldVal@(Val x body) =
            do
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
    markNotInjected val
    & replaceEachUnwrappedHole inject
    & ListClass.fromList
    & lift
    & join
    where
        inject pl =
            ListClass.fromList
            [ do
                unify injectedType (fst pl ^. Infer.plType)
                    & Infer.run
                    & mapStateT exceptToListT
                return injected
            , V.Apply
                (Val (fst pl & Infer.plType %~ (`T.TFun` injectedType), (Nothing, NotInjected)) (V.BLeaf V.LHole))
                injected
                & V.BApp & Val (fst pl, (Nothing, NotInjected))
                & return
            ]
            & lift
            & join
            & mapStateT (ListClass.take 1)
        injected = injectedArg <&> onInjectedPayload
        onInjectedPayload pl =
            ( pl ^. Input.inferred
            , (Just (pl ^. Input.stored . Property.pVal), Injected)
            )
        injectedType = injectedArg ^. Val.payload . Input.inferredType

mkHoleResultValInjected ::
    Monad m =>
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
                (inputPl ^. Input.stored & Property.value & Just, Injected)
            )

mkHoleResultVals ::
    Monad m =>
    Transaction.Property m Infer.Dependencies ->
    Maybe (Val (Input.Payload m a)) ->
    Input.Payload m dummy ->
    BaseExpr ->
    StateT Infer.Context (ListT (T m)) (Infer.Dependencies, HoleResultVal m IsInjected)
mkHoleResultVals frozenDeps mInjectedArg exprPl base =
    case base of
    SeedExpr seed ->
        do
            (seedDeps, inferResult) <-
                do
                    seedDeps <- loadTheNewDeps seed
                    inferResult <-
                        Infer.infer seedDeps scope seed & InferT.liftInfer
                        <&> Lens.traversed . _2 %~ (,) Nothing
                    return (seedDeps, inferResult)
                & mapStateT exceptTtoListT
            form <- applyForms (Nothing, ()) inferResult
            newDeps <- loadNewDeps seedDeps scope form & lift & lift
            return (newDeps, form)
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
            & maybe (return . markNotInjected) holeResultsInject mInjectedArg
            >>= holeWrapIfNeeded (inferred ^. Infer.plType)

mkHoleResult ::
    Monad m =>
    ConvertM.Context m -> Transaction m () ->
    ValIProperty m -> HoleResultVal m IsInjected ->
    T m (HoleResult (T m) (Expression UUID (T m) ()))
mkHoleResult sugarContext updateDeps stored val =
    do
        (fConverted, forkedChanges) <-
            Transaction.fork $
            do
                updateDeps
                writeConvertTypeChecked sugarContext stored val
        return
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
    Input.Payload m dummy -> ValIProperty m ->
    BaseExpr ->
    ListT (T m)
    ( HoleResultScore
    , T m (HoleResult (T m) (Expression UUID (T m) ()))
    )
mkHoleResults mInjectedArg sugarContext exprPl stored base =
    do
        ((newDeps, val), inferContext) <-
            mkHoleResultVals (sugarContext ^. ConvertM.scFrozenDeps)
            mInjectedArg exprPl base
            & (`runStateT` (sugarContext ^. ConvertM.scInferContext))
        let newSugarContext =
                sugarContext
                & ConvertM.scInferContext .~ inferContext
                & ConvertM.scFrozenDeps . Property.pVal .~ newDeps
        let updateDeps = newDeps & sugarContext ^. ConvertM.scFrozenDeps . Property.pSet
        return
            ( resultScore (val <&> fst)
            , mkHoleResult newSugarContext updateDeps stored val
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
