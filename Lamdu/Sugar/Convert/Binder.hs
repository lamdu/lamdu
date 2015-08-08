{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types, PatternGuards, RecordWildCards #-}
module Lamdu.Sugar.Convert.Binder
    ( convertBinder, convertLam
    ) where

import           Prelude.Compat

import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (guard, void, when, join)
import           Control.MonadA (MonadA)
import           Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.List.Utils as ListUtils
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Store.Guid (Guid)
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction, MkProperty)
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Builtins.Anchors as Builtins
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Eval.Val (ScopeId)
import qualified Lamdu.Eval.Val as EV
import qualified Lamdu.Expr.GenIds as GenIds
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.Convert.Expression.Actions (addActions, makeAnnotation)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.ParamList (ParamList)
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.OrderTags (orderedFlatComposite)
import           Lamdu.Sugar.Types

type T = Transaction

data ConventionalParams m a = ConventionalParams
    { cpTags :: Set T.Tag
    , cpParamInfos :: Map T.Tag ConvertM.TagParamInfo
    , _cpParams :: BinderParams Guid m
    , cpMAddFirstParam :: Maybe (T m ParamAddResult)
    , cpScopes :: Map ScopeId [ScopeId]
    , cpMLamParam :: Maybe V.Var
    }

cpParams :: Lens' (ConventionalParams m a) (BinderParams Guid m)
cpParams f ConventionalParams {..} = f _cpParams <&> \_cpParams -> ConventionalParams{..}

data FieldParam = FieldParam
    { fpTag :: T.Tag
    , fpFieldType :: Type
    , fpValue :: Map ScopeId [(ScopeId, EV.EvalResult ())]
    }

onMatchingSubexprs ::
    MonadA m => (a -> m ()) -> (a -> Val () -> Bool) -> Val a -> m ()
onMatchingSubexprs action predicate =
    Lens.itraverseOf_ (ExprLens.subExprPayloads . Lens.ifiltered (flip predicate))
    (const action)

onMatchingSubexprsWithPath ::
    MonadA m => (a -> m ()) -> (a -> [Val ()] -> Bool) -> Val a -> m ()
onMatchingSubexprsWithPath action predicate =
    Lens.itraverseOf_ (ExprLens.payloadsIndexedByPath . Lens.ifiltered (flip predicate))
    (const action)

toHole :: MonadA m => ExprIRef.ValIProperty m -> T m ()
toHole = void . DataOps.setToHole

isGetVarOf :: V.Var -> Val a -> Bool
isGetVarOf = Lens.anyOf ExprLens.valVar . (==)

data StoredLam m = StoredLam
    { _slLam :: V.Lam (Val (ExprIRef.ValIProperty m))
    , slLambdaProp :: ExprIRef.ValIProperty m
    }

slLam :: Lens' (StoredLam m) (V.Lam (Val (ExprIRef.ValIProperty m)))
slLam f StoredLam{..} = f _slLam <&> \_slLam -> StoredLam{..}

mkStoredLam ::
    V.Lam (Val (Input.Payload m a)) ->
    Input.Payload m a -> Maybe (StoredLam m)
mkStoredLam lam pl =
    StoredLam
    <$> (lam & (traverse . traverse) (^. Input.mStored))
    <*> pl ^. Input.mStored

changeRecursionsFromCalls :: MonadA m => V.Var -> Val (ExprIRef.ValIProperty m) -> T m ()
changeRecursionsFromCalls var =
    onMatchingSubexprs changeRecursion (const isCall)
    where
        isCall (Val _ (V.BApp (V.Apply f _))) = isGetVarOf var f
        isCall _ = False
        changeRecursion prop =
            do
                body <- ExprIRef.readValBody (Property.value prop)
                case body of
                    V.BApp (V.Apply f _) -> Property.set prop f
                    _ -> error "assertion: expected BApp"

makeDeleteLambda ::
    MonadA m => Maybe V.Var -> StoredLam m ->
    ConvertM m (T m ParamDelResult)
makeDeleteLambda mRecursiveVar (StoredLam (V.Lam paramVar lamBodyStored) lambdaProp) =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        return $
            do
                getVarsToHole paramVar lamBodyStored
                mRecursiveVar
                    & Lens._Just %%~ (`changeRecursionsFromCalls` lamBodyStored)
                    & void
                let lamBodyI = Property.value (lamBodyStored ^. V.payload)
                _ <- protectedSetToVal lambdaProp lamBodyI
                return ParamDelResultDelVar

newTag :: MonadA m => T m T.Tag
newTag = GenIds.transaction GenIds.randomTag

isRecursiveCallArg :: V.Var -> [Val ()] -> Bool
isRecursiveCallArg recursiveVar (cur : parent : _) =
    Lens.allOf ExprLens.valVar (/= recursiveVar) cur &&
    Lens.anyOf (ExprLens.valApply . V.applyFunc . ExprLens.valVar)
    (== recursiveVar) parent
isRecursiveCallArg _ _ = False

-- TODO: find nicer way to do it than using properties and rereading
-- data?  Perhaps keep track of the up-to-date pure val as it is being
-- mutated?
rereadVal :: MonadA m => ExprIRef.ValIProperty m -> T m (Val (ExprIRef.ValIProperty m))
rereadVal valProp =
    ExprIRef.readVal (Property.value valProp)
    <&> fmap (flip (,) ())
    <&> ExprIRef.addProperties (Property.set valProp)
    <&> fmap fst

changeRecursiveCallArgs ::
    MonadA m =>
    (ExprIRef.ValI m -> T m (ExprIRef.ValI m)) ->
    ExprIRef.ValIProperty m -> V.Var -> T m ()
changeRecursiveCallArgs change valProp var =
    -- Reread body before fixing it,
    -- to avoid re-writing old data (without converted vars)
    rereadVal valProp
    >>= onMatchingSubexprsWithPath changeRecurseArg (const (isRecursiveCallArg var))
    where
        changeRecurseArg prop =
            Property.value prop
            & change
            >>= Property.set prop

wrapArgWithRecord :: MonadA m => T.Tag -> T.Tag -> ExprIRef.ValI m -> T m (ExprIRef.ValI m)
wrapArgWithRecord tagForExistingArg tagForNewArg oldArg =
    do
        hole <- DataOps.newHole
        ExprIRef.newValBody (V.BLeaf V.LRecEmpty)
            >>= ExprIRef.newValBody . V.BRecExtend . V.RecExtend tagForNewArg hole
            >>= ExprIRef.newValBody . V.BRecExtend . V.RecExtend tagForExistingArg oldArg

convertVarToGetField ::
    MonadA m => T.Tag -> V.Var -> Val (Property (T m) (ExprIRef.ValI m)) -> T m ()
convertVarToGetField tagForVar paramVar =
    onMatchingSubexprs (convertVar . Property.value) (const (isGetVarOf paramVar))
    where
        convertVar bodyI =
            ExprIRef.newValBody (V.BLeaf (V.LVar paramVar))
            <&> (`V.GetField` tagForVar) <&> V.BGetField
            >>= ExprIRef.writeValBody bodyI

data NewParamPosition = NewParamBefore | NewParamAfter

makeConvertToRecordParams ::
    MonadA m => Maybe V.Var -> StoredLam m ->
    ConvertM m (NewParamPosition -> T m ParamAddResult)
makeConvertToRecordParams mRecursiveVar (StoredLam (V.Lam paramVar lamBody) lamProp) =
    do
        wrapOnError <- ConvertM.wrapOnTypeError
        return $ \newParamPosition ->
            do
                tagForVar <- newTag
                tagForNewVar <- newTag
                setParamList paramList $ case newParamPosition of
                    NewParamBefore -> [tagForNewVar, tagForVar]
                    NewParamAfter -> [tagForVar, tagForNewVar]
                convertVarToGetField tagForVar paramVar lamBody
                mRecursiveVar
                    & traverse_
                        (changeRecursiveCallArgs
                          (wrapArgWithRecord tagForVar tagForNewVar)
                          (lamBody ^. V.payload))
                _ <- wrapOnError lamProp
                return $ ParamAddResultVarToTags VarToTags
                    { vttReplacedVar = paramVar
                    , vttReplacedVarEntityId = EntityId.ofLambdaParam paramVar
                    , vttReplacedByTag = tagGForLambdaTagParam paramVar tagForVar
                    , vttNewTag = tagGForLambdaTagParam paramVar tagForNewVar
                    }
    where
        paramList = Anchors.assocFieldParamList (Property.value lamProp)

tagGForLambdaTagParam :: V.Var -> T.Tag -> TagG ()
tagGForLambdaTagParam paramVar tag = TagG (EntityId.ofLambdaTagParam paramVar tag) tag ()

convertRecordParams ::
    (MonadA m, Monoid a) =>
    Maybe V.Var -> [FieldParam] ->
    V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ConventionalParams m a)
convertRecordParams mRecursiveVar fieldParams lam@(V.Lam param _) pl =
    do
        params <- traverse mkParam fieldParams
        mAddFirstParam <-
            mStoredLam
            & Lens.traverse %%~ makeAddFieldParam mRecursiveVar param (:tags)
        pure ConventionalParams
            { cpTags = Set.fromList tags
            , cpParamInfos = mconcat $ mkParamInfo <$> fieldParams
            , _cpParams = FieldParams params
            , cpMAddFirstParam = mAddFirstParam
            , cpScopes = pl ^. Input.evalAppliesOfLam <&> map fst
            , cpMLamParam = Just param
            }
    where
        tags = fpTag <$> fieldParams
        fpIdEntityId = EntityId.ofLambdaTagParam param . fpTag
        mkParamInfo fp =
            Map.singleton (fpTag fp) . ConvertM.TagParamInfo param $ fpIdEntityId fp
        mStoredLam = mkStoredLam lam pl
        mkParam fp =
            do
                actions <-
                    mStoredLam
                    & Lens.traverse %%~ makeFieldParamActions mRecursiveVar param tags fp
                pure
                    ( fpTag fp
                    , FuncParam
                        { _fpInfo =
                          NamedParamInfo
                          { _npiName = UniqueId.toGuid $ fpTag fp
                          , _npiMActions = actions
                          }
                        , _fpId = fpIdEntityId fp
                        , _fpAnnotation =
                            Annotation
                            { _aInferredType = fpFieldType fp
                            , _aMEvaluationResult =
                                do
                                    fpValue fp & Map.null & not & guard
                                    fpValue fp ^.. Lens.traversed . Lens.traversed
                                        & Map.fromList & Just
                            }
                        , _fpHiddenIds = []
                        }
                    )

setParamList :: MonadA m => MkProperty m (Maybe [T.Tag]) -> [T.Tag] -> T m ()
setParamList paramListProp newParamList =
    do
        zip newParamList [0..] & mapM_ (uncurry setParamOrder)
        Just newParamList & Transaction.setP paramListProp
    where
        setParamOrder = Transaction.setP . Anchors.assocTagOrder

makeAddFieldParam ::
    MonadA m =>
    Maybe V.Var -> V.Var -> (T.Tag -> ParamList) -> StoredLam m ->
    ConvertM m (T m ParamAddResult)
makeAddFieldParam mRecursiveVar param mkNewTags storedLam =
    do
        wrapOnError <- ConvertM.wrapOnTypeError
        return $
            do
                tag <- newTag
                mkNewTags tag & setParamList (slParamList storedLam)
                let addFieldToCall argI =
                        do
                            hole <- DataOps.newHole
                            ExprIRef.newValBody $ V.BRecExtend $ V.RecExtend tag hole argI
                mRecursiveVar
                    & Lens.traverse %%~
                        changeRecursiveCallArgs addFieldToCall
                        (storedLam ^. slLam . V.lamResult . V.payload)
                    & void
                void $ wrapOnError $ slLambdaProp storedLam
                return $
                    ParamAddResultNewTag $
                    TagG (EntityId.ofLambdaTagParam param tag) tag ()

makeFieldParamActions ::
    MonadA m =>
    Maybe V.Var -> V.Var -> [T.Tag] -> FieldParam -> StoredLam m ->
    ConvertM m (FuncParamActions m)
makeFieldParamActions mRecursiveVar param tags fp storedLam =
    do
        addParam <- makeAddFieldParam mRecursiveVar param mkNewTags storedLam
        delParam <- makeDelFieldParam mRecursiveVar tags fp storedLam
        pure FuncParamActions
            { _fpAddNext = addParam
            , _fpDelete = delParam
            }
    where
        mkNewTags tag =
            break (== fpTag fp) tags & \(pre, x:post) -> pre ++ [x, tag] ++ post

fixRecursiveCallRemoveField ::
    MonadA m =>
    T.Tag -> ExprIRef.ValI m -> T m (ExprIRef.ValI m)
fixRecursiveCallRemoveField tag argI =
    do
        body <- ExprIRef.readValBody argI
        case body of
            V.BRecExtend (V.RecExtend t v restI)
                | t == tag -> return restI
                | otherwise ->
                    do
                        newRestI <- fixRecursiveCallRemoveField tag restI
                        when (newRestI /= restI) $
                            ExprIRef.writeValBody argI $
                            V.BRecExtend $ V.RecExtend t v newRestI
                        return argI
            _ -> return argI

fixRecursiveCallToSingleArg ::
    MonadA m => T.Tag -> ExprIRef.ValI m -> T m (ExprIRef.ValI m)
fixRecursiveCallToSingleArg tag argI =
    do
        body <- ExprIRef.readValBody argI
        case body of
            V.BRecExtend (V.RecExtend t v restI)
                | t == tag -> return v
                | otherwise -> fixRecursiveCallToSingleArg tag restI
            _ -> return argI

getFieldParamsToParams :: MonadA m => StoredLam m -> T.Tag -> T m ()
getFieldParamsToParams (StoredLam (V.Lam param lamBody) _) tag =
    onMatchingSubexprs (toParam . Property.value)
    (const (isGetFieldParam param tag)) lamBody
    where
        toParam bodyI = ExprIRef.writeValBody bodyI $ V.BLeaf $ V.LVar param

makeDelFieldParam ::
    MonadA m =>
    Maybe V.Var -> [T.Tag] -> FieldParam -> StoredLam m ->
    ConvertM m (T m ParamDelResult)
makeDelFieldParam mRecursiveVar tags fp storedLam =
    do
        wrapOnError <- ConvertM.wrapOnTypeError
        return $
            do
                Transaction.setP (slParamList storedLam) newTags
                getFieldParamsToHole tag storedLam
                mLastTag
                    & traverse_ (getFieldParamsToParams storedLam)
                mRecursiveVar
                    & traverse_
                        (changeRecursiveCallArgs fixRecurseArg
                          (storedLam ^. slLam . V.lamResult . V.payload))
                _ <- wrapOnError $ slLambdaProp storedLam
                return delResult
    where
        paramVar = storedLam ^. slLam . V.lamParamId
        tag = fpTag fp
        fixRecurseArg =
            maybe (fixRecursiveCallRemoveField tag)
            fixRecursiveCallToSingleArg mLastTag
        (newTags, mLastTag, delResult) =
            case List.delete tag tags of
            [x] ->
                ( Nothing
                , Just x
                , ParamDelResultTagsToVar TagsToVar
                    { ttvReplacedTag = tagGForLambdaTagParam paramVar x
                    , ttvReplacedByVar = paramVar
                    , ttvReplacedByVarEntityId = EntityId.ofLambdaParam paramVar
                    , ttvDeletedTag = tagGForLambdaTagParam paramVar tag
                    }
                )
            xs -> (Just xs, Nothing, ParamDelResultDelTag)

slParamList :: MonadA m => StoredLam m -> Transaction.MkProperty m (Maybe ParamList)
slParamList = Anchors.assocFieldParamList . Property.value . slLambdaProp

makeNonRecordParamActions ::
    MonadA m => Maybe V.Var -> StoredLam m ->
    ConvertM m (FuncParamActions m, T m ParamAddResult)
makeNonRecordParamActions mRecursiveVar storedLam =
    do
        delete <- makeDeleteLambda mRecursiveVar storedLam
        convertToRecordParams <- makeConvertToRecordParams mRecursiveVar storedLam
        return
            ( FuncParamActions
                { _fpAddNext = convertToRecordParams NewParamAfter
                , _fpDelete = delete
                }
            , convertToRecordParams NewParamBefore
            )

lamParamType :: Input.Payload m a -> Type
lamParamType lamExprPl =
    fromMaybe (error "Lambda value not inferred to a function type?!") $
    lamExprPl ^? Input.inferred . Infer.plType . ExprLens._TFun . _1

mkFuncParam :: EntityId -> Input.Payload m a -> info -> FuncParam info
mkFuncParam paramEntityId lamExprPl info =
    FuncParam
    { _fpInfo = info
    , _fpId = paramEntityId
    , _fpAnnotation =
        Annotation
        { _aInferredType = lamParamType lamExprPl
        , _aMEvaluationResult =
            do
                lamExprPl ^. Input.evalAppliesOfLam
                    & Map.null & not & guard
                lamExprPl ^.. Input.evalAppliesOfLam .
                    Lens.traversed . Lens.traversed
                    & Map.fromList & Just
        }
    , _fpHiddenIds = []
    }

convertNonRecordParam ::
    MonadA m => Maybe V.Var ->
    V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ConventionalParams m a)
convertNonRecordParam mRecursiveVar lam@(V.Lam param _) lamExprPl =
    do
        mActions <- mStoredLam & Lens._Just %%~ makeNonRecordParamActions mRecursiveVar
        let funcParam =
                case lamParamType lamExprPl of
                T.TRecord T.CEmpty
                    | isParamUnused lam ->
                      mActions
                      <&> (^. _1 . fpDelete)
                      <&> void
                      <&> NullParamActions
                      & NullParamInfo
                      & mkFuncParam paramEntityId lamExprPl & NullParam
                _ ->
                    NamedParamInfo
                    { _npiName = UniqueId.toGuid param
                    , _npiMActions = mActions <&> fst
                    } & mkFuncParam paramEntityId lamExprPl & VarParam
        pure ConventionalParams
            { cpTags = mempty
            , cpParamInfos = Map.empty
            , _cpParams = funcParam
            , cpMAddFirstParam = mActions <&> snd
            , cpScopes = lamExprPl ^. Input.evalAppliesOfLam <&> map fst
            , cpMLamParam = Just param
            }
    where
        mStoredLam = mkStoredLam lam lamExprPl
        paramEntityId = EntityId.ofLambdaParam param

isParamAlwaysUsedWithGetField :: V.Lam (Val a) -> Bool
isParamAlwaysUsedWithGetField (V.Lam param body) =
    Lens.nullOf (ExprLens.payloadsIndexedByPath . Lens.ifiltered cond) body
    where
        cond (_ : Val () V.BGetField{} : _) _ = False
        cond (Val () (V.BLeaf (V.LVar v)) : _) _ = v == param
        cond _ _ = False

-- TODO: move to eval vals
extractField :: T.Tag -> EV.EvalResult pl -> EV.EvalResult pl
extractField _ (Left err) = Left err
extractField tag (Right (EV.HRecExtend (V.RecExtend vt vv vr)))
    | vt == tag = vv
    | otherwise = extractField tag vr
extractField tag (Right x) =
    "Expected record with tag: " ++ show tag ++ " got: " ++ show (void x)
    & EV.EvalTypeError & Left

isParamUnused :: V.Lam (Val a) -> Bool
isParamUnused (V.Lam var body) =
    Lens.allOf (ExprLens.valLeafs . ExprLens._LVar) (/= var) body

convertLamParams ::
    (MonadA m, Monoid a) =>
    Maybe V.Var ->
    V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
    ConvertM m (ConventionalParams m a)
convertLamParams mRecursiveVar lambda lambdaPl =
    do
        tagsInOuterScope <- ConvertM.readContext <&> Map.keysSet . (^. ConvertM.scTagParamInfos)
        case lambdaPl ^. Input.inferred . Infer.plType of
            T.TFun (T.TRecord composite) _
                | Nothing <- extension
                , ListUtils.isLengthAtLeast 2 fields
                , Set.null (tagsInOuterScope `Set.intersection` tagsInInnerScope)
                , isParamAlwaysUsedWithGetField lambda ->
                    convertRecordParams mRecursiveVar fieldParams lambda lambdaPl
                where
                    tagsInInnerScope = Set.fromList $ fst <$> fields
                    (fields, extension) = orderedFlatComposite composite
                    fieldParams = map makeFieldParam fields
            _ -> convertNonRecordParam mRecursiveVar lambda lambdaPl
    where
        makeFieldParam (tag, typeExpr) =
            FieldParam
            { fpTag = tag
            , fpFieldType = typeExpr
            , fpValue =
                    lambdaPl ^. Input.evalAppliesOfLam
                    <&> Lens.mapped . Lens._2 %~ extractField tag
            }

convertEmptyParams :: MonadA m =>
    Maybe V.Var -> Val (Input.Payload m a) -> ConvertM m (ConventionalParams m a)
convertEmptyParams mRecursiveVar val =
    do
        protectedSetToVal <- ConvertM.typeProtectedSetToVal
        let makeAddFirstParam storedVal =
                do
                    (newParam, dst) <- DataOps.lambdaWrap (storedVal ^. V.payload)
                    case mRecursiveVar of
                        Nothing -> return ()
                        Just recursiveVar -> changeRecursionsToCalls recursiveVar storedVal
                    void $ protectedSetToVal (storedVal ^. V.payload) dst
                    return $
                        ParamAddResultNewVar (EntityId.ofLambdaParam newParam) newParam

        pure
            ConventionalParams
            { cpTags = mempty
            , cpParamInfos = Map.empty
            , _cpParams = DefintionWithoutParams
            , cpMAddFirstParam =
                val
                <&> (^. Input.mStored)
                & sequenceA
                & Lens._Just %~ makeAddFirstParam
            , cpScopes =
                -- Collect scopes from all evaluated subexpressions.
                val ^..
                    ExprLens.subExprPayloads . Input.evalResults .
                    Lens.to Map.keys . Lens.traversed
                <&> join (,) & Map.fromList <&> (:[])
            , cpMLamParam = Nothing
            }

convertParams ::
    (MonadA m, Monoid a) =>
    Maybe V.Var -> Val (Input.Payload m a) ->
    ConvertM m
    ( ConventionalParams m a
    , Val (Input.Payload m a)
    )
convertParams mRecursiveVar expr =
    case expr ^. V.body of
    V.BAbs lambda ->
        do
            params <-
                convertLamParams mRecursiveVar lambda (expr ^. V.payload)
                -- The lambda disappears here, so add its id to the first
                -- param's hidden ids:
                <&> cpParams . _VarParam . fpHiddenIds <>~ hiddenIds
                <&> cpParams . _FieldParams . Lens.ix 0 . _2 . fpHiddenIds <>~ hiddenIds
            return (params, lambda ^. V.lamResult)
        where
              hiddenIds = [expr ^. V.payload . Input.entityId]
    _ ->
        do
            params <- convertEmptyParams mRecursiveVar expr
            return (params, expr)

changeRecursionsToCalls :: MonadA m => V.Var -> Val (ExprIRef.ValIProperty m) -> T m ()
changeRecursionsToCalls =
    onMatchingSubexprs changeRecursion . const . isGetVarOf
    where
        changeRecursion prop =
            DataOps.newHole
            >>= ExprIRef.newValBody . V.BApp . V.Apply (Property.value prop)
            >>= Property.set prop

data ExprLetItem a = ExprLetItem
    { ewiBody :: Val a
    , ewiBodyScopesMap :: Map ScopeId ScopeId
    , ewiParam :: V.Var
    , ewiArg :: Val a
    , ewiHiddenPayloads :: [a]
    , ewiAnnotation :: Annotation
    }

mExtractLet :: Val (Input.Payload m a) -> Maybe (ExprLetItem (Input.Payload m a))
mExtractLet expr = do
    V.Apply func arg <- expr ^? ExprLens.valApply
    V.Lam param body <- func ^? V.body . ExprLens._BAbs
    Just ExprLetItem
        { ewiBody = body
        , ewiBodyScopesMap =
            func ^. V.payload . Input.evalAppliesOfLam
            <&> extractRedexApplies
        , ewiParam = param
        , ewiArg = arg
        , ewiHiddenPayloads = (^. V.payload) <$> [expr, func]
        , ewiAnnotation = makeAnnotation (arg ^. V.payload)
        }
    where
        extractRedexApplies [(scopeId, _)] = scopeId
        extractRedexApplies _ =
            error "redex should only be applied once per parent scope"

onGetVars ::
    MonadA m =>
    (ExprIRef.ValIProperty m -> T m ()) -> V.Var ->
    Val (ExprIRef.ValIProperty m) -> T m ()
onGetVars f = onMatchingSubexprs f . const . isGetVarOf

getVarsToHole :: MonadA m => V.Var -> Val (ExprIRef.ValIProperty m) -> T m ()
getVarsToHole = onGetVars toHole

isGetFieldParam :: V.Var -> T.Tag -> Val t -> Bool
isGetFieldParam param tag
    (Val _ (V.BGetField (V.GetField (Val _ (V.BLeaf (V.LVar v))) t)))
    = t == tag && v == param
isGetFieldParam _ _ _ = False

getFieldParamsToHole :: MonadA m => T.Tag -> StoredLam m -> T m ()
getFieldParamsToHole tag (StoredLam (V.Lam param lamBody) _) =
    onMatchingSubexprs toHole (const (isGetFieldParam param tag)) lamBody

mkExtract ::
    MonadA m =>
    [V.Var] -> V.Var -> Transaction m () ->
    Val (ExprIRef.ValIProperty m) -> Val (ExprIRef.ValIProperty m) ->
    ConvertM m (Transaction m EntityId)
mkExtract binderScopeVars param delItem bodyStored argStored =
    do
        ctx <- ConvertM.readContext
        do
            mapM_ (`getVarsToHole` argStored) binderScopeVars
            delItem
            case ctx ^. ConvertM.scMBodyStored of
                Nothing ->
                    do
                        getVarsToHole param bodyStored
                        getVarsToHole Builtins.recurseVar argStored
                        DataOps.newDefinitionWithPane
                            (ctx ^. ConvertM.scCodeAnchors) extractedI
                            <&> EntityId.ofIRef
                Just scopeBodyP ->
                    DataOps.redexWrapWithGivenParam param extractedI scopeBodyP
                    & Lens.mapped .~ EntityId.ofLambdaParam param
            & return
    where
        extractedI = argStored ^. V.payload & Property.value

mkWIActions ::
    MonadA m =>
    [V.Var] -> V.Var -> ExprIRef.ValIProperty m ->
    Val (ExprIRef.ValIProperty m) -> Val (ExprIRef.ValIProperty m) ->
    ConvertM m (LetItemActions m)
mkWIActions binderScopeVars param topLevelProp bodyStored argStored =
    do
        extr <- mkExtract binderScopeVars param del bodyStored argStored
        return
            LetItemActions
            { _liDelete = getVarsToHole param bodyStored >> del
            , _liAddNext =
                DataOps.redexWrap topLevelProp <&> EntityId.ofLambdaParam . fst
            , _liExtract = extr
            }
    where
        del = bodyStored ^. V.payload & replaceWith topLevelProp & void

convertLetItems ::
    (MonadA m, Monoid a) =>
    [V.Var] -> Val (Input.Payload m a) ->
    ConvertM m
    ( [LetItem Guid m (ExpressionU m a)]
    , Val (Input.Payload m a)
    , Map ScopeId ScopeId
    )
convertLetItems binderScopeVars expr =
    case mExtractLet expr of
    Nothing -> return ([], expr, Map.empty)
    Just ewi ->
        do
            value <- convertBinder Nothing defGuid (ewiArg ewi)
            actions <-
                mkWIActions binderScopeVars param
                <$> expr ^. V.payload . Input.mStored
                <*> traverse (^. Input.mStored) (ewiBody ewi)
                <*> traverse (^. Input.mStored) (ewiArg ewi)
                & Lens.sequenceOf Lens._Just
            (items, body, bodyScopesMap) <-
                ewiBody ewi & convertLetItems (ewiParam ewi : binderScopeVars)
            return
                ( LetItem
                    { _liEntityId = defEntityId
                    , _liValue =
                        value
                        & bBody . rPayload . plData <>~
                        ewiHiddenPayloads ewi ^. Lens.traversed . Input.userData
                    , _liActions = actions
                    , _liName = UniqueId.toGuid param
                    , _liAnnotation = ewiAnnotation ewi
                    , _liScopes =
                        ewiBodyScopesMap ewi
                        & Map.keys & map (join (,)) & Map.fromList
                    }
                    :
                    ( items
                        <&> liScopes %~ appendScopeMaps (ewiBodyScopesMap ewi)
                    )
                , body
                , appendScopeMaps (ewiBodyScopesMap ewi) bodyScopesMap
                )
        where
            param = ewiParam ewi
            defGuid = UniqueId.toGuid param
            defEntityId = EntityId.ofLambdaParam param
            appendScopeMaps x y = x <&> overrideId y

overrideId :: Ord a => Map a a -> a -> a
overrideId mapping k = Map.lookup k mapping & fromMaybe k

makeBinder :: (MonadA m, Monoid a) =>
    Maybe (MkProperty m (Maybe ScopeId)) ->
    Maybe (MkProperty m PresentationMode) ->
    ConventionalParams m a -> Val (Input.Payload m a) ->
    ConvertM m (Binder Guid m (ExpressionU m a))
makeBinder mChosenScopeProp mPresentationModeProp convParams funcBody =
    do
        (letItems, letBody, bodyScopesMap) <-
            convertLetItems (cpMLamParam convParams ^.. Lens._Just) funcBody
        bodyS <-
            ConvertM.convertSubexpression letBody
            & ConvertM.local
                ( ConvertM.scMBodyStored .~
                    letBody ^. V.payload . Input.mStored
                )
        let binderScopes s = (s, overrideId bodyScopesMap s)
        return Binder
            { _bParams = convParams ^. cpParams
            , _bMPresentationModeProp = mPresentationModeProp
            , _bMChosenScopeProp = mChosenScopeProp
            , _bBody = bodyS
            , _bScopes = cpScopes convParams <&> map binderScopes
            , _bLetItems = letItems
            , _bMActions =
                mkActions
                <$> cpMAddFirstParam convParams
                <*> letBody ^. V.payload . Input.mStored
            }
    & ConvertM.local addParams
    where
        addParams ctx =
            ctx
            & ConvertM.scTagParamInfos <>~ cpParamInfos convParams
            & ConvertM.scNullParams <>~
            case convParams ^. cpParams of
            NullParam {} -> Set.fromList (cpMLamParam convParams ^.. Lens._Just)
            _ -> Set.empty
        mkActions addFirstParam letStored =
            BinderActions
            { _baAddFirstParam = addFirstParam
            , _baAddInnermostLetItem =
                    EntityId.ofLambdaParam . fst <$> DataOps.redexWrap letStored
            }

convertLam ::
    (MonadA m, Monoid a) =>
    V.Lam (Val (Input.Payload m a)) ->
    Input.Payload m a -> ConvertM m (ExpressionU m a)
convertLam lam@(V.Lam _ lamBody) exprPl =
    do
        mDeleteLam <- mkStoredLam lam exprPl & Lens._Just %%~ makeDeleteLambda Nothing
        convParams <- convertLamParams Nothing lam exprPl
        binder <-
            makeBinder
            (exprPl ^. Input.mStored <&> Anchors.assocScopeRef . Property.value)
            Nothing convParams (lam ^. V.lamResult)
        let setToInnerExprAction =
                maybe NoInnerExpr SetToInnerExpr $
                do
                    guard $ Lens.nullOf ExprLens.valHole lamBody
                    mDeleteLam
                        <&> Lens.mapped .~ binder ^. bBody . rPayload . plEntityId
        BodyLam binder
            & addActions exprPl
            <&> rPayload . plActions . Lens._Just . setToInnerExpr .~ setToInnerExprAction

-- Let-item or definition (form of <name> [params] = <body>)
convertBinder ::
    (MonadA m, Monoid a) =>
    Maybe V.Var -> Guid ->
    Val (Input.Payload m a) -> ConvertM m (Binder Guid m (ExpressionU m a))
convertBinder mRecursiveVar defGuid expr =
    do
        (convParams, funcBody) <- convertParams mRecursiveVar expr
        let mPresentationModeProp
                | Lens.has (cpParams . _FieldParams) convParams =
                    Just $ Anchors.assocPresentationMode defGuid
                | otherwise = Nothing
        makeBinder (Just (Anchors.assocScopeRef defGuid)) mPresentationModeProp
            convParams funcBody
