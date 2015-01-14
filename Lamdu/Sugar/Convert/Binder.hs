{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types, PatternGuards, RecordWildCards #-}
module Lamdu.Sugar.Convert.Binder
  ( convertBinder, convertLam
  ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (guard, void, when)
import           Control.MonadA (MonadA)
import           Data.Foldable (traverse_)
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Monoid(..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Store.Guid (Guid)
import           Data.Store.Property (Property)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction, MkProperty)
import qualified Data.Store.Transaction as Transaction
import           Data.Traversable (traverse, sequenceA)
import           Lamdu.Data.Anchors (assocTagOrder)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import           Lamdu.Expr.FlatComposite (FlatComposite(..))
import qualified Lamdu.Expr.FlatComposite as FlatComposite
import qualified Lamdu.Expr.GenIds as GenIds
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Type (Type)
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Convert.ParamList (ParamList)
import qualified Lamdu.Sugar.Convert.ParamList as ParamList
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types

type T = Transaction

data ConventionalParams m a = ConventionalParams
  { cpTags :: Set T.Tag
  , cpParamInfos :: Map T.Tag ConvertM.TagParamInfo
  , _cpParams :: BinderParams Guid m
  , cpMAddFirstParam :: Maybe (T m ParamAddResult)
  }

cpParams :: Lens' (ConventionalParams m a) (BinderParams Guid m)
cpParams f ConventionalParams {..} = f _cpParams <&> \_cpParams -> ConventionalParams{..}

data FieldParam = FieldParam
  { fpTag :: T.Tag
  , fpFieldType :: Type
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
        getParamsToHole paramVar lamBodyStored
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
  onMatchingSubexprs convertVar (const (isGetVarOf paramVar))
  where
    convertVar prop =
      Property.value prop & (`V.GetField` tagForVar) & V.BGetField
      & ExprIRef.newValBody
      >>= Property.set prop

makeConvertToRecordParams ::
  MonadA m => Maybe V.Var -> StoredLam m -> ConvertM m (T m ParamAddResult)
makeConvertToRecordParams mRecursiveVar (StoredLam (V.Lam paramVar lamBody) lamProp) =
  do
    wrapOnError <- ConvertM.wrapOnTypeError
    return $
      do
        tagForVar <- newTag
        tagForNewVar <- newTag
        setParamList paramList [tagForVar, tagForNewVar]
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
    paramList = ParamList.mkProp (Property.value lamProp)

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
        pure FuncParam
          { _fpName = UniqueId.toGuid $ fpTag fp
          , _fpId = fpIdEntityId fp
          , _fpVarInfo = fpTag fp
          , _fpInferredType = fpFieldType fp
          , _fpMActions = actions
          , _fpHiddenIds = []
          }

setParamList :: MonadA m => MkProperty m (Maybe [T.Tag]) -> [T.Tag] -> T m ()
setParamList paramListProp newParamList =
  do
    zip newParamList [0..] & mapM_ (uncurry setParamOrder)
    Just newParamList & Transaction.setP paramListProp
  where
    setParamOrder = Transaction.setP . assocTagOrder

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
        let
          addFieldToCall argI =
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
    mkNewTags tag=
      break (== fpTag fp) tags & \(pre, post) -> pre ++ [tag] ++ post

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
  onMatchingSubexprs toParam (const (isGetFieldParam param tag)) lamBody
  where
    toParam prop =
      do
        body <- ExprIRef.readValBody (Property.value prop)
        case body of
          V.BGetField (V.GetField v _) -> Property.set prop v
          _ -> error "assertion: this was a get field"

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
slParamList = ParamList.mkProp . Property.value . slLambdaProp

makeNonRecordParamActions ::
  MonadA m => Maybe V.Var -> StoredLam m ->
  ConvertM m (FuncParamActions m, T m ParamAddResult)
makeNonRecordParamActions mRecursiveVar storedLam =
  do
    delete <- makeDeleteLambda mRecursiveVar storedLam
    convertToRecordParams <- makeConvertToRecordParams mRecursiveVar storedLam
    return
      ( FuncParamActions
        { _fpAddNext = convertToRecordParams
        , _fpDelete = delete
        }
      , convertToRecordParams
      )

convertNonRecordParam :: MonadA m =>
  Maybe V.Var ->
  V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
  ConvertM m (ConventionalParams m a)
convertNonRecordParam mRecursiveVar lam@(V.Lam param _) lamExprPl =
  do
    mActions <- mStoredLam & Lens._Just %%~ makeNonRecordParamActions mRecursiveVar
    let
      funcParam =
        FuncParam
        { _fpName = UniqueId.toGuid param
        , _fpVarInfo = ()
        , _fpId = paramEntityId
        , _fpInferredType = paramType
        , _fpMActions = fst <$> mActions
        , _fpHiddenIds = []
        }
    pure ConventionalParams
      { cpTags = mempty
      , cpParamInfos = Map.empty
      , _cpParams = VarParam funcParam
      , cpMAddFirstParam = snd <$> mActions
      }
  where
    mStoredLam = mkStoredLam lam lamExprPl
    paramEntityId = EntityId.ofLambdaParam param
    paramType =
      fromMaybe (error "Lambda value not inferred to a function type?!") $
      lamExprPl ^? Input.inferred . Infer.plType . ExprLens._TFun . _1

isParamAlwaysUsedWithGetField :: V.Lam (Val a) -> Bool
isParamAlwaysUsedWithGetField (V.Lam param body) =
  Lens.nullOf (ExprLens.payloadsIndexedByPath . Lens.ifiltered cond) body
  where
    cond (_ : Val () V.BGetField{} : _) _ = False
    cond (Val () (V.BLeaf (V.LVar v)) : _) _ = v == param
    cond _ _ = False

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
        , Map.size fields >= 2
        , Set.null (tagsInOuterScope `Set.intersection` tagsInInnerScope)
        , isParamAlwaysUsedWithGetField lambda ->
          convertRecordParams mRecursiveVar fieldParams lambda lambdaPl
        where
          tagsInInnerScope = Map.keysSet fields
          FlatComposite fields extension = FlatComposite.fromComposite composite
          fieldParams = map makeFieldParam $ Map.toList fields
      _ -> convertNonRecordParam mRecursiveVar lambda lambdaPl
  where
    makeFieldParam (tag, typeExpr) =
      FieldParam
      { fpTag = tag
      , fpFieldType = typeExpr
      }

convertEmptyParams :: MonadA m =>
  Maybe V.Var -> Val (Maybe (ExprIRef.ValIProperty m)) -> ConvertM m (ConventionalParams m a)
convertEmptyParams mRecursiveVar val =
  do
    protectedSetToVal <- ConvertM.typeProtectedSetToVal
    let
      makeAddFirstParam storedVal =
        do  (newParam, dst) <- DataOps.lambdaWrap (storedVal ^. V.payload)
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
      , _cpParams = NoParams
      , cpMAddFirstParam =
        val
        & sequenceA
        & Lens._Just %~ makeAddFirstParam
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
        <&> cpParams . _FieldParams . Lens.ix 0 . fpHiddenIds <>~ hiddenIds
      return (params, lambda ^. V.lamResult)
    where
       hiddenIds = [expr ^. V.payload . Input.entityId]
  _ ->
    do
      params <-
        expr
        <&> (^. Input.mStored)
        & convertEmptyParams mRecursiveVar
      return (params, expr)

changeRecursionsToCalls :: MonadA m => V.Var -> Val (ExprIRef.ValIProperty m) -> T m ()
changeRecursionsToCalls =
  onMatchingSubexprs changeRecursion . const . isGetVarOf
  where
    changeRecursion prop =
      DataOps.newHole
      >>= ExprIRef.newValBody . V.BApp . V.Apply (Property.value prop)
      >>= Property.set prop

data ExprWhereItem a = ExprWhereItem
  { ewiBody :: Val a
  , ewiParam :: V.Var
  , ewiArg :: Val a
  , ewiHiddenPayloads :: [a]
  , ewiInferredType :: Type
  }

mExtractWhere :: Val (Input.Payload m a) -> Maybe (ExprWhereItem (Input.Payload m a))
mExtractWhere expr = do
  V.Apply func arg <- expr ^? ExprLens.valApply
  V.Lam param body <- func ^? V.body . ExprLens._BAbs
  Just ExprWhereItem
    { ewiBody = body
    , ewiParam = param
    , ewiArg = arg
    , ewiHiddenPayloads = (^. V.payload) <$> [expr, func]
    , ewiInferredType = arg ^. V.payload . Input.inferred . Infer.plType
    }

getParamsToHole :: MonadA m => V.Var -> Val (ExprIRef.ValIProperty m) -> T m ()
getParamsToHole = onMatchingSubexprs toHole . const . isGetVarOf

isGetFieldParam :: V.Var -> T.Tag -> Val t -> Bool
isGetFieldParam param tag
  (Val _ (V.BGetField (V.GetField (Val _ (V.BLeaf (V.LVar v))) t)))
  = t == tag && v == param
isGetFieldParam _ _ _ = False

getFieldParamsToHole :: MonadA m => T.Tag -> StoredLam m -> T m ()
getFieldParamsToHole tag (StoredLam (V.Lam param lamBody) _) =
  onMatchingSubexprs toHole (const (isGetFieldParam param tag)) lamBody

convertWhereItems ::
  (MonadA m, Monoid a) =>
  Val (Input.Payload m a) ->
  ConvertM m ([WhereItem Guid m (ExpressionU m a)], Val (Input.Payload m a))
convertWhereItems expr =
  case mExtractWhere expr of
  Nothing -> return ([], expr)
  Just ewi -> do
    let
      param = ewiParam ewi
      defGuid = UniqueId.toGuid param
      defEntityId = EntityId.ofLambdaParam param
    value <- convertBinder Nothing defGuid (ewiArg ewi)
    let
      mkWIActions topLevelProp bodyStored =
        ListItemActions
        { _itemDelete = do
             getParamsToHole param bodyStored
             void $ replaceWith topLevelProp $ bodyStored ^. V.payload
        , _itemAddNext = EntityId.ofLambdaParam . fst <$> DataOps.redexWrap topLevelProp
        }
    let
      hiddenData = ewiHiddenPayloads ewi ^. Lens.traversed . Input.userData
      item = WhereItem
        { _wiEntityId = defEntityId
        , _wiValue =
            value
            & dBody . rPayload . plData <>~ hiddenData
        , _wiActions =
            mkWIActions <$>
            expr ^. V.payload . Input.mStored <*>
            traverse (^. Input.mStored) (ewiBody ewi)
        , _wiName = UniqueId.toGuid param
        , _wiInferredType = ewiInferredType ewi
        }
    (nextItems, whereBody) <- convertWhereItems $ ewiBody ewi
    return (item : nextItems, whereBody)

makeBinder :: (MonadA m, Monoid a) =>
  Maybe (MkProperty m PresentationMode) ->
  ConventionalParams m a -> Val (Input.Payload m a) ->
  ConvertM m (Binder Guid m (ExpressionU m a))
makeBinder setPresentationMode convParams funcBody =
  ConvertM.local (ConvertM.scTagParamInfos <>~ cpParamInfos convParams) $
    do
      (whereItems, whereBody) <- convertWhereItems funcBody
      bodyS <- ConvertM.convertSubexpression whereBody
      return Binder
        { _dParams = convParams ^. cpParams
        , _dSetPresentationMode = setPresentationMode
        , _dBody = bodyS
        , _dWhereItems = whereItems
        , _dMActions =
          mkActions
          <$> cpMAddFirstParam convParams
          <*> whereBody ^. V.payload . Input.mStored
        }
  where
    mkActions addFirstParam whereStored =
      BinderActions
      { _baAddFirstParam = addFirstParam
      , _baAddInnermostWhereItem =
          EntityId.ofLambdaParam . fst <$> DataOps.redexWrap whereStored
      }

convertLam ::
  (MonadA m, Monoid a) =>
  V.Lam (Val (Input.Payload m a)) ->
  Input.Payload m a -> ConvertM m (ExpressionU m a)
convertLam lam@(V.Lam _ lamBody) exprPl =
  do
    mDeleteLam <- mkStoredLam lam exprPl & Lens._Just %%~ makeDeleteLambda Nothing
    convParams <- convertLamParams Nothing lam exprPl
    binder <- makeBinder Nothing convParams $ lam ^. V.lamResult
    let
      setToInnerExprAction =
        maybe NoInnerExpr SetToInnerExpr $ do
          guard $ Lens.nullOf ExprLens.valHole lamBody
          mDeleteLam
            <&> Lens.mapped .~ binder ^. dBody . rPayload . plEntityId
    BodyLam binder
      & addActions exprPl
      <&> rPayload . plActions . Lens._Just . setToInnerExpr .~ setToInnerExprAction

-- Where-item or definition (form of <name> [params] = <body>)
convertBinder ::
  (MonadA m, Monoid a) =>
  Maybe V.Var -> Guid ->
  Val (Input.Payload m a) -> ConvertM m (Binder Guid m (ExpressionU m a))
convertBinder mRecursiveVar defGuid expr =
  do
    (convParams, funcBody) <- convertParams mRecursiveVar expr
    let
      setPresentationMode
        | Lens.has (cpParams . _FieldParams) convParams =
          Just $ Anchors.assocPresentationMode defGuid
        | otherwise = Nothing
    makeBinder setPresentationMode convParams funcBody
