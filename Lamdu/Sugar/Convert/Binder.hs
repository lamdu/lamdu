{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types, PatternGuards, RecordWildCards #-}
module Lamdu.Sugar.Convert.Binder
  ( convertBinder, convertLam
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad (guard, void)
import Control.MonadA (MonadA)
import Data.Foldable (traverse_)
import Data.List.Utils (isLengthAtLeast)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Store.Property (Property)
import Data.Store.Transaction (Transaction, MkProperty)
import Data.Traversable (traverse, sequenceA)
import Lamdu.Expr.FlatComposite (FlatComposite(..))
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val(..))
import Lamdu.Sugar.Convert.Expression.Actions (addActions)
import Lamdu.Sugar.Convert.Monad (ConvertM)
import Lamdu.Sugar.Convert.ParamList (ParamList)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.FlatComposite as FlatComposite
import qualified Lamdu.Expr.GenIds as GenIds
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Convert.ParamList as ParamList
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

type T = Transaction

data ConventionalParams m a = ConventionalParams
  { cpTags :: Set T.Tag
  , cpParamInfos :: Map T.Tag ConvertM.TagParamInfo
  , _cpParams :: [FuncParam Guid m]
  , cpMAddFirstParam :: Maybe (T m EntityId)
  }

cpParams :: Lens' (ConventionalParams m a) [FuncParam Guid m]
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

mkStoredLam ::
  V.Lam (Val (Input.Payload m a)) ->
  Input.Payload m a -> Maybe (StoredLam m)
mkStoredLam lam pl =
  StoredLam
  <$> (lam & (traverse . traverse) (^. Input.mStored))
  <*> pl ^. Input.mStored

makeDeleteLambda :: MonadA m => StoredLam m -> ConvertM m (T m ())
makeDeleteLambda (StoredLam (V.Lam paramVar lamBodyStored) lambdaProp) =
  do
    protectedSetToVal <- ConvertM.typeProtectedSetToVal
    return $
      do
        getParamsToHole paramVar lamBodyStored
        let lamBodyI = Property.value (lamBodyStored ^. V.payload)
        void $ protectedSetToVal lambdaProp lamBodyI <&> EntityId.ofValI

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
wrapArgWithRecord tag0 tag1 oldArg =
  do
    hole <- DataOps.newHole
    ExprIRef.newValBody (V.BLeaf V.LRecEmpty)
      >>= ExprIRef.newValBody . V.BRecExtend . V.RecExtend tag0 hole
      >>= ExprIRef.newValBody . V.BRecExtend . V.RecExtend tag1 oldArg

convertVarToGetField ::
  MonadA m => T.Tag -> V.Var -> Val (Property (T m) (ExprIRef.ValI m)) -> T m ()
convertVarToGetField tagForVar paramVar =
  onMatchingSubexprs convertVar (const (isGetVarOf paramVar))
  where
    convertVar prop =
      Property.value prop & (`V.GetField` tagForVar) & V.BGetField
      & ExprIRef.newValBody
      >>= Property.set prop

wrapOnError ::
  Functor f =>
  (Property (T m) a -> a -> f (ExprIRef.ValI m)) ->
  Property (T m) a -> f EntityId
wrapOnError protectedSetToVal prop = protectedSetToVal prop (Property.value prop) <&> EntityId.ofValI

makeConvertToRecordParams ::
  MonadA m => Maybe V.Var -> StoredLam m -> ConvertM m (T m EntityId)
makeConvertToRecordParams mRecursiveVar (StoredLam (V.Lam paramVar lamBody) lamProp) =
  do
    protectedSetToVal <- ConvertM.typeProtectedSetToVal
    return $
      do
        tagForVar <- newTag
        tagForNewVar <- newTag
        Transaction.setP paramList $ Just [tagForVar, tagForNewVar]
        convertVarToGetField tagForVar paramVar lamBody
        mRecursiveVar
          & traverse_
            (changeRecursiveCallArgs
             (wrapArgWithRecord tagForVar tagForNewVar)
             (lamBody ^. V.payload))
        wrapOnError protectedSetToVal lamProp
  where
    paramList = ParamList.mkProp (Property.value lamProp)

convertRecordParams ::
  (MonadA m, Monoid a) => [FieldParam] ->
  V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
  ConvertM m (ConventionalParams m a)
convertRecordParams fieldParams lam@(V.Lam param _) pl = do
  params <- traverse mkParam fieldParams
  pure ConventionalParams
    { cpTags = Set.fromList tags
    , cpParamInfos = mconcat $ mkParamInfo <$> fieldParams
    , _cpParams = params
    , cpMAddFirstParam = addParam (:tags) . slParamList <$> mStoredLam
    }
  where
    addParam insert paramListProp =
      do
        tag <- newTag
        Transaction.setP paramListProp $ Just $ insert tag
        return $ EntityId.ofLambdaTagParam param tag
    tags = fpTag <$> fieldParams
    fpIdEntityId = EntityId.ofLambdaTagParam param . fpTag
    mkParamInfo fp =
      Map.singleton (fpTag fp) . ConvertM.TagParamInfo param $ fpIdEntityId fp
    mStoredLam = mkStoredLam lam pl
    mkParam fp =
      pure FuncParam
      { _fpName = UniqueId.toGuid $ fpTag fp
      , _fpId = fpIdEntityId fp
      , _fpVarKind = FuncFieldParameter
      , _fpInferredType = fpFieldType fp
      , _fpMActions = fpActions fp <$> mStoredLam
      , _fpHiddenIds = []
      }
    insertAfter pos tag =
      break (== pos) tags & \(pre, post) -> pre ++ [tag] ++ post
    fpActions fp storedLam =
      FuncParamActions
      { _fpListItemActions = ListItemActions
        { _itemAddNext = addParam (insertAfter (fpTag fp)) $ slParamList storedLam
        , _itemDelete = delFieldParam tags fp storedLam
        }
      }

delFieldParam :: MonadA m => [T.Tag] -> FieldParam -> StoredLam m -> T m ()
delFieldParam tags fp storedLam =
  do
    Transaction.setP (slParamList storedLam) $ Just $ newTags
    getFieldParamsToHole (fpTag fp) storedLam
  where
    newTags = List.delete (fpTag fp) tags

slParamList :: MonadA m => StoredLam m -> Transaction.MkProperty m (Maybe ParamList)
slParamList = ParamList.mkProp . Property.value . slLambdaProp

makeNonRecordParamActions ::
  MonadA m => Maybe V.Var -> StoredLam m -> ConvertM m (FuncParamActions m, T m EntityId)
makeNonRecordParamActions mRecursiveVar storedLam =
  do
    delete <- makeDeleteLambda storedLam
    convertToRecordParams <- makeConvertToRecordParams mRecursiveVar storedLam
    return
      ( FuncParamActions
        { _fpListItemActions =
          ListItemActions
          { _itemDelete = delete
          , _itemAddNext =
            convertToRecordParams
          }
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
        , _fpVarKind = FuncParameter
        , _fpId = paramEntityId
        , _fpInferredType = paramType
        , _fpMActions = fst <$> mActions
        , _fpHiddenIds = []
        }
    pure ConventionalParams
      { cpTags = mempty
      , cpParamInfos = Map.empty
      , _cpParams = [funcParam]
      , cpMAddFirstParam = snd <$> mActions
      }
  where
    mStoredLam = mkStoredLam lam lamExprPl
    paramEntityId = EntityId.ofLambdaParam param
    paramType =
      fromMaybe (error "Lambda value not inferred to a function type?!") $
      lamExprPl ^? Input.inferred . Infer.plType . ExprLens._TFun . Lens._1

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
          convertRecordParams fieldParams lambda lambdaPl
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
            return $ EntityId.ofLambdaParam newParam

    pure
      ConventionalParams
      { cpTags = mempty
      , cpParamInfos = Map.empty
      , _cpParams = []
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
        <&> cpParams . Lens.ix 0 . fpHiddenIds <>~ [expr ^. V.payload . Input.entityId]
      return (params, lambda ^. V.lamResult)
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

getFieldParamsToHole :: MonadA m => T.Tag -> StoredLam m -> T m ()
getFieldParamsToHole tag (StoredLam (V.Lam param lamBody) _) =
  onMatchingSubexprs toHole cond lamBody
  where
    cond _
      (Val _ (V.BGetField
        (V.GetField (Val _ (V.BLeaf (V.LVar v))) t)
      )) = t == tag && v == param
    cond _ _ = False

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
    mDeleteLam <- mkStoredLam lam exprPl & Lens._Just %%~ makeDeleteLambda
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
        | isLengthAtLeast 2 (convParams ^. cpParams) =
          Just $ Anchors.assocPresentationMode defGuid
        | otherwise = Nothing
    makeBinder setPresentationMode convParams funcBody
