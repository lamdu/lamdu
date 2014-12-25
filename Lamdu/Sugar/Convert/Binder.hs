{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types, PatternGuards #-}
module Lamdu.Sugar.Convert.Binder
  ( convertBinder, convertLam
  , makeDeleteLambda
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Monad (void)
import Control.MonadA (MonadA)
import Data.List.Utils (isLengthAtLeast)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction, MkProperty)
import Data.Traversable (traverse, sequenceA)
import Lamdu.Expr.FlatComposite (FlatComposite(..))
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val(..))
import Lamdu.Sugar.Convert.Monad (ConvertM)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Store.Property as Property
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.FlatComposite as FlatComposite
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.GenIds as GenIds
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

type T = Transaction

data ConventionalParams m a = ConventionalParams
  { cpTags :: Set T.Tag
  , cpParamInfos :: Map T.Tag ConvertM.TagParamInfo
  , cpParams :: [FuncParam Guid m]
  , cpAddFirstParam :: T m EntityId
  , cpHiddenPayloads :: [Input.Payload m a]
  }

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

makeDeleteLambda ::
  MonadA m =>
  V.Lam (Val (ExprIRef.ValIProperty m)) -> ExprIRef.ValIProperty m ->
  ConvertM m (T m ())
makeDeleteLambda (V.Lam paramVar lamBodyStored) stored =
  do
    protectedSetToVal <- ConvertM.typeProtectedSetToVal
    return $
      do
        getParamsToHole paramVar lamBodyStored
        let lamBodyI = Property.value (lamBodyStored ^. V.payload)
        void $ protectedSetToVal stored lamBodyI <&> EntityId.ofValI

newTag :: MonadA m => T m T.Tag
newTag = GenIds.transaction GenIds.randomTag

isRecursiveCallArg :: V.Var -> [Val ()] -> Bool
isRecursiveCallArg recursiveVar (cur : parent : _) =
  Lens.allOf ExprLens.valVar (/= recursiveVar) cur &&
  Lens.anyOf (ExprLens.valApply . V.applyFunc . ExprLens.valVar)
  (== recursiveVar) parent
isRecursiveCallArg _ _ = False

fixRecursiveCallsToUseRecords :: MonadA m =>
  T.Tag -> T.Tag -> V.Var -> Val (ExprIRef.ValIProperty m) -> Transaction m ()
fixRecursiveCallsToUseRecords tagForVar tagForNewVar =
  onMatchingSubexprsWithPath fixRecurseArg . const . isRecursiveCallArg
  where
    fixRecurseArg prop =
      do
        hole <- DataOps.newHole
        ExprIRef.newValBody (V.BLeaf V.LRecEmpty)
          >>= ExprIRef.newValBody . V.BRecExtend . V.RecExtend tagForNewVar hole
          >>= ExprIRef.newValBody . V.BRecExtend . V.RecExtend tagForVar (Property.value prop)
          >>= Property.set prop

makeConvertToRecordParams ::
  MonadA m =>
  Maybe V.Var ->
  V.Lam (Val (ExprIRef.ValIProperty m)) -> ExprIRef.ValIProperty m ->
  ConvertM m (T m EntityId)
makeConvertToRecordParams mRecursiveVar (V.Lam paramVar lamBody) stored =
  do
    protectedSetToVal <- ConvertM.typeProtectedSetToVal
    return $
      do
        tagForVar <- newTag
        tagForNewVar <- newTag
        let
          convertVar prop =
            Property.value prop & (`V.GetField` tagForVar) & V.BGetField
            & ExprIRef.newValBody
            >>= Property.set prop
        onMatchingSubexprs convertVar (const (isGetVarOf paramVar)) lamBody
        case mRecursiveVar of
          Nothing -> return ()
          Just recursiveVar ->
            fixRecursiveCallsToUseRecords tagForVar tagForNewVar recursiveVar lamBody
        protectedSetToVal stored (Property.value stored) <&> EntityId.ofValI

mkRecordParams ::
  (MonadA m, Monoid a) =>
  [FieldParam] ->
  V.Lam (Maybe (Val (ExprIRef.ValIProperty m))) ->
  Input.Payload m a ->
  ConvertM m (ConventionalParams m a)
mkRecordParams fieldParams lam@(V.Lam param _) lambdaPl = do
  params <- traverse mkParam fieldParams
  pure ConventionalParams
    { cpTags = Set.fromList $ fpTag <$> fieldParams
    , cpParamInfos = mconcat $ mkParamInfo <$> fieldParams
    , cpParams = params
    , cpAddFirstParam =
      error "TODO cpAddFirstParam"--  addFirstFieldParam lamEntityId $
      -- fromMaybe (error "Record param type must be stored!") mParamTypeI
    , cpHiddenPayloads = [lambdaPl]
    }
  where
    fpIdEntityId = EntityId.ofLambdaTagParam param . fpTag
    mkParamInfo fp =
      Map.singleton (fpTag fp) . ConvertM.TagParamInfo param $ fpIdEntityId fp
    mkParam fp =
      pure FuncParam
      { _fpName = UniqueId.toGuid $ fpTag fp
      , _fpId = fpIdEntityId fp
      , _fpVarKind = FuncFieldParameter
      , _fpInferredType = fpFieldType fp
      , _fpMActions = fpActions fp <$> sequenceA lam
      , _fpHiddenIds = []
      }
    fpActions fp lamStored =
      FuncParamActions
      { _fpListItemActions = ListItemActions
        { _itemAddNext = error "TODO:add" -- addFieldParamAfter lamGuid tagExprGuid paramTypeI
        , _itemDelete = delFieldParam fp lamStored
        }
      }

-- addFieldParamAfter :: MonadA m => Guid -> Guid -> ExprIRef.ValI m -> T m Guid
-- addFieldParamAfter lamGuid tagExprGuid paramTypeI =
--   rereadFieldParamTypes tagExprGuid paramTypeI $
--   \prevFields theField nextFields -> do
--     fieldGuid <- Transaction.newKey
--     holeTypeI <- DataOps.newHole
--     rewriteFieldParamTypes paramTypeI $
--       prevFields ++ theField : (T.Tag fieldGuid, holeTypeI) : nextFields
--     pure $ Guid.combine lamGuid fieldGuid

delFieldParam :: MonadA m =>
  FieldParam -> V.Lam (Val (ExprIRef.ValIProperty m)) -> T m ()
delFieldParam fp = getFieldParamsToHole (fpTag fp)

makeLamParamActions :: MonadA m =>
  Maybe V.Var ->
  V.Lam (Val (ExprIRef.ValIProperty m)) -> ExprIRef.ValIProperty m ->
  ConvertM m (FuncParamActions m)
makeLamParamActions mRecursiveVar lam lambdaProp =
  do
    delete <- makeDeleteLambda lam lambdaProp
    addParam <- makeConvertToRecordParams mRecursiveVar lam lambdaProp
    return
      FuncParamActions
      { _fpListItemActions =
        ListItemActions
        { _itemDelete = delete
        , _itemAddNext = addParam
        }
      }

convertLamParam :: MonadA m =>
  Maybe V.Var ->
  V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
  ConvertM m (ConventionalParams m a)
convertLamParam mRecursiveVar lam@(V.Lam param _) lamExprPl =
  do
    mActions <-
      sequenceA $ makeLamParamActions mRecursiveVar
      <$> (lam & (traverse . traverse) (^. Input.mStored))
      <*> lamExprPl ^. Input.mStored
    let
      funcParam =
        FuncParam
        { _fpName = UniqueId.toGuid param
        , _fpVarKind = FuncParameter
        , _fpId = paramEntityId
        , _fpInferredType = paramType
        , _fpMActions = mActions
        , _fpHiddenIds = [lamExprPl ^. Input.entityId]
        }
    pure ConventionalParams
      { cpTags = mempty
      , cpParamInfos = Map.empty
      , cpParams = [funcParam]
      , cpAddFirstParam = error "TODO: addFirstParam"
      , cpHiddenPayloads = []
      }
  where
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
          mkRecordParams fieldParams (lambda <&> traverse %%~ (^. Input.mStored)) lambdaPl
        where
          tagsInInnerScope = Map.keysSet fields
          FlatComposite fields extension = FlatComposite.fromComposite composite
          fieldParams = map makeFieldParam $ Map.toList fields
      _ -> convertLamParam mRecursiveVar lambda lambdaPl
  where
    makeFieldParam (tag, typeExpr) =
      FieldParam
      { fpTag = tag
      , fpFieldType = typeExpr
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
      params <- convertLamParams mRecursiveVar lambda (expr ^. V.payload)
      return (params, lambda ^. V.lamResult)
  _ ->
    do
      params <-
        emptyConventionalParams mRecursiveVar $
        fromMaybe (error "Definition body is always stored!") $ -- TODO: use maybes rather than error
        traverse (^. Input.mStored) expr
      return (params, expr)

fixRecursionsToCalls :: MonadA m =>
  V.Var -> Val (ExprIRef.ValIProperty m) -> Transaction m ()
fixRecursionsToCalls =
  onMatchingSubexprs fixRecursion . const . isGetVarOf
  where
    fixRecursion prop =
      DataOps.newHole
      >>= ExprIRef.newValBody . V.BApp . V.Apply (Property.value prop)
      >>= Property.set prop

makeAddFirstParam :: MonadA m =>
  Maybe V.Var -> Val (ExprIRef.ValIProperty m) -> ConvertM m (T m EntityId)
makeAddFirstParam mRecursiveVar val =
  do
    protectedSetToVal <- ConvertM.typeProtectedSetToVal
    return $
      do
        (newParam, dst) <- DataOps.lambdaWrap stored
        case mRecursiveVar of
          Nothing -> return ()
          Just recursiveVar -> fixRecursionsToCalls recursiveVar val
        void $ protectedSetToVal stored dst
        return $ EntityId.ofLambdaParam newParam
  where
    stored = val ^. V.payload

emptyConventionalParams :: MonadA m =>
  Maybe V.Var -> Val (ExprIRef.ValIProperty m) -> ConvertM m (ConventionalParams m a)
emptyConventionalParams mRecursiveVar val =
  do
    addFirstParam <- makeAddFirstParam mRecursiveVar val
    pure
      ConventionalParams
      { cpTags = mempty
      , cpParamInfos = Map.empty
      , cpParams = []
      , cpAddFirstParam = addFirstParam
      , cpHiddenPayloads = []
      }

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

getParamsToHole :: MonadA m =>
  V.Var -> Val (ExprIRef.ValIProperty m) -> T m ()
getParamsToHole = onMatchingSubexprs toHole . const . isGetVarOf

getFieldParamsToHole :: MonadA m =>
  T.Tag -> V.Lam (Val (ExprIRef.ValIProperty m)) -> T m ()
getFieldParamsToHole tag (V.Lam param lamBody) =
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
        { _dParams = cpParams convParams
        , _dSetPresentationMode = setPresentationMode
        , _dBody =
          bodyS
          & rPayload . plData <>~
            cpHiddenPayloads convParams ^. Lens.traversed . Input.userData
        , _dWhereItems = whereItems
        , _dMActions = mkActions <$> whereBody ^. V.payload . Input.mStored
        }
  where
    mkActions whereStored =
      BinderActions
      { _baAddFirstParam = cpAddFirstParam convParams
      , _baAddInnermostWhereItem =
          EntityId.ofLambdaParam . fst <$> DataOps.redexWrap whereStored
      }

convertLam ::
  (MonadA m, Monoid a) =>
  V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
  ConvertM m (Binder Guid m (ExpressionU m a))
convertLam lam pl =
  do
    convParams <- convertLamParams Nothing lam pl
    makeBinder Nothing convParams $ lam ^. V.lamResult

convertBinder ::
  (MonadA m, Monoid a) =>
  Maybe V.Var -> Guid ->
  Val (Input.Payload m a) -> ConvertM m (Binder Guid m (ExpressionU m a))
convertBinder mRecursiveVar defGuid expr =
  do
    (convParams, funcBody) <- convertParams mRecursiveVar expr
    let
      setPresentationMode
        | isLengthAtLeast 2 (cpParams convParams) =
          Just $ Anchors.assocPresentationMode defGuid
        | otherwise = Nothing
    makeBinder setPresentationMode convParams funcBody
