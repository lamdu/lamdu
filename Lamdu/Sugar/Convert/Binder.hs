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

toHole :: MonadA m => ExprIRef.ValIProperty m -> T m ()
toHole = void . DataOps.setToHole

isGetParamOf :: V.Var -> Val a -> Bool
isGetParamOf = Lens.anyOf ExprLens.valVar . (==)

makeDeleteLambda ::
  MonadA m =>
  V.Lam (Val (ExprIRef.ValIProperty m)) -> ExprIRef.ValIProperty m ->
  ConvertM m (T m EntityId)
makeDeleteLambda (V.Lam paramVar lamBodyStored) stored =
  do
    protectedSetToVal <- ConvertM.typeProtectedSetToVal
    return $
      do
        getParamsToHole paramVar lamBodyStored
        let lamBodyI = Property.value (lamBodyStored ^. V.payload)
        protectedSetToVal stored lamBodyI <&> EntityId.ofValI

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
  FieldParam -> V.Lam (Val (ExprIRef.ValIProperty m)) -> T m EntityId
delFieldParam fp lamStored =
   do
     getFieldParamsToHole (fpTag fp) lamStored
     return $ error "TODO: cursor"

makeLamParamActions :: MonadA m =>
  V.Lam (Val (ExprIRef.ValIProperty m)) -> ExprIRef.ValIProperty m ->
  ConvertM m (FuncParamActions m)
makeLamParamActions lam lambdaProp =
  do
    delete <- makeDeleteLambda lam lambdaProp
    return
      FuncParamActions
      { _fpListItemActions =
        ListItemActions
        { _itemDelete = delete
        , _itemAddNext = error "TODO: addSecondParam"
        }
      }

convertLamParam :: MonadA m =>
  V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
  ConvertM m (ConventionalParams m a)
convertLamParam lam@(V.Lam param _) lamExprPl =
  do
    mActions <-
      sequenceA $ makeLamParamActions
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
    -- _existingParamTypeIRef =
    --   fromMaybe (error "Only stored record param type is converted as record") $
    --   existingParamType ^? SugarInfer.exprIRef
--     _existingParamAsTag = T.Tag existingParamVar
--     addSecondParam mkFields = do
--       let existingParamField = (existingParamAsTag, existingParamTypeIRef)
--       (newTag, newParamField) <- newField
--       newParamTypeI <-
--         ExprIRef.newExprBody . V.VRec . V.Record KType $
--         mkFields existingParamField newParamField
--       newParamsGuid <- Transaction.newKey
--       ExprIRef.writeExprBody (Property.value lamProp) $
--         ExprUtil.makeLambda newParamsGuid newParamTypeI .
--         Property.value $ bodyWithStored ^. V.payload
--       let
--         toGetField iref = do
--           recordRef <- ExprIRef.newExprBody $ ExprLens.bodyParameterRef # newParamsGuid
--           tagRef <- ExprIRef.newExprBody existingParamAsTag
--           ExprIRef.writeExprBody iref $
--             V.VGetField V.GetField
--             { V._getFieldRecord = recordRef
--             , V._getFieldTag = tagRef
--             }
--       onMatchingSubexprs (toGetField . Property.value)
--         (isGetParamOf existingParamVar) bodyWithStored
--       let lamGuid = ExprIRef.valIGuid $ Property.value lamProp
--       pure $ Guid.combine lamGuid $ newTag ^. Lens.from V.tag

isParamAlwaysUsedWithGetField :: V.Lam (Val a) -> Bool
isParamAlwaysUsedWithGetField (V.Lam param body) =
  Lens.nullOf (ExprLens.payloadsIndexedByPath . Lens.ifiltered cond) body
  where
    cond (_ : Val () V.BGetField{} : _) _ = False
    cond (Val () (V.BLeaf (V.LVar v)) : _) _ = v == param
    cond _ _ = False

convertLamParams ::
  (MonadA m, Monoid a) =>
  V.Lam (Val (Input.Payload m a)) -> Input.Payload m a ->
  ConvertM m (ConventionalParams m a)
convertLamParams lambda lambdaPl =
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
      _ -> convertLamParam lambda lambdaPl
  where
    makeFieldParam (tag, typeExpr) =
      FieldParam
      { fpTag = tag
      , fpFieldType = typeExpr
      }

convertParams ::
  (MonadA m, Monoid a) =>
  Val (Input.Payload m a) ->
  ConvertM m
  ( ConventionalParams m a
  , Val (Input.Payload m a)
  )
convertParams expr =
  case expr ^. V.body of
  V.BAbs lambda ->
    do
      params <- convertLamParams lambda (expr ^. V.payload)
      return (params, lambda ^. V.lamResult)
  _ -> return (emptyConventionalParams stored, expr)
  where
    stored =
      fromMaybe (error "Definition body is always stored!") $
      expr ^. V.payload . Input.mStored

emptyConventionalParams :: MonadA m => ExprIRef.ValIProperty m -> ConventionalParams m a
emptyConventionalParams stored = ConventionalParams
  { cpTags = mempty
  , cpParamInfos = Map.empty
  , cpParams = []
  , cpAddFirstParam = lambdaWrap stored
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

lambdaWrap :: MonadA m => ExprIRef.ValIProperty m -> T m EntityId
lambdaWrap stored =
  f <$> DataOps.lambdaWrap stored
  where
    f (newParam, _) = EntityId.ofLambdaParam newParam

getParamsToHole :: MonadA m =>
  V.Var -> Val (ExprIRef.ValIProperty m) -> T m ()
getParamsToHole = onMatchingSubexprs toHole . const . isGetParamOf

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
    value <- convertBinder defGuid (ewiArg ewi)
    let
      mkWIActions topLevelProp bodyStored =
        ListItemActions
        { _itemDelete = do
             getParamsToHole param bodyStored
             replaceWith topLevelProp $ bodyStored ^. V.payload
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

-- addFirstFieldParam :: MonadA m => Guid -> ExprIRef.ValI m -> T m Guid
-- addFirstFieldParam lamGuid recordI = do
--   recordBody <- ExprIRef.readValBody recordI
--   case recordBody ^? V._VRec . ExprLens.kindedRecordFields KType of
--     Just fields -> do
--       (newTag, field) <- newField
--       ExprIRef.writeExprBody recordI $
--         V.VRec . V.Record KType $ (newTag, field) : fields
--       pure $ Guid.combine lamGuid $ newTag ^. Lens.from V.tag
--     _ -> pure $ ExprIRef.valIGuid recordI

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
    convParams <- convertLamParams lam pl
    makeBinder Nothing convParams $ lam ^. V.lamResult

convertBinder ::
  (MonadA m, Monoid a) =>
  Guid -> Val (Input.Payload m a) -> ConvertM m (Binder Guid m (ExpressionU m a))
convertBinder defGuid expr =
  do
    (convParams, funcBody) <- convertParams expr
    let
      setPresentationMode
        | isLengthAtLeast 2 (cpParams convParams) =
          Just $ Anchors.assocPresentationMode defGuid
        | otherwise = Nothing
    makeBinder setPresentationMode convParams funcBody
