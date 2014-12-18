{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types, PatternGuards #-}
module Lamdu.Sugar.Convert.DefExpr
  ( convert
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.MonadA (MonadA)
import Data.List.Utils (isLengthAtLeast)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..), (<>))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Lamdu.Expr.FlatComposite (FlatComposite(..))
import Lamdu.Expr.IRef (DefI)
import Lamdu.Expr.Scheme (Scheme(..))
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val(..))
import Lamdu.Sugar.Convert.Monad (ConvertM, Context(..))
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.FlatComposite as FlatComposite
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Scheme as Scheme
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Infer as SugarInfer
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

type T = Transaction

mkContext ::
  MonadA m =>
  DefI m ->
  Anchors.Code (Transaction.MkProperty m) m ->
  Infer.Context -> T m (Context m)
mkContext defI cp inferContext = do
  specialFunctions <- Transaction.getP $ Anchors.specialFunctions cp
  return Context
    { _scInferContext = inferContext
    , _scCodeAnchors = cp
    , _scSpecialFunctions = specialFunctions
    , _scTagParamInfos = mempty
    , _scRecordParamsInfos = mempty
    , _scReinferCheckDefinition =
        do
          defBody <- Transaction.readIRef defI
          case defBody of
            Definition.BodyBuiltin {} -> return True
            Definition.BodyExpr (Definition.Expr valI _) ->
              ExprIRef.readVal valI
              >>= runMaybeT . SugarInfer.loadInfer
              <&> Lens.has Lens._Just
    , scConvertSubexpression = ConvertExpr.convert
    }

data ConventionalParams m a = ConventionalParams
  { cpTags :: Set T.Tag
  , cpParamInfos :: Map T.Tag ConvertM.TagParamInfo
  , cpRecordParamsInfos :: Map V.Var (ConvertM.RecordParamsInfo m)
  , cpParams :: [FuncParam Guid m]
  , cpAddFirstParam :: T m EntityId
  , cpHiddenPayloads :: [InputPayload m a]
  }

data FieldParam = FieldParam
  { fpTag :: T.Tag
  , fpFieldType :: Type
  }

mkRecordParams ::
  (MonadA m, Monoid a) =>
  ConvertM.RecordParamsInfo m -> V.Var -> [FieldParam] ->
  Val (InputPayload m a) ->
  Maybe (Val (ExprIRef.ValIProperty m)) ->
  ConvertM m (ConventionalParams m a)
mkRecordParams recordParamsInfo param fieldParams lambdaExprI _mBodyStored = do
  params <- traverse mkParam fieldParams
  pure ConventionalParams
    { cpTags = Set.fromList $ fpTag <$> fieldParams
    , cpParamInfos = mconcat $ mkParamInfo <$> fieldParams
    , cpRecordParamsInfos = Map.singleton param recordParamsInfo
    , cpParams = params
    , cpAddFirstParam =
      error "TODO cpAddFirstParam"--  addFirstFieldParam lamEntityId $
      -- fromMaybe (error "Record param type must be stored!") mParamTypeI
    , cpHiddenPayloads = [lambdaExprI ^. V.payload]
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
      , _fpMActions = error "TODO: _fpMActions"
        -- fpActions (fpIdEntityId fp)
        -- <$> mLambdaP <*> mParamTypeI <*> mBodyStored
      , _fpHiddenIds = []
      }
--     fpActions tagExprGuid lambdaP paramTypeI bodyStored =
--       FuncParamActions
--       { _fpListItemActions = ListItemActions
--         { _itemAddNext = error "TODO:add" -- addFieldParamAfter lamGuid tagExprGuid paramTypeI
--         , _itemDelete =
--           error "TODO:del"
-- --          delFieldParam tagExprGuid paramTypeI param lambdaP bodyStored
--         }
--       }

-- type ExprField m = (T.Tag, ExprIRef.ValI m)
-- rereadFieldParamTypes ::
--   MonadA m =>
--   Guid -> ExprIRef.ValI m ->
--   ([ExprField m] -> ExprField m -> [ExprField m] -> T m Guid) ->
--   T m Guid
-- rereadFieldParamTypes tagExprGuid paramTypeI f = do
--   paramType <- ExprIRef.readValBody paramTypeI
--   let
--     mBrokenFields =
--       paramType ^? V._VRec . ExprLens.kindedRecordFields KType .
--       (Lens.to . break) ((T.Tag tagExprGuid ==) . fst)
--   case mBrokenFields of
--     Just (prevFields, theField : nextFields) -> f prevFields theField nextFields
--     _ -> return tagExprGuid

-- rewriteFieldParamTypes ::
--   MonadA m => ExprIRef.ValI m -> [ExprField m] -> T m ()
-- rewriteFieldParamTypes paramTypeI fields =
--   ExprIRef.writeExprBody paramTypeI . V.VRec $
--   V.Record KType fields

-- addFieldParamAfter :: MonadA m => Guid -> Guid -> ExprIRef.ValI m -> T m Guid
-- addFieldParamAfter lamGuid tagExprGuid paramTypeI =
--   rereadFieldParamTypes tagExprGuid paramTypeI $
--   \prevFields theField nextFields -> do
--     fieldGuid <- Transaction.newKey
--     holeTypeI <- DataOps.newHole
--     rewriteFieldParamTypes paramTypeI $
--       prevFields ++ theField : (T.Tag fieldGuid, holeTypeI) : nextFields
--     pure $ Guid.combine lamGuid fieldGuid

-- delFieldParam ::
--   MonadA m => Guid -> ExprIRef.ValI m -> Guid ->
--   ExprIRef.ValIProperty m -> Val (ExprIRef.ValIProperty m) -> T m Guid
-- delFieldParam tagExprGuid paramTypeI paramGuid lambdaP bodyStored =
--   rereadFieldParamTypes tagExprGuid paramTypeI $
--   \prevFields ((T.Tag tagG), _) nextFields -> do
--     deleteFieldParamRef paramGuid tagG bodyStored
--     case prevFields ++ nextFields of
--       [] -> error "We were given fewer than 2 field params, which should never happen"
--       [(T.Tag fieldTagGuid, fieldTypeI)] -> do
--         ExprIRef.writeExprBody (Property.value lambdaP) $
--           ExprUtil.makeLambda fieldTagGuid fieldTypeI bodyI
--         deleteParamRef paramGuid bodyStored
--         let
--           toGetParam iref =
--             ExprIRef.writeExprBody iref $
--             ExprLens.bodyParameterRef # fieldTagGuid
--         onMatchingSubexprs (toGetParam . Property.value)
--           (isGetFieldParam paramGuid fieldTagGuid) bodyStored
--         pure $ Guid.combine lamGuid fieldTagGuid
--       newFields -> do
--         rewriteFieldParamTypes paramTypeI newFields
--         pure $ dest prevFields nextFields
--   where
--     lamGuid = ExprIRef.valIGuid $ Property.value lambdaP
--     bodyI = bodyStored ^. V.payload . Property.pVal
--     dest prevFields nextFields =
--       fromMaybe (ExprIRef.valIGuid bodyI) . listToMaybe $
--       map (getParamGuidFromTagExprI . fst) $ nextFields ++ reverse prevFields
--     getParamGuidFromTagExprI (T.Tag tGuid) = Guid.combine lamGuid tGuid

convertDefinitionParams ::
  (MonadA m, Monoid a) =>
  ConvertM.RecordParamsInfo m -> Set T.Tag -> Val (InputPayload m a) ->
  ConvertM m
  ( ConventionalParams m a
  , Val (InputPayload m a)
  )
convertDefinitionParams recordParamsInfo usedTags expr =
  case expr ^. V.body of
  V.BAbs lambda@(V.Lam paramVar body) -> do
    let lambdaPl = expr ^. V.payload
    param <-
      ConvertExpr.convertPositionalFuncParam lambda lambdaPl
      <&> fpHiddenIds <>~ [lambdaPl ^. ipEntityId]
    case T.TVar undefined of -- TODO: we should read associated data for param list
      T.TRecord composite
        | Nothing <- extension
        , Map.size fields >= 2
        , Set.null (usedTags `Set.intersection` Map.keysSet fields) -> do
          convParams <-
            mkRecordParams recordParamsInfo paramVar fieldParams
            expr (traverse (^. ipStored) body)
          return (convParams, body)
        where
          FlatComposite fields extension = FlatComposite.fromComposite composite
          fieldParams = map makeFieldParam $ Map.toList fields
      _ ->
        pure
        ( singleConventionalParam stored param paramVar body
        , body
        )
  _ -> return (emptyConventionalParams stored, expr)
  where
    stored =
      fromMaybe (error "Definition body is always stored!") $
      expr ^. V.payload . ipStored
    makeFieldParam (tag, typeExpr) =
      FieldParam
      { fpTag = tag
      , fpFieldType = typeExpr
      }

singleConventionalParam ::
  MonadA m =>
  ExprIRef.ValIProperty m -> FuncParam Guid m ->
  V.Var -> Val (InputPayload m a) -> ConventionalParams m a
singleConventionalParam _lamProp existingParam _existingParamVar body =
  ConventionalParams
  { cpTags = mempty
  , cpParamInfos = Map.empty
  , cpRecordParamsInfos = Map.empty
  , cpParams =
    [ existingParam & fpMActions . Lens._Just . fpListItemActions . itemAddNext .~
      addSecondParam (\old new -> [old, new])
    ]
  , cpAddFirstParam = addSecondParam (\old new -> [new, old])
  , cpHiddenPayloads = []
  }
  where
    -- _existingParamTypeIRef =
    --   fromMaybe (error "Only stored record param type is converted as record") $
    --   existingParamType ^? SugarInfer.exprIRef
    _bodyWithStored =
      fromMaybe (error "Definition body should be stored") $
      traverse (^. ipStored) body
    addSecondParam _mkFields = error "TODO: addSecondParam"
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

emptyConventionalParams :: MonadA m => ExprIRef.ValIProperty m -> ConventionalParams m a
emptyConventionalParams stored = ConventionalParams
  { cpTags = mempty
  , cpParamInfos = Map.empty
  , cpRecordParamsInfos = Map.empty
  , cpParams = []
  , cpAddFirstParam = ConvertExpr.lambdaWrap stored
  , cpHiddenPayloads = []
  }

data ExprWhereItem a = ExprWhereItem
  { ewiBody :: Val a
  , ewiParam :: V.Var
  , ewiArg :: Val a
  , ewiHiddenPayloads :: [a]
  , ewiInferredType :: Type
  }

mExtractWhere :: Val (InputPayload m a) -> Maybe (ExprWhereItem (InputPayload m a))
mExtractWhere expr = do
  V.Apply func arg <- expr ^? ExprLens.valApply
  V.Lam param body <- func ^? V.body . ExprLens._BAbs
  Just ExprWhereItem
    { ewiBody = body
    , ewiParam = param
    , ewiArg = arg
    , ewiHiddenPayloads = (^. V.payload) <$> [expr, func]
    , ewiInferredType = arg ^. V.payload . ipInferred . Infer.plType
    }

convertWhereItems ::
  (MonadA m, Monoid a) =>
  Set T.Tag ->
  Val (InputPayload m a) ->
  ConvertM m ([WhereItem Guid m (ExpressionU m a)], Val (InputPayload m a))
convertWhereItems usedTags expr =
  case mExtractWhere expr of
  Nothing -> return ([], expr)
  Just ewi -> do
    let
      param = ewiParam ewi
      defGuid = UniqueId.toGuid param
      defEntityId = EntityId.ofLambdaParam param
    value <-
      convertDefinitionContent defGuid
      (pure defEntityId) usedTags
      (ewiArg ewi)
    let
      mkWIActions topLevelProp bodyStored =
        ListItemActions
        { _itemDelete = do
             ConvertExpr.deleteParamRef param bodyStored
             replaceWith topLevelProp $ bodyStored ^. V.payload
        , _itemAddNext = EntityId.ofLambdaParam . fst <$> DataOps.redexWrap topLevelProp
        }
    let
      hiddenData = ewiHiddenPayloads ewi ^. Lens.traversed . ipData
      item = WhereItem
        { _wiEntityId = defEntityId
        , _wiValue =
            value
            & dBody . rPayload . plData <>~ hiddenData
        , _wiActions =
            mkWIActions <$>
            expr ^. V.payload . ipStored <*>
            traverse (^. ipStored) (ewiBody ewi)
        , _wiName = UniqueId.toGuid param
        , _wiInferredType = ewiInferredType ewi
        }
    (nextItems, whereBody) <- convertWhereItems usedTags $ ewiBody ewi
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

convertDefinitionContent ::
  (MonadA m, Monoid a) =>
  Guid -> T m EntityId -> Set T.Tag -> Val (InputPayload m a) ->
  ConvertM m (DefinitionContent Guid m (ExpressionU m a))
convertDefinitionContent defGuid jumpToDef usedTags expr =
  do
    (convParams, funcBody) <-
      convertDefinitionParams
      (ConvertM.RecordParamsInfo (UniqueId.toGuid defGuid) jumpToDef)
      usedTags expr
    ConvertM.local
      ((ConvertM.scTagParamInfos <>~ cpParamInfos convParams) .
       (ConvertM.scRecordParamsInfos <>~ cpRecordParamsInfos convParams)) $ do
        (whereItems, whereBody) <-
          convertWhereItems (usedTags <> cpTags convParams) funcBody
        bodyS <- ConvertM.convertSubexpression whereBody
        let
          setPresentationMode
            | isLengthAtLeast 2 (cpParams convParams) =
              Just $ Anchors.assocPresentationMode defGuid
            | otherwise = Nothing
        return DefinitionContent
          { _dParams = cpParams convParams
          , _dSetPresentationMode = setPresentationMode
          , _dBody =
            bodyS
            & rPayload . plData <>~
              cpHiddenPayloads convParams ^. Lens.traversed . ipData
          , _dWhereItems = whereItems
          , _dAddFirstParam = cpAddFirstParam convParams
          , _dAddInnermostWhereItem =
            fmap (EntityId.ofLambdaParam . fst) . DataOps.redexWrap $
            fromMaybe (error "Where must be stored") $
            whereBody ^. V.payload . ipStored
          }

makeExprDefTypeInfo ::
  MonadA m => ExprIRef.ValI m -> DefI m -> Definition.ExportedType -> Scheme -> DefinitionTypeInfo m
makeExprDefTypeInfo _ _ (Definition.ExportedType defType) inferredType
  | defType `Scheme.alphaEq` inferredType = DefinitionExportedTypeInfo defType
makeExprDefTypeInfo defValI defI defType inferredType =
  DefinitionNewType AcceptNewType
    { antOldType = defType
    , antNewType = inferredType
    , antAccept =
      Transaction.writeIRef defI $
      Definition.BodyExpr $
      Definition.Expr defValI $ Definition.ExportedType inferredType
    }

convert ::
  MonadA m => Anchors.CodeProps m ->
  Definition.Expr (Val (ExprIRef.ValIProperty m)) ->
  DefI m -> T m (DefinitionBody Guid m (ExpressionU m [EntityId]))
convert cp (Definition.Expr val defType) defI = do
  (valInferred, newInferContext) <-
    SugarInfer.loadInfer val
    <&> _1 . Lens.mapped %~ mkInputPayload
    & runMaybeT
    <&> fromMaybe (error "Type inference failed")
  let addStoredEntityIds x = x & ipData .~ (EntityId.ofValI . Property.value <$> x ^.. ipStored . Lens._Just)
  context <- mkContext defI cp newInferContext
  ConvertM.run context $ do
    content <-
      valInferred
      <&> addStoredEntityIds
      & convertDefinitionContent defGuid (ConvertExpr.jumpToDefI cp defI) mempty
    return $ DefinitionBodyExpression DefinitionExpression
      { _deContent = content
      , _deTypeInfo =
        makeExprDefTypeInfo exprI defI defType $
        Infer.makeScheme newInferContext $
        valInferred ^. V.payload . ipInferred . Infer.plType
      }
  where
    mkInputPayload (inferPl, stored) =
      InputPayload
      { _ipEntityId = EntityId.ofValI $ Property.value stored
      , _ipInferred = inferPl
      , _ipStored = Just stored
      , _ipData = ()
      , _ipGuid = IRef.guid $ ExprIRef.unValI $ Property.value stored
      }
    exprI = val ^. V.payload . Property.pVal
    defGuid = IRef.guid defI
