{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types, PatternGuards #-}
module Lamdu.Sugar.Convert
  ( convertDefI
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import Control.MonadA (MonadA)
import Data.List.Utils (isLengthAtLeast)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..), (<>))
import Data.Set (Set)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Lamdu.Expr.FlatComposite (FlatComposite(..))
import Lamdu.Expr.IRef (DefIM)
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
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.Expr.Scheme as Scheme
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Apply as ConvertApply
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Hole as ConvertHole
import qualified Lamdu.Sugar.Convert.Infer as SugarInfer
import qualified Lamdu.Sugar.Convert.List as ConvertList
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId

type T = Transaction

onMatchingSubexprs ::
  MonadA m => (a -> m ()) -> (a -> Val () -> Bool) -> Val a -> m ()
onMatchingSubexprs action predicate =
  Lens.itraverseOf_ (ExprLens.subExprPayloads . Lens.ifiltered (flip predicate))
  (const action)

toHole :: MonadA m => ExprIRef.ValIProperty m -> T m ()
toHole = void . DataOps.setToHole

isGetParamOf :: V.Var -> Val a -> Bool
isGetParamOf = Lens.anyOf ExprLens.valVar . (==)

isGetFieldParam :: V.Var -> T.Tag -> Val a -> Bool
isGetFieldParam expectedRecordVar expectedTag =
  p . (^? ExprLens.valGetField)
  where
    p Nothing = False
    p (Just (V.GetField record tag)) =
      expectedTag == tag &&
      Lens.anyOf ExprLens.valVar (== expectedRecordVar) record

deleteParamRef ::
  MonadA m => V.Var -> Val (ExprIRef.ValIProperty m) -> T m ()
deleteParamRef = onMatchingSubexprs toHole . const . isGetParamOf

_deleteFieldParamRef ::
  MonadA m => V.Var -> T.Tag -> Val (ExprIRef.ValIProperty m) -> T m ()
_deleteFieldParamRef param tag =
  onMatchingSubexprs toHole . const $ isGetFieldParam param tag

lambdaWrap :: MonadA m => ExprIRef.ValIProperty m -> T m EntityId
lambdaWrap stored =
  f <$> DataOps.lambdaWrap stored
  where
    f (newParam, _) = EntityId.ofLambdaParam newParam

mkPositionalFuncParamActions ::
  MonadA m => V.Var -> ExprIRef.ValIProperty m -> Val (ExprIRef.ValIProperty m) -> FuncParamActions m
mkPositionalFuncParamActions param lambdaProp body =
  FuncParamActions
  { _fpListItemActions =
    ListItemActions
    { _itemDelete = do
        deleteParamRef param body
        replaceWith lambdaProp $ body ^. V.payload
    , _itemAddNext = lambdaWrap $ body ^. V.payload
    }
  }

makeStoredNamePropertyS ::
  (UniqueId.ToGuid s, MonadA m) =>
  s -> ConvertM m (NameProperty MStoredName m)
makeStoredNamePropertyS x = ConvertM.liftTransaction . ConvertExpr.makeStoredNameProperty $ x

convertPositionalFuncParam ::
  (MonadA m, Monoid a) => V.Lam (Val (InputPayload m a)) ->
  InputPayload m a ->
  ConvertM m (FuncParam MStoredName m)
convertPositionalFuncParam (V.Lam param body) lamExprPl = do
  name <- makeStoredNamePropertyS param
  pure FuncParam
    { _fpName = name
    , _fpVarKind = FuncParameter
    , _fpId = paramEntityId
    , _fpInferredType = paramType
    , _fpMActions =
      mkPositionalFuncParamActions param
      <$> lamExprPl ^. ipStored
      <*> traverse (^. ipStored) body
    }
  where
    paramEntityId = EntityId.ofLambdaParam param
    paramType =
      fromMaybe (error "Lambda value not inferred to a function type?!") $
      lamExprPl ^? ipInferred . Infer.plType . ExprLens._TFun . _1

convertLam ::
  (MonadA m, Monoid a) =>
  V.Lam (Val (InputPayload m a)) ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertLam lam@(V.Lam paramVar result) exprPl = do
  param <- convertPositionalFuncParam lam exprPl
  resultS <- ConvertM.convertSubexpression result
  let
    setToInnerExprAction =
      maybe NoInnerExpr SetToInnerExpr $ do
        guard $ Lens.nullOf ExprLens.valHole result
        bodyStored <- traverse (^. ipStored) result
        stored <- exprPl ^. ipStored
        return $ do
          deleteParamRef paramVar bodyStored
          EntityId.ofValI <$>
            DataOps.setToWrapper (Property.value (bodyStored ^. V.payload)) stored
  BodyLam
    Lam
    { _lParam =
        param
        & fpMActions .~ Nothing
    , _lResult = resultS
    }
    & ConvertExpr.make exprPl
    <&> rPayload . plActions . Lens._Just . setToInnerExpr .~ setToInnerExprAction

convertVar ::
  MonadA m => V.Var ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertVar param exprPl = do
  recordParamsMap <- (^. ConvertM.scRecordParamsInfos) <$> ConvertM.readContext
  case Map.lookup param recordParamsMap of
    Just (ConvertM.RecordParamsInfo defName jumpTo) ->
      ConvertExpr.make exprPl $ BodyGetParams GetParams
      { _gpDefName = defName
      , _gpJumpTo = jumpTo
      }
    Nothing ->
      do
        parName <- makeStoredNamePropertyS param
        ConvertExpr.make exprPl .
          BodyGetVar $ GetVar
          { _gvName = parName
          , _gvJumpTo = pure $ EntityId.ofLambdaParam param
          , _gvVarType = GetParameter
          }

jumpToDefI ::
  MonadA m => Anchors.CodeProps m -> DefIM m -> T m EntityId
jumpToDefI cp defI = EntityId.ofIRef defI <$ DataOps.newPane cp defI

convertVLiteralInteger ::
  MonadA m => Integer ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertVLiteralInteger i exprPl = ConvertExpr.make exprPl $ BodyLiteralInteger i

convertTag :: MonadA m => EntityId -> T.Tag -> ConvertM m (TagG MStoredName m)
convertTag inst tag = TagG inst tag <$> makeStoredNamePropertyS tag

convertField ::
  (MonadA m, Monoid a) => Maybe (ExprIRef.ValIM m) ->
  EntityId -> T.Tag -> Val (InputPayload m a) ->
  ConvertM m (RecordField MStoredName m (ExpressionU m a))
convertField _mIRef inst tag expr = do
  tagS <- convertTag inst tag
  exprS <- ConvertM.convertSubexpression expr
  return RecordField
    { _rfMDelete = return $ error "TODO: _rfMDelete"
    , _rfTag = tagS
    , _rfExpr = exprS
    }

plIRef :: Lens.Traversal' (InputPayload m a) (ExprIRef.ValIM m)
plIRef = ipStored . Lens._Just . Property.pVal

convertEmptyRecord :: MonadA m => InputPayload m a -> ConvertM m (ExpressionU m a)
convertEmptyRecord exprPl =
  ConvertExpr.make exprPl $
  BodyRecord Record
  { _rItems = []
  , _rMAddField = return $ error "TODO: _rMAddField" -- addField <$> exprPl ^? plIRef
  }

convertRecExtend ::
  (MonadA m, Monoid a) => V.RecExtend (Val (InputPayload m a)) ->
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertRecExtend (V.RecExtend tag val rest) exprPl = do
  restS <- ConvertM.convertSubexpression rest
  fieldS <-
      convertField (exprPl ^? plIRef)
      (EntityId.ofRecExtendTag (exprPl ^. ipEntityId)) tag val
  case restS ^. rBody of
    BodyRecord (Record restFields _mAddFirstAddItem) ->
      ConvertExpr.make exprPl $ BodyRecord $
        Record (fieldS : restFields) $
        return $ error "TODO: Support add first item on records"
    _ -> return $ error "TODO: Support record extend of non-record"
  where
    -- addField iref =
    --   writeRecordFields iref defaultEntityId $ \recordFields -> do
    --     tag <- T.Tag <$> Transaction.newKey
    --     holeExpr <- DataOps.newHole
    --     return
    --       ( ExprIRef.valIEntityId holeTagExpr
    --       , (tag, holeExpr) : recordFields
    --       )

convertGetField ::
  (MonadA m, Monoid a) =>
  V.GetField (Val (InputPayload m a)) ->
  InputPayload m a ->
  ConvertM m (ExpressionU m a)
convertGetField (V.GetField recExpr tag) exprPl = do
  tagParamInfos <- (^. ConvertM.scTagParamInfos) <$> ConvertM.readContext
  let
    mkGetVar jumpTo = do
      name <- makeStoredNamePropertyS tag
      pure GetVar
        { _gvName = name
        , _gvJumpTo = pure jumpTo
        , _gvVarType = GetFieldParameter
        }
  mVar <- traverse mkGetVar $ do
    paramInfo <- Map.lookup tag tagParamInfos
    param <- recExpr ^? ExprLens.valVar
    guard $ param == ConvertM.tpiFromParameters paramInfo
    return $ ConvertM.tpiJumpTo paramInfo
  ConvertExpr.make exprPl =<<
    case mVar of
    Just var ->
      return $ BodyGetVar var
    Nothing -> do
      tName <- makeStoredNamePropertyS tag
      traverse ConvertM.convertSubexpression
        GetField
        { _gfRecord = recExpr
        , _gfTag =
            TagG
            { _tagInstance = EntityId.ofGetFieldTag (exprPl ^. ipEntityId)
            , _tagVal = tag
            , _tagGName = tName
            }
        }
        <&> BodyGetField

convertGlobal ::
  MonadA m => V.GlobalId -> InputPayload m a -> ConvertM m (ExpressionU m a)
convertGlobal globalId exprPl =
  runMatcherT $ do
    justToLeft $ ConvertList.nil globalId exprPl
    lift $ do
      cp <- (^. ConvertM.scCodeAnchors) <$> ConvertM.readContext
      defName <- makeStoredNamePropertyS defI
      ConvertExpr.make exprPl .
        BodyGetVar $ GetVar
        { _gvName = defName
        , _gvJumpTo = jumpToDefI cp defI
        , _gvVarType = GetDefinition
        }
    where
      defI = ExprIRef.defI globalId

convertExpressionI :: (MonadA m, Monoid a) => Val (InputPayload m a) -> ConvertM m (ExpressionU m a)
convertExpressionI ee =
  ($ ee ^. V.payload) $
  case ee ^. V.body of
  V.BAbs x -> convertLam x
  V.BApp x -> ConvertApply.convert x
  V.BRecExtend x -> convertRecExtend x
  V.BGetField x -> convertGetField x
  V.BLeaf (V.LVar x) -> convertVar x
  V.BLeaf (V.LGlobal x) -> convertGlobal x
  V.BLeaf (V.LLiteralInteger x) -> convertVLiteralInteger x
  V.BLeaf V.LHole -> ConvertHole.convert
  V.BLeaf V.LRecEmpty -> convertEmptyRecord

mkContext ::
  MonadA m =>
  Anchors.Code (Transaction.MkProperty m) (Tag m) ->
  Infer.Context -> T m (Context m)
mkContext cp inferContext = do
  specialFunctions <- Transaction.getP $ Anchors.specialFunctions cp
  return Context
    { _scInferContext = inferContext
    , _scCodeAnchors = cp
    , _scSpecialFunctions = specialFunctions
    , _scTagParamInfos = mempty
    , _scRecordParamsInfos = mempty
    , scConvertSubexpression = convertExpressionI
    }

data ConventionalParams m a = ConventionalParams
  { cpTags :: Set T.Tag
  , cpParamInfos :: Map T.Tag ConvertM.TagParamInfo
  , cpRecordParamsInfos :: Map V.Var (ConvertM.RecordParamsInfo m)
  , cpParams :: [FuncParam MStoredName m]
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
    mkParam fp = do
      name <- makeStoredNamePropertyS $ fpTag fp
      pure FuncParam
        { _fpName = name
        , _fpId = fpIdEntityId fp
        , _fpVarKind = FuncFieldParameter
        , _fpInferredType = fpFieldType fp
        , _fpMActions = error "TODO: _fpMActions"
          -- fpActions (fpIdEntityId fp)
          -- <$> mLambdaP <*> mParamTypeI <*> mBodyStored
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

-- type ExprField m = (T.Tag, ExprIRef.ValIM m)
-- rereadFieldParamTypes ::
--   MonadA m =>
--   Guid -> ExprIRef.ValIM m ->
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
--   MonadA m => ExprIRef.ValIM m -> [ExprField m] -> T m ()
-- rewriteFieldParamTypes paramTypeI fields =
--   ExprIRef.writeExprBody paramTypeI . V.VRec $
--   V.Record KType fields

-- addFieldParamAfter :: MonadA m => Guid -> Guid -> ExprIRef.ValIM m -> T m Guid
-- addFieldParamAfter lamGuid tagExprGuid paramTypeI =
--   rereadFieldParamTypes tagExprGuid paramTypeI $
--   \prevFields theField nextFields -> do
--     fieldGuid <- Transaction.newKey
--     holeTypeI <- DataOps.newHole
--     rewriteFieldParamTypes paramTypeI $
--       prevFields ++ theField : (T.Tag fieldGuid, holeTypeI) : nextFields
--     pure $ Guid.combine lamGuid fieldGuid

-- delFieldParam ::
--   MonadA m => Guid -> ExprIRef.ValIM m -> Guid ->
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
    param <- convertPositionalFuncParam lambda (expr ^. V.payload)
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
  ExprIRef.ValIProperty m -> FuncParam MStoredName m ->
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
  ConvertM m ([WhereItem MStoredName m (ExpressionU m a)], Val (InputPayload m a))
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
             deleteParamRef param bodyStored
             replaceWith topLevelProp $ bodyStored ^. V.payload
        , _itemAddNext = EntityId.ofLambdaParam . fst <$> DataOps.redexWrap topLevelProp
        }
    name <- makeStoredNamePropertyS param
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
        , _wiName = name
        , _wiInferredType = ewiInferredType ewi
        }
    (nextItems, whereBody) <- convertWhereItems usedTags $ ewiBody ewi
    return (item : nextItems, whereBody)

-- addFirstFieldParam :: MonadA m => Guid -> ExprIRef.ValIM m -> T m Guid
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
  ConvertM m (DefinitionContent MStoredName m (ExpressionU m a))
convertDefinitionContent defGuid jumpToDef usedTags expr =
  do
    defName <- makeStoredNamePropertyS defGuid
    (convParams, funcBody) <-
      convertDefinitionParams
      (ConvertM.RecordParamsInfo defName jumpToDef)
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

convertDefIBuiltin ::
  MonadA m => Definition.Builtin -> DefIM m -> Definition.ExportedType ->
  DefinitionBody MStoredName m (ExpressionU m [EntityId])
convertDefIBuiltin (Definition.Builtin name) defI defType =
  DefinitionBodyBuiltin DefinitionBuiltin
    { biName = name
    , biSetName = setName
    , biType = defType
    }
  where
    setName =
      Transaction.writeIRef defI . (`Definition.Body` defType) .
      Definition.ContentBuiltin . Definition.Builtin

makeExprDefTypeInfo ::
  MonadA m => ExprIRef.ValIM m -> DefIM m -> Definition.ExportedType -> Scheme -> DefinitionTypeInfo m
makeExprDefTypeInfo _ _ (Definition.ExportedType defType) inferredType
  | defType `Scheme.alphaEq` inferredType = DefinitionExportedTypeInfo defType
makeExprDefTypeInfo defValI defI defType inferredType =
  DefinitionNewType AcceptNewType
    { antOldType = defType
    , antNewType = inferredType
    , antAccept =
      Transaction.writeIRef defI $
      Definition.Body (Definition.ContentExpr defValI) $
      Definition.ExportedType inferredType
    }

convertDefIExpr ::
  MonadA m => Anchors.CodeProps m ->
  Val (Load.ExprPropertyClosure (Tag m)) ->
  DefIM m -> Definition.ExportedType ->
  T m (DefinitionBody MStoredName m (ExpressionU m [EntityId]))
convertDefIExpr cp valLoaded defI defType = do
  (valInferred, newInferContext) <- SugarInfer.loadInfer valIRefs
  let addStoredEntityIds x = x & ipData .~ (EntityId.ofValI . Property.value <$> x ^.. ipStored . Lens._Just)
  context <- mkContext cp newInferContext
  ConvertM.run context $ do
    content <-
      valInferred
      <&> addStoredEntityIds
      & convertDefinitionContent defGuid (jumpToDefI cp defI) mempty
    return $ DefinitionBodyExpression DefinitionExpression
      { _deContent = content
      , _deTypeInfo =
        makeExprDefTypeInfo exprI defI defType $
        Infer.makeScheme newInferContext $
        valInferred ^. V.payload . ipInferred . Infer.plType
      }
  where
    valIRefs = valLoaded <&> Load.exprPropertyOfClosure
    exprI = valIRefs ^. V.payload . Property.pVal
    defGuid = IRef.guid defI

convertDefI ::
  MonadA m =>
  Anchors.CodeProps m ->
  -- TODO: Use DefinitionClosure?
  Definition.Definition
  (Val (Load.ExprPropertyClosure (Tag m))) (DefIM m) ->
  T m (Definition MStoredName m (ExpressionU m [EntityId]))
convertDefI cp (Definition.Definition (Definition.Body bodyContent exportedType) defI) = do
  bodyS <- convertDefContent bodyContent exportedType
  name <- ConvertExpr.makeStoredNameProperty defI
  return Definition
    { _drEntityId = EntityId.ofIRef defI
    , _drName = name
    , _drBody = bodyS
    }
  where
    convertDefContent (Definition.ContentBuiltin builtin) =
      return . convertDefIBuiltin builtin defI
    convertDefContent (Definition.ContentExpr valLoaded) =
      convertDefIExpr cp valLoaded defI
