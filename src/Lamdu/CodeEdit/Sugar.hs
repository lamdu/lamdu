{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, Rank2Types, PatternGuards #-}
module Lamdu.CodeEdit.Sugar
  ( module Lamdu.CodeEdit.Sugar.Types
  , convertDefI
  , StorePoint
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens (LensLike, Lens')
import Control.Lens.Operators
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Traversable (traverse)
import Data.Typeable (Typeable1)
import Lamdu.CodeEdit.Sugar.Infer (Stored)
import Lamdu.CodeEdit.Sugar.Internal
import Lamdu.CodeEdit.Sugar.Monad (SugarM, Context(..))
import Lamdu.CodeEdit.Sugar.Types
import Lamdu.CodeEdit.Sugar.Types.Internal
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Infer.Conflicts (InferredWithConflicts(..))
import System.Random (RandomGen)
import qualified Control.Lens as Lens
import qualified Data.Cache as Cache
import qualified Data.List.Utils as ListUtils
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.CodeEdit.Sugar.Convert.Apply as Apply
import qualified Lamdu.CodeEdit.Sugar.Convert.Hole as Hole
import qualified Lamdu.CodeEdit.Sugar.Expression as SugarExpr
import qualified Lamdu.CodeEdit.Sugar.Infer as SugarInfer
import qualified Lamdu.CodeEdit.Sugar.Monad as SugarM
import qualified Lamdu.CodeEdit.Sugar.RemoveTypes as SugarRemoveTypes
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Load as Load
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Ops as DataOps

onMatchingSubexprs ::
  MonadA m => (a -> m ()) ->
  (Expr.Expression def a -> Bool) ->
  Expr.Expression def a -> m ()
onMatchingSubexprs action predicate =
  traverse_ (action . (^. Expr.ePayload)) .
  filter predicate . ExprUtil.subExpressions

toHole :: MonadA m => Stored m -> T m ()
toHole = void . DataOps.setToHole

isGetParamOf :: Guid -> Expr.Expression def a -> Bool
isGetParamOf = Lens.anyOf ExprLens.exprParameterRef . (==)

isGetFieldParam :: Guid -> Guid -> Expr.Expression def a -> Bool
isGetFieldParam param tagG =
  p . (^? ExprLens.exprGetField)
  where
    p Nothing = False
    p (Just (Expr.GetField record tag)) =
      Lens.anyOf ExprLens.exprTag (== tagG) tag &&
      Lens.anyOf ExprLens.exprParameterRef (== param) record

deleteParamRef ::
  MonadA m => Guid -> Expr.Expression def (Stored m) -> T m ()
deleteParamRef =
  onMatchingSubexprs toHole . isGetParamOf

deleteFieldParamRef ::
  MonadA m => Guid -> Guid -> Expr.Expression def (Stored m) -> T m ()
deleteFieldParamRef param tagG =
  onMatchingSubexprs toHole $ isGetFieldParam param tagG

lambdaWrap :: MonadA m => Stored m -> T m Guid
lambdaWrap stored =
  f <$> DataOps.lambdaWrap stored
  where
    f (newParam, newLamI) = Guid.combine (ExprIRef.exprGuid newLamI) newParam

fakeExample :: Guid -> ExpressionP name0 m0 (Payload name1 m1 ())
fakeExample guid =
  Expression (BodyAtom "NotImplemented") $ Payload
  { _plGuid = Guid.augment "EXAMPLE" guid
  , _plInferredTypes = []
  , _plActions = Nothing
  , _plMNextHoleGuid = Nothing
  , _plData = ()
  }

mkPositionalFuncParamActions ::
  MonadA m => Guid -> Stored m -> Expr.Expression def (Stored m) ->
  FuncParamActions MStoredName m
mkPositionalFuncParamActions param lambdaProp body =
  FuncParamActions
  { _fpListItemActions =
    ListItemActions
    { _itemDelete = do
        deleteParamRef param body
        SugarInfer.replaceWith lambdaProp $ body ^. Expr.ePayload
    , _itemAddNext = lambdaWrap $ body ^. Expr.ePayload
    }
  , _fpGetExample = pure $ fakeExample param
  }

getStoredNameS :: MonadA m => Guid -> SugarM m (Maybe String)
getStoredNameS = SugarM.liftTransaction . SugarExpr.getStoredName

addFuncParamName ::
  MonadA m => FuncParam MStoredName f expr ->
  SugarM m (FuncParam MStoredName f expr)
addFuncParamName fp = do
  name <- getStoredNameS $ fp ^. fpGuid
  pure fp { _fpName = name }

convertPositionalFuncParam ::
  (Typeable1 m, MonadA m, Monoid a) => Expr.Lambda (SugarInfer.ExprMM m a) ->
  SugarInfer.PayloadMM m a ->
  SugarM m (FuncParam MStoredName m (ExpressionU m a))
convertPositionalFuncParam (Expr.Lambda _k paramGuid paramType body) lamExprPl = do
  paramTypeS <- SugarM.convertSubexpression paramType
  addFuncParamName FuncParam
    { _fpName = Nothing
    , _fpGuid = paramGuid
    , _fpVarKind = FuncParameter
    , _fpId = Guid.combine lamGuid paramGuid
    , _fpAltIds = [paramGuid] -- For easy jumpTo
    , _fpType =
      SugarRemoveTypes.successfulType paramTypeS
      -- Slightly strange but we mappend the hidden lambda's
      -- annotation into the param type:
      & rPayload . plData <>~ lamData
    , _fpMActions =
      mkPositionalFuncParamActions paramGuid
      <$> lamExprPl ^. SugarInfer.plStored
      <*> traverse (^. SugarInfer.plStored) body
    }
  where
    lamGuid = lamExprPl ^. SugarInfer.plGuid
    lamData = lamExprPl ^. SugarInfer.plData

convertLam ::
  (MonadA m, Typeable1 m, Monoid a) =>
  Expr.Lambda (SugarInfer.ExprMM m a) ->
  SugarInfer.PayloadMM m a -> SugarM m (ExpressionU m a)
convertLam lam@(Expr.Lambda k paramGuid _paramType result) exprPl = do
  param <- convertPositionalFuncParam lam exprPl
  resultS <- SugarM.convertSubexpression result
  SugarExpr.make exprPl $ BodyLam
    Lam
    { _lParam =
        param
        & fpType %~ SugarExpr.setNextHoleToFirstSubHole resultS
        & fpMActions .~ Nothing
    , _lResultType = resultS
    , _lKind = k
    , _lIsDep = isDep
    }
  where
    isDep =
      case k of
      Val -> SugarInfer.isPolymorphicFunc exprPl
      Type -> ExprUtil.exprHasGetVar paramGuid result

convertParameterRef ::
  (MonadA m, Typeable1 m) => Guid ->
  SugarInfer.PayloadMM m a -> SugarM m (ExpressionU m a)
convertParameterRef parGuid exprPl = do
  recordParamsMap <- (^. SugarM.scRecordParamsInfos) <$> SugarM.readContext
  case Map.lookup parGuid recordParamsMap of
    Just (SugarM.RecordParamsInfo defGuid jumpTo) -> do
      defName <- getStoredNameS defGuid
      SugarExpr.make exprPl $ BodyGetParams GetParams
        { _gpDefGuid = defGuid
        , _gpDefName = defName
        , _gpJumpTo = jumpTo
        }
    Nothing -> do
      parName <- getStoredNameS parGuid
      SugarExpr.make exprPl .
        BodyGetVar $ GetVar
        { _gvName = parName
        , _gvIdentifier = parGuid
        , _gvJumpTo = pure parGuid
        , _gvVarType = GetParameter
        }

jumpToDefI ::
  MonadA m => Anchors.CodeProps m -> DefI (Tag m) -> T m Guid
jumpToDefI cp defI = IRef.guid defI <$ DataOps.newPane cp defI

convertGetVariable ::
  (MonadA m, Typeable1 m) =>
  Expr.VariableRef (DefI (Tag m)) ->
  SugarInfer.PayloadMM m a -> SugarM m (ExpressionU m a)
convertGetVariable varRef exprPl = do
  cp <- (^. SugarM.scCodeAnchors) <$> SugarM.readContext
  case varRef of
    Expr.ParameterRef parGuid -> convertParameterRef parGuid exprPl
    Expr.DefinitionRef defI -> do
      let defGuid = IRef.guid defI
      defName <- getStoredNameS defGuid
      SugarExpr.make exprPl .
        BodyGetVar $ GetVar
        { _gvName = defName
        , _gvIdentifier = defGuid
        , _gvJumpTo = jumpToDefI cp defI
        , _gvVarType = GetDefinition
        }

convertLiteralInteger ::
  (MonadA m, Typeable1 m) => Integer ->
  SugarInfer.PayloadMM m a -> SugarM m (ExpressionU m a)
convertLiteralInteger i exprPl =
  SugarExpr.make exprPl . BodyLiteralInteger $
  LiteralInteger
  { liValue = i
  , liSetValue = setValue . Property.value <$> exprPl ^. SugarInfer.plStored
  }
  where
    setValue iref val =
      ExprIRef.writeExprBody iref $
      ExprLens.bodyLiteralInteger # val

convertTag ::
  (MonadA m, Typeable1 m) => Guid ->
  SugarInfer.PayloadMM m a -> SugarM m (ExpressionU m a)
convertTag tag exprPl = do
  name <- getStoredNameS tag
  SugarExpr.make exprPl . BodyTag $ TagG tag name

convertAtom ::
  (MonadA m, Typeable1 m) => String ->
  SugarInfer.PayloadMM m a -> SugarM m (ExpressionU m a)
convertAtom str exprPl =
  SugarExpr.make exprPl $ BodyAtom str

sideChannel ::
  Monad m =>
  Lens' s a ->
  LensLike m s (side, s) a (side, a)
sideChannel lens f s = (`runStateT` s) . Lens.zoom lens $ StateT f

writeRecordFields ::
  MonadA m =>
  ExprIRef.ExpressionIM m -> result ->
  ( [(ExprIRef.ExpressionIM m, ExprIRef.ExpressionIM m)] ->
    T m
    ( result
    , [(ExprIRef.ExpressionIM m, ExprIRef.ExpressionIM m)]
    )
  ) ->
  T m result
writeRecordFields iref def f = do
  oldBody <- ExprIRef.readExprBody iref
  case oldBody ^? Expr._BodyRecord of
    Nothing -> return def
    Just oldRecord -> do
      (res, newRecord) <-
        sideChannel Expr.recordFields f oldRecord
      ExprIRef.writeExprBody iref $ Expr.BodyRecord newRecord
      return res

recordFieldActions ::
  MonadA m => Guid -> ExprIRef.ExpressionIM m -> ExprIRef.ExpressionIM m ->
  ListItemActions m
recordFieldActions defaultGuid exprIRef iref =
  ListItemActions
  { _itemDelete = action delete
  , _itemAddNext = action addNext
  }
  where
    action f = writeRecordFields iref defaultGuid $ splitFields f
    addNext (prevFields, field, nextFields) = do
      tagHole <- DataOps.newHole
      exprHole <- DataOps.newHole
      return
        ( ExprIRef.exprGuid tagHole
        , prevFields ++ field : (tagHole, exprHole) : nextFields
        )
    delete (prevFields, _, nextFields) =
      return
      ( case nextFields ++ reverse prevFields of
        [] -> defaultGuid
        ((nextTagExpr, _) : _) -> ExprIRef.exprGuid nextTagExpr
      , prevFields ++ nextFields
      )
    splitFields f oldFields =
      case break ((== exprIRef) . fst) oldFields of
      (prevFields, field : nextFields) -> f (prevFields, field, nextFields)
      _ -> return (defaultGuid, oldFields)

convertField ::
  (Typeable1 m, MonadA m, Monoid a) =>
  Maybe (ExprIRef.ExpressionIM m) -> Guid ->
  ( SugarInfer.ExprMM m a
  , SugarInfer.ExprMM m a
  ) ->
  SugarM m (RecordField m (ExpressionU m a))
convertField mIRef defaultGuid (tagExpr, expr) = do
  tagExprS <- SugarM.convertSubexpression tagExpr
  exprS <- SugarM.convertSubexpression expr
  return RecordField
    { _rfMItemActions =
      recordFieldActions defaultGuid <$> tagExpr ^? SugarInfer.exprIRef <*> mIRef
    , _rfTag = tagExprS
    , _rfExpr = exprS
    }

convertRecord ::
  (Typeable1 m, MonadA m, Monoid a) =>
  Expr.Record (SugarInfer.ExprMM m a) ->
  SugarInfer.PayloadMM m a -> SugarM m (ExpressionU m a)
convertRecord (Expr.Record k fields) exprPl = do
  sFields <- mapM (convertField (exprPl ^? SugarInfer.plIRef) defaultGuid) fields
  SugarExpr.make exprPl $ BodyRecord
    Record
    { _rKind = k
    , _rFields =
        FieldList
        { _flItems = map setTagNextHole $ withExprNextHoles sFields
        , _flMAddFirstItem = addField <$> exprPl ^? SugarInfer.plIRef
        }
    }
  where
    defaultGuid = exprPl ^. SugarInfer.plGuid
    setTagNextHole field =
      field & rfTag %~ SugarExpr.setNextHoleToFirstSubHole (field ^. rfExpr)
    withExprNextHoles (field : rest@(nextField:_)) =
      (field
       & rfExpr %~ SugarExpr.setNextHoleToFirstSubHole (nextField ^. rfExpr))
      : withExprNextHoles rest
    withExprNextHoles xs = xs
    addField iref =
      writeRecordFields iref defaultGuid $ \recordFields -> do
        holeTagExpr <- DataOps.newHole
        holeExpr <- DataOps.newHole
        return
          ( ExprIRef.exprGuid holeTagExpr
          , (holeTagExpr, holeExpr) : recordFields
          )

convertGetField ::
  (MonadA m, Typeable1 m, Monoid a) =>
  Expr.GetField (SugarInfer.ExprMM m a) ->
  SugarInfer.PayloadMM m a ->
  SugarM m (ExpressionU m a)
convertGetField (Expr.GetField recExpr tagExpr) exprPl = do
  tagParamInfos <- (^. SugarM.scTagParamInfos) <$> SugarM.readContext
  let
    mkGetVar (tag, jumpTo) = do
      name <- getStoredNameS tag
      pure GetVar
        { _gvName = name
        , _gvIdentifier = tag
        , _gvJumpTo = pure jumpTo
        , _gvVarType = GetFieldParameter
        }
  mVar <- traverse mkGetVar $ do
    tag <- tagExpr ^? ExprLens.exprTag
    paramInfo <- Map.lookup tag tagParamInfos
    param <- recExpr ^? ExprLens.exprParameterRef
    guard $ param == SugarM.tpiFromParameters paramInfo
    return (tag, SugarM.tpiJumpTo paramInfo)
  SugarExpr.make exprPl =<<
    case mVar of
    Just var ->
      return $ BodyGetVar var
    Nothing ->
      BodyGetField <$> traverse SugarM.convertSubexpression
      GetField
      { _gfRecord = recExpr
      , _gfTag = tagExpr
      }

removeRedundantSubExprTypes :: Expression n m a -> Expression n m a
removeRedundantSubExprTypes =
  rBody %~
  ( Lens.traversed %~ removeRedundantTypes
    & Lens.outside _BodyHole .~
      BodyHole . (Lens.traversed %~ removeRedundantSubExprTypes)
  ) .
  (_BodyApply . aFunc %~ remSuc) .
  (_BodyGetField . gfRecord %~ remSuc) .
  (_BodyLam . lResultType %~ remSuc) .
  (_BodyRecord %~
    (fields . rfTag %~ remSuc) .
    (Lens.filtered ((== Type) . (^. rKind)) . fields . rfExpr %~ remSuc)
  )
  where
    fields = rFields . flItems . Lens.traversed
    remSuc =
      Lens.filtered (Lens.nullOf (rBody . _BodyHole)) %~
      SugarRemoveTypes.successfulType

removeRedundantTypes :: Expression name m a -> Expression name m a
removeRedundantTypes =
  removeRedundantSubExprTypes .
  (Lens.filtered cond %~ SugarRemoveTypes.successfulType)
  where
    cond e =
      Lens.anyOf (rBody . _BodyGetVar) ((/= GetDefinition) . (^. gvVarType)) e ||
      Lens.has (rBody . _BodyRecord) e

convertExpressionI ::
  (Typeable1 m, MonadA m, Monoid a) =>
  SugarInfer.ExprMM m a -> SugarM m (ExpressionU m a)
convertExpressionI ee =
  ($ ee ^. Expr.ePayload) $
  case ee ^. Expr.eBody of
  Expr.BodyLam x -> convertLam x
  Expr.BodyApply x -> Apply.convert x
  Expr.BodyRecord x -> convertRecord x
  Expr.BodyGetField x -> convertGetField x
  Expr.BodyLeaf (Expr.GetVariable x) -> convertGetVariable x
  Expr.BodyLeaf (Expr.LiteralInteger x) -> convertLiteralInteger x
  Expr.BodyLeaf (Expr.Tag x) -> convertTag x
  Expr.BodyLeaf Expr.Hole -> Hole.convert
  Expr.BodyLeaf Expr.Set -> convertAtom "Set"
  Expr.BodyLeaf Expr.IntegerType -> convertAtom "Int"
  Expr.BodyLeaf Expr.TagType -> convertAtom "Tag"

-- Check no holes
isCompleteType :: Expr.Expression def a -> Bool
isCompleteType =
  Lens.nullOf (Lens.folding ExprUtil.subExpressions . ExprLens.exprHole)

mkContext ::
  (MonadA m, Typeable1 m) =>
  Anchors.Code (Transaction.MkProperty m) (Tag m) ->
  Cache.KeyBS ->
  Infer.Context (DefI (Tag m)) ->
  T m (Context m)
mkContext cp holeInferStateKey holeInferState = do
  specialFunctions <- Transaction.getP $ Anchors.specialFunctions cp
  return Context
    { _scHoleInferStateKey = holeInferStateKey
    , _scHoleInferState = holeInferState
    , _scCodeAnchors = cp
    , _scSpecialFunctions = specialFunctions
    , _scTagParamInfos = mempty
    , _scRecordParamsInfos = mempty
    , scConvertSubexpression = convertExpressionI
    }

convertExpressionPure ::
  (MonadA m, Typeable1 m, RandomGen g, Monoid a) =>
  Anchors.CodeProps m -> g ->
  ExprIRef.ExpressionM m a -> CT m (ExpressionU m a)
convertExpressionPure cp gen res = do
  context <-
    lift $ mkContext cp (err "holeInferStateKey")
    (err "holeInferState")
  fmap removeRedundantTypes .
    SugarM.run context .
    SugarM.convertSubexpression $
    SugarInfer.mkExprPure gen res
  where
    err x = error $ "convertExpressionPure: " ++ x

data ConventionalParams m a = ConventionalParams
  { cpTags :: [Guid]
  , cpParamInfos :: Map Guid SugarM.TagParamInfo
  , cpRecordParamsInfos :: Map Guid (SugarM.RecordParamsInfo m)
  , cpParams :: [FuncParam MStoredName m (ExpressionU m a)]
  , cpAddFirstParam :: T m Guid
  , cpHiddenPayloads :: [SugarInfer.PayloadMM m a]
  }

data FieldParam m a = FieldParam
  { fpTagGuid :: Guid
  , fpTagExpr :: SugarInfer.ExprMM m a
  , fpFieldType :: SugarInfer.ExprMM m a
  }

mkRecordParams ::
  (MonadA m, Typeable1 m, Monoid a) =>
  SugarM.RecordParamsInfo m -> Guid -> [FieldParam m a] ->
  SugarInfer.ExprMM m a ->
  Maybe (ExprIRef.ExpressionIM m) ->
  Maybe (ExprIRef.ExpressionM m (Stored m)) ->
  SugarM m (ConventionalParams m a)
mkRecordParams recordParamsInfo paramGuid fieldParams lambdaExprI mParamTypeI mBodyStored = do
  params <- traverse mkParam fieldParams
  pure ConventionalParams
    { cpTags = fpTagGuid <$> fieldParams
    , cpParamInfos = mconcat $ mkParamInfo <$> fieldParams
    , cpRecordParamsInfos = Map.singleton paramGuid recordParamsInfo
    , cpParams = params
    , cpAddFirstParam =
      addFirstFieldParam lamGuid $
      fromMaybe (error "Record param type must be stored!") mParamTypeI
    , cpHiddenPayloads = [lambdaExprI ^. Expr.ePayload]
    }
  where
    lamGuid = lambdaExprI ^. SugarInfer.exprGuid
    mLambdaP = lambdaExprI ^. Expr.ePayload . SugarInfer.plStored
    mkParamInfo fp =
      Map.singleton (fpTagGuid fp) . SugarM.TagParamInfo paramGuid .
      Guid.combine lamGuid $ fpTagGuid fp
    mkParam fp = do
      typeS <- SugarM.convertSubexpression $ fpFieldType fp
      let
        guid = fpTagGuid fp
        tagExprGuid = fpTagExpr fp ^. Expr.ePayload . SugarInfer.plGuid
      addFuncParamName FuncParam
        { _fpName = Nothing
        , _fpGuid = guid
        , _fpId = Guid.combine lamGuid guid
        , _fpAltIds = []
        , _fpVarKind = FuncFieldParameter
        , _fpType = SugarRemoveTypes.successfulType typeS
        , _fpMActions =
          fpActions tagExprGuid
          <$> mLambdaP <*> mParamTypeI <*> mBodyStored
        }
    fpActions tagExprGuid lambdaP paramTypeI bodyStored =
      FuncParamActions
      { _fpListItemActions = ListItemActions
        { _itemAddNext = addFieldParamAfter lamGuid tagExprGuid paramTypeI
        , _itemDelete =
          delFieldParam tagExprGuid paramTypeI paramGuid lambdaP bodyStored
        }
      , _fpGetExample = pure $ fakeExample tagExprGuid
      }

type ExprField m = (ExprIRef.ExpressionIM m, ExprIRef.ExpressionIM m)
rereadFieldParamTypes ::
  MonadA m =>
  Guid -> ExprIRef.ExpressionIM m ->
  ([ExprField m] -> ExprField m -> [ExprField m] -> T m Guid) ->
  T m Guid
rereadFieldParamTypes tagExprGuid paramTypeI f = do
  paramType <- ExprIRef.readExprBody paramTypeI
  let
    mBrokenFields =
      paramType ^? Expr._BodyRecord . ExprLens.kindedRecordFields Type .
      (Lens.to . break) ((tagExprGuid ==) . ExprIRef.exprGuid . fst)
  case mBrokenFields of
    Just (prevFields, theField : nextFields) -> f prevFields theField nextFields
    _ -> return tagExprGuid

rewriteFieldParamTypes ::
  MonadA m => ExprIRef.ExpressionIM m -> [ExprField m] -> T m ()
rewriteFieldParamTypes paramTypeI fields =
  ExprIRef.writeExprBody paramTypeI . Expr.BodyRecord $
  Expr.Record Type fields

addFieldParamAfter :: MonadA m => Guid -> Guid -> ExprIRef.ExpressionIM m -> T m Guid
addFieldParamAfter lamGuid tagExprGuid paramTypeI =
  rereadFieldParamTypes tagExprGuid paramTypeI $
  \prevFields theField nextFields -> do
    fieldGuid <- Transaction.newKey
    tagExprI <- ExprIRef.newExprBody $ ExprLens.bodyTag # fieldGuid
    holeTypeI <- DataOps.newHole
    rewriteFieldParamTypes paramTypeI $
      prevFields ++ theField : (tagExprI, holeTypeI) : nextFields
    pure $ Guid.combine lamGuid fieldGuid

delFieldParam ::
  MonadA m => Guid -> ExprIRef.ExpressionIM m -> Guid ->
  Stored m -> ExprIRef.ExpressionM m (Stored m) -> T m Guid
delFieldParam tagExprGuid paramTypeI paramGuid lambdaP bodyStored =
  rereadFieldParamTypes tagExprGuid paramTypeI $
  \prevFields (tagExprI, _) nextFields -> do
    tagExpr <- ExprIRef.readExprBody tagExprI
    case tagExpr ^? ExprLens.bodyTag of
      Just tagG -> deleteFieldParamRef paramGuid tagG bodyStored
      Nothing -> return ()
    case prevFields ++ nextFields of
      [] -> error "We were given fewer than 2 field params, which should never happen"
      [(fieldTagI, fieldTypeI)] -> do
        fieldTag <- ExprIRef.readExprBody fieldTagI
        let
          fieldTagGuid =
            fromMaybe (error "field params always have proper Tag expr") $
            fieldTag ^? ExprLens.bodyTag
        ExprIRef.writeExprBody (Property.value lambdaP) $
          ExprUtil.makeLambda fieldTagGuid fieldTypeI bodyI
        deleteParamRef paramGuid bodyStored
        let
          toGetParam iref =
            ExprIRef.writeExprBody iref $
            ExprLens.bodyParameterRef # fieldTagGuid
        onMatchingSubexprs (toGetParam . Property.value)
          (isGetFieldParam paramGuid fieldTagGuid) bodyStored
        pure $ Guid.combine lamGuid fieldTagGuid
      newFields -> do
        rewriteFieldParamTypes paramTypeI newFields
        dest prevFields nextFields
  where
    lamGuid = ExprIRef.exprGuid $ Property.value lambdaP
    bodyI = bodyStored ^. Expr.ePayload . Property.pVal
    dest prevFields nextFields =
      fromMaybe (ExprIRef.exprGuid bodyI) . listToMaybe <$>
      traverse (getParamGuidFromTagExprI . fst)
      (nextFields ++ reverse prevFields)
    getParamGuidFromTagExprI tagExprI = do
      tagExpr <- ExprIRef.readExprBody tagExprI
      pure . Guid.combine lamGuid .
        fromMaybe (error "field param must have tags") $
        tagExpr ^? ExprLens.bodyTag

convertDefinitionParams ::
  (MonadA m, Typeable1 m, Monoid a) =>
  SugarM.RecordParamsInfo m -> [Guid] -> SugarInfer.ExprMM m a ->
  SugarM m
  ( [FuncParam MStoredName m (ExpressionU m a)]
  , ConventionalParams m a
  , SugarInfer.ExprMM m a
  )
convertDefinitionParams recordParamsInfo usedTags expr =
  case expr ^. Expr.eBody of
  Expr.BodyLam lambda@(Expr.Lambda Val paramGuid paramType body) -> do
    param <- convertPositionalFuncParam lambda $ expr ^. Expr.ePayload
    if SugarInfer.isPolymorphicFunc $ expr ^. Expr.ePayload
      then do -- Dependent:
        (depParams, convParams, deepBody) <- convertDefinitionParams recordParamsInfo usedTags body
        return (param : depParams, convParams, deepBody)
      else -- Independent:
      case paramType ^. Expr.eBody of
      Expr.BodyRecord (Expr.Record Type fields)
        | ListUtils.isLengthAtLeast 2 fields
        , Just fieldParams <- traverse makeFieldParam fields
        , all ((`notElem` usedTags) . fpTagGuid) fieldParams -> do
          convParams <-
            mkRecordParams recordParamsInfo paramGuid fieldParams
            expr (paramType ^? SugarInfer.exprIRef)
            (traverse (^. SugarInfer.plStored) body)
          return ([], convParams, body)
      _ ->
        pure
        ( []
        , singleConventionalParam stored param paramGuid paramType body
        , body
        )
  _ -> return ([], emptyConventionalParams stored, expr)
  where
    stored =
      fromMaybe (error "Definition body is always stored!") $
      expr ^. SugarInfer.exprStored
    makeFieldParam (tagExpr, typeExpr) = do
      fieldTagGuid <- tagExpr ^? ExprLens.exprTag
      pure FieldParam
        { fpTagGuid = fieldTagGuid
        , fpTagExpr = tagExpr
        , fpFieldType = typeExpr
        }

singleConventionalParam ::
  MonadA m =>
  Stored m -> FuncParam MStoredName m (ExpressionU m a) ->
  Guid -> SugarInfer.ExprMM m a -> SugarInfer.ExprMM m a -> ConventionalParams m a
singleConventionalParam lamProp existingParam existingParamGuid existingParamType body =
  ConventionalParams
  { cpTags = []
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
    existingParamTag = ExprLens.bodyTag # existingParamGuid
    existingParamTypeIRef =
      fromMaybe (error "Only stored record param type is converted as record") $
      existingParamType ^? SugarInfer.exprIRef
    bodyWithStored =
      fromMaybe (error "Definition body should be stored") $
      traverse (^. SugarInfer.plStored) body
    addSecondParam mkFields = do
      existingParamTagI <- ExprIRef.newExprBody existingParamTag
      let existingParamField = (existingParamTagI, existingParamTypeIRef)
      (newTagGuid, newParamField) <- newField
      newParamTypeI <-
        ExprIRef.newExprBody . Expr.BodyRecord . Expr.Record Type $
        mkFields existingParamField newParamField
      newParamsGuid <- Transaction.newKey
      ExprIRef.writeExprBody (Property.value lamProp) $
        ExprUtil.makeLambda newParamsGuid newParamTypeI .
        Property.value $ bodyWithStored ^. Expr.ePayload
      let
        toGetField iref = do
          recordRef <- ExprIRef.newExprBody $ ExprLens.bodyParameterRef # newParamsGuid
          tagRef <- ExprIRef.newExprBody existingParamTag
          ExprIRef.writeExprBody iref $
            Expr.BodyGetField Expr.GetField
            { Expr._getFieldRecord = recordRef
            , Expr._getFieldTag = tagRef
            }
      onMatchingSubexprs (toGetField . Property.value)
        (isGetParamOf existingParamGuid) bodyWithStored
      let lamGuid = ExprIRef.exprGuid $ Property.value lamProp
      pure $ Guid.combine lamGuid newTagGuid

emptyConventionalParams :: MonadA m => ExprIRef.ExpressionProperty m -> ConventionalParams m a
emptyConventionalParams stored = ConventionalParams
  { cpTags = []
  , cpParamInfos = Map.empty
  , cpRecordParamsInfos = Map.empty
  , cpParams = []
  , cpAddFirstParam = lambdaWrap stored
  , cpHiddenPayloads = []
  }

data ExprWhereItem def a = ExprWhereItem
  { ewiBody :: Expr.Expression def a
  , ewiParamGuid :: Guid
  , ewiArg :: Expr.Expression def a
  , ewiHiddenPayloads :: [a]
  }

mExtractWhere :: Expr.Expression def a -> Maybe (ExprWhereItem def a)
mExtractWhere expr = do
  Expr.Apply func arg <- expr ^? ExprLens.exprApply
  (paramGuid, paramType, body) <- func ^? ExprLens.exprKindedLam Val
  -- paramType has to be Hole for this to be sugarred to Where
  paramType ^? ExprLens.exprHole
  Just ExprWhereItem
    { ewiBody = body
    , ewiParamGuid = paramGuid
    , ewiArg = arg
    , ewiHiddenPayloads = (^. Expr.ePayload) <$> [expr, func, paramType]
    }

convertWhereItems ::
  (MonadA m, Typeable1 m, Monoid a) =>
  [Guid] ->
  SugarInfer.ExprMM m a ->
  SugarM m ([WhereItem MStoredName m (ExpressionU m a)], SugarInfer.ExprMM m a)
convertWhereItems usedTags expr =
  case mExtractWhere expr of
  Nothing -> return ([], expr)
  Just ewi -> do
    let
      defGuid = ewiParamGuid ewi
      recordParamsInfo =
        SugarM.RecordParamsInfo defGuid $ pure defGuid
    value <-
      convertDefinitionContent recordParamsInfo usedTags $
      ewiArg ewi
    let
      mkWIActions topLevelProp bodyStored =
        ListItemActions
        { _itemDelete = do
             deleteParamRef defGuid bodyStored
             SugarInfer.replaceWith topLevelProp $ bodyStored ^. Expr.ePayload
        , _itemAddNext = fmap fst $ DataOps.redexWrap topLevelProp
        }
    name <- getStoredNameS defGuid
    let
      hiddenData = ewiHiddenPayloads ewi ^. Lens.traversed . SugarInfer.plData
      item = WhereItem
        { wiValue =
          value
          & dBody . rPayload . plData <>~ hiddenData
        , wiGuid = defGuid
        , wiActions =
          mkWIActions <$>
          expr ^. SugarInfer.exprStored <*>
          traverse (^. SugarInfer.plStored) (ewiBody ewi)
        , wiName = name
        }
    (nextItems, whereBody) <- convertWhereItems usedTags $ ewiBody ewi
    return (item : nextItems, whereBody)

newField ::
  MonadA m => T m (Guid, (ExprIRef.ExpressionIM m, ExprIRef.ExpressionIM m))
newField = do
  tag <- Transaction.newKey
  newTagI <- ExprIRef.newExprBody (ExprLens.bodyTag # tag)
  holeI <- DataOps.newHole
  return (tag, (newTagI, holeI))

addFirstFieldParam :: MonadA m => Guid -> ExprIRef.ExpressionIM m -> T m Guid
addFirstFieldParam lamGuid recordI = do
  recordBody <- ExprIRef.readExprBody recordI
  case recordBody ^? Expr._BodyRecord . ExprLens.kindedRecordFields Type of
    Just fields -> do
      (newTagGuid, field) <- newField
      ExprIRef.writeExprBody recordI $
        Expr.BodyRecord . Expr.Record Type $ field : fields
      pure $ Guid.combine lamGuid newTagGuid
    _ -> pure $ ExprIRef.exprGuid recordI

assertedGetProp ::
  String ->
  Expr.Expression def (SugarInfer.Payload inferred (Maybe stored) a) -> stored
assertedGetProp _
  Expr.Expression
  { Expr._ePayload =
    SugarInfer.Payload { SugarInfer._plStored = Just prop } } = prop
assertedGetProp msg _ = error msg

convertDefinitionContent ::
  (MonadA m, Typeable1 m, Monoid a) =>
  SugarM.RecordParamsInfo m -> [Guid] -> SugarInfer.ExprMM m a ->
  SugarM m (DefinitionContent MStoredName m (ExpressionU m a))
convertDefinitionContent recordParamsInfo usedTags expr = do
  (depParams, convParams, funcBody) <-
    convertDefinitionParams recordParamsInfo usedTags expr
  SugarM.local
    ((SugarM.scTagParamInfos <>~ cpParamInfos convParams) .
     (SugarM.scRecordParamsInfos <>~ cpRecordParamsInfos convParams)) $ do
      (whereItems, whereBody) <-
        convertWhereItems (usedTags ++ cpTags convParams) funcBody
      bodyS <- SugarM.convertSubexpression whereBody
      return DefinitionContent
        { _dDepParams = depParams
        , _dParams = cpParams convParams
        , _dBody =
          bodyS
          & rPayload . plData <>~
            cpHiddenPayloads convParams ^. Lens.traversed . SugarInfer.plData
        , _dWhereItems = whereItems
        , _dAddFirstParam = cpAddFirstParam convParams
        , _dAddInnermostWhereItem =
          fmap fst . DataOps.redexWrap $
          assertedGetProp "Where must be stored" whereBody
        }

convertDefIBuiltin ::
  (Typeable1 m, MonadA m) => Anchors.CodeProps m ->
  Definition.Builtin -> DefI (Tag m) ->
  ExprIRef.ExpressionM m (Stored m) ->
  CT m (DefinitionBody MStoredName m (ExpressionU m [Guid]))
convertDefIBuiltin cp (Definition.Builtin name) defI defType =
  DefinitionBodyBuiltin <$> do
    defTypeS <-
      convertExpressionPure cp iDefTypeGen (void defType)
      <&> Lens.mapped . Lens.mapped .~ mempty
    pure DefinitionBuiltin
      { biName = name
      , biMSetName = Just setName
      , biType = defTypeS
      }
  where
    defGuid = IRef.guid defI
    iDefTypeGen = SugarExpr.mkGen 1 3 defGuid
    typeIRef = Property.value (defType ^. Expr.ePayload)
    setName =
      Transaction.writeIRef defI .
      (`Definition.Definition` typeIRef) .
      Definition.BodyBuiltin . Definition.Builtin

makeTypeInfo ::
  (Typeable1 m, MonadA m, Monoid a) =>
  Anchors.CodeProps m -> DefI (Tag m) ->
  ExprIRef.ExpressionM m (Stored m, a) ->
  ExprIRef.ExpressionM m a -> Bool ->
  CT m (DefinitionTypeInfo m (ExpressionU m a))
makeTypeInfo cp defI defType inferredType success = do
  defTypeS <- conv iDefTypeGen $ snd <$> defType
  case () of
    ()
      | not success || not (isCompleteType inferredType) ->
        DefinitionIncompleteType <$> do
          inferredTypeS <- conv iNewTypeGen inferredType
          pure ShowIncompleteType
            { sitOldType = defTypeS
            , sitNewIncompleteType = inferredTypeS
            }
      | typesMatch -> pure $ DefinitionExportedTypeInfo defTypeS
      | otherwise ->
        DefinitionNewType <$> do
          inferredTypeS <- conv iNewTypeGen inferredType
          pure AcceptNewType
            { antOldType = defTypeS
            , antNewType = inferredTypeS
            , antAccept =
              Property.set (defType ^. Expr.ePayload . Lens._1) =<<
              ExprIRef.newExpression (void inferredType)
            }
  where
    conv = convertExpressionPure cp
    iDefTypeGen = SugarExpr.mkGen 1 3 defGuid
    iNewTypeGen = SugarExpr.mkGen 2 3 defGuid
    typesMatch = (snd <$> defType) `ExprUtil.alphaEq` inferredType
    defGuid = IRef.guid defI

convertDefIExpression ::
  (MonadA m, Typeable1 m) => Anchors.CodeProps m ->
  ExprIRef.ExpressionM m (Load.ExprPropertyClosure (Tag m)) ->
  DefI (Tag m) ->
  ExprIRef.ExpressionM m (Stored m) ->
  CT m (DefinitionBody MStoredName m (ExpressionU m [Guid]))
convertDefIExpression cp exprLoaded defI defType = do
  ilr@SugarInfer.InferredWithImplicits
    { SugarInfer._iwiExpr = iwiExpr
    , SugarInfer._iwiSuccess = success
    } <-
    SugarInfer.inferAddImplicits
    -- We can use empty string because we always use
    -- initialInferState, so any key is a unique key
    inferLoadedGen (Just defI) exprLoaded "" initialInfer
  let
    inferredType =
      void . Infer.iType . iwcInferred $ iwiExpr ^. SugarInfer.exprInferred
    addStoredGuids lens x = (x, ExprIRef.exprGuid . Property.value <$> x ^.. lens)
    guidIntoPl (pl, x) = pl & SugarInfer.plData %~ \() -> x
  typeInfo <-
    makeTypeInfo cp defI (addStoredGuids id <$> defType) (mempty <$ inferredType) success
  let
    holeInferStateKey = ilr ^. SugarInfer.iwiBaseInferContextKey
    holeInferState = ilr ^. SugarInfer.iwiBaseInferContext
  context <- lift $ mkContext cp holeInferStateKey holeInferState
  SugarM.run context $ do
    content <-
      iwiExpr
      <&> guidIntoPl . addStoredGuids (SugarInfer.plStored . Lens._Just)
      & traverse . SugarInfer.plInferred %~ Just
      & convertDefinitionContent recordParamsInfo []
    return $ DefinitionBodyExpression DefinitionExpression
      { _deContent = removeRedundantTypes <$> content
      , _deTypeInfo = typeInfo
      }
  where
    initialInfer = Infer.initial (Just defI)
    defGuid = IRef.guid defI
    recordParamsInfo = SugarM.RecordParamsInfo defGuid $ jumpToDefI cp defI
    inferLoadedGen = SugarExpr.mkGen 0 3 defGuid

convertDefI ::
  (MonadA m, Typeable1 m) =>
  Anchors.CodeProps m -> DefI (Tag m) ->
  -- TODO: Use DefinitionClosure?
  Definition.Definition (ExprIRef.ExpressionM m (Load.ExprPropertyClosure (Tag m))) ->
  CT m (Definition (Maybe String) m (ExpressionU m [Guid]))
convertDefI cp defI (Definition.Definition defBody typeLoaded) = do
  bodyS <- convertDefBody defBody $ Load.exprPropertyOfClosure <$> typeLoaded
  name <- lift $ SugarExpr.getStoredName defGuid
  return Definition
    { _drGuid = defGuid
    , _drName = name
    , _drBody = bodyS
    }
  where
    defGuid = IRef.guid defI
    convertDefBody (Definition.BodyBuiltin builtin) =
      convertDefIBuiltin cp builtin defI
    convertDefBody (Definition.BodyExpression exprLoaded) =
      convertDefIExpression cp exprLoaded defI
