{-# LANGUAGE FlexibleContexts, OverloadedStrings, ConstraintKinds, TypeFamilies, Rank2Types, PatternGuards #-}
module Lamdu.CodeEdit.Sugar
  ( Definition(..), drName, drGuid, drBody
  , DefinitionBody(..)
  , ListItemActions(..), itemAddNext, itemDelete
  , FuncParamActions(..), fpListItemActions, fpGetExample
  , DefinitionExpression(..), deContent, deTypeInfo
  , ShowIncompleteType(..), AcceptNewType(..)
  , DefinitionTypeInfo(..)
    , _DefinitionNoTypeInfo
    , _DefinitionIncompleteType
    , _DefinitionNewType
  , DefinitionContent(..)
  , DefinitionBuiltin(..)
  , Actions(..)
    , giveAsArg, callWithArg, callWithNextArg
    , setToHole, replaceWithNewHole, cut
  , Body(..)
    , _BodyLam, _BodyApply, _BodyGetVar, _BodyHole
    , _BodyInferred, _BodyCollapsed, _BodyLiteralInteger
    , _BodyAtom, _BodyList, _BodyRecord, _BodyTag
  , Payload(..), plInferredTypes, plActions, plNextHole
  , ExpressionP(..)
    , rGuid, rBody, rPayload, rHiddenGuids, rPresugaredExpression
  , NameSource(..), NameCollision(..), Name(..), MStoredName
  , DefinitionN, DefinitionU
  , Expression, ExpressionN
  , BodyN
  , WhereItem(..)
  , ListItem(..), ListActions(..), List(..)
  , RecordField(..), rfMItemActions, rfTag, rfExpr
  , Kind(..), Record(..), FieldList(..), GetField(..)
  , GetVarType(..)
  , GetVar(..), gvIdentifier, gvName, gvJumpTo, gvVarType
  , GetParams(..), gpDefGuid, gpDefName, gpJumpTo
  , SpecialArgs(..), _NoSpecialArgs, _ObjectArg, _InfixArgs
  , Apply(..), aFunc, aSpecialArgs, aAnnotatedArgs
  , Lam(..), lKind, lParam, lIsDep, lResultType
  , FuncParamType(..)
  , FuncParam(..), fpName, fpGuid, fpId, fpAltIds, fpVarKind, fpHiddenLambdaGuid, fpType, fpMActions
  , Hole(..), holeScope, holeMActions
  , HoleResultSeed(..), _ResultSeedExpression, _ResultSeedNewTag, _ResultSeedNewDefinition
  , ScopeItem
  , Scope(..), scopeLocals, scopeGlobals, scopeTags, scopeGetParams
  , HoleActions(..), holePaste, holeMDelete, holeResult, holeInferExprType, holeInferredType
  , StorePoint
  , HoleResult(..)
    , holeResultInferred
    , holeResultConverted
    , holeResultPick, holeResultPickPrefix
  , LiteralInteger(..)
  , TagG(..), tagName, tagGuid
  , Inferred(..), iValue, iMAccept, iHole
  , Collapsed(..), cFuncGuid, cCompact, cFullExpression
  , loadConvertDefI
  , PrefixAction, emptyPrefixAction
  , SugarExpr.removeTypes
  , Hole.holeResultHasHoles
  , ExprStorePoint
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens (LensLike, Lens')
import Control.Lens.Operators
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe, isJust)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Traversable (traverse)
import Data.Typeable (Typeable1)
import Lamdu.CodeEdit.Sugar.Infer (Stored)
import Lamdu.CodeEdit.Sugar.Monad (SugarM)
import Lamdu.CodeEdit.Sugar.Types
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
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Load as Load
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Ops as DataOps

type Convertor m = SugarInfer.ExprMM m -> SugarM m (ExpressionU m)

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

fakeExample :: Guid -> ExpressionP name0 m (Payload name1 f)
fakeExample guid =
  Expression
  { _rGuid = Guid.augment "EXAMPLE" guid
  , _rBody = BodyAtom "NotImplemented"
  , _rPayload = Payload [] Nothing Nothing
  , _rHiddenGuids = []
  , _rPresugaredExpression = Nothing <$ ExprUtil.pureHole
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
  (Typeable1 m, MonadA m) => Expr.Lambda (SugarInfer.ExprMM m) ->
  SugarInfer.ExprMM m ->
  SugarM m (FuncParam MStoredName m (ExpressionU m))
convertPositionalFuncParam (Expr.Lambda _k paramGuid paramType body) lamExprI = do
  paramTypeS <- SugarM.convertSubexpression paramType
  addFuncParamName FuncParam
    { _fpName = Nothing
    , _fpGuid = paramGuid
    , _fpVarKind = FuncParameter
    , _fpId = Guid.combine lamGuid paramGuid
    , _fpAltIds = [paramGuid] -- For easy jumpTo
    , _fpHiddenLambdaGuid = Just $ SugarInfer.resultGuid lamExprI
    , _fpType = SugarExpr.removeSuccessfulType paramTypeS
    , _fpMActions =
      mkPositionalFuncParamActions paramGuid
      <$> lamExprI ^. Expr.ePayload . SugarInfer.plStored
      <*> traverse (^. SugarInfer.plStored) body
    }
  where
    lamGuid = SugarInfer.resultGuid lamExprI

convertPositionalLambda ::
  (Typeable1 m, MonadA m) => Expr.Lambda (SugarInfer.ExprMM m) ->
  SugarInfer.ExprMM m ->
  SugarM m (FuncParam MStoredName m (ExpressionU m), ExpressionU m)
convertPositionalLambda lam lamExprI = do
  param <- convertPositionalFuncParam lam lamExprI
  result <- SugarM.convertSubexpression (lam ^. Expr.lambdaResult)
  return (param & fpType %~ SugarExpr.setNextHole result, result)

convertLam :: (MonadA m, Typeable1 m) => Expr.Lambda (SugarInfer.ExprMM m) -> Convertor m
convertLam lambda@(Expr.Lambda k paramGuid _paramType result) exprI = do
  (param, sBody) <- convertPositionalLambda lambda exprI
  SugarExpr.make exprI $ BodyLam
    Lam
    { _lParam = param
    , _lResultType = SugarExpr.removeSuccessfulType sBody
    , _lKind = k
    , _lIsDep = isDep
    }
  where
    isDep =
      case k of
      Val -> SugarInfer.isPolymorphicFunc exprI
      Type -> ExprUtil.exprHasGetVar paramGuid result

convertParameterRef :: (MonadA m, Typeable1 m) => Guid -> Convertor m
convertParameterRef parGuid exprI = do
  recordParamsMap <- (^. SugarM.scRecordParamsInfos) <$> SugarM.readContext
  case Map.lookup parGuid recordParamsMap of
    Just (SugarM.RecordParamsInfo defGuid jumpTo) -> do
      defName <- getStoredNameS defGuid
      SugarExpr.make exprI $ BodyGetParams GetParams
        { _gpDefGuid = defGuid
        , _gpDefName = defName
        , _gpJumpTo = jumpTo
        }
    Nothing -> do
      parName <- getStoredNameS parGuid
      fmap SugarExpr.removeSuccessfulType . SugarExpr.make exprI .
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
  Expr.VariableRef (DefI (Tag m)) -> Convertor m
convertGetVariable varRef exprI = do
  cp <- (^. SugarM.scCodeAnchors) <$> SugarM.readContext
  case varRef of
    Expr.ParameterRef parGuid -> convertParameterRef parGuid exprI
    Expr.DefinitionRef defI -> do
      let defGuid = IRef.guid defI
      defName <- getStoredNameS defGuid
      SugarExpr.make exprI .
        BodyGetVar $ GetVar
        { _gvName = defName
        , _gvIdentifier = defGuid
        , _gvJumpTo = jumpToDefI cp defI
        , _gvVarType = GetDefinition
        }

memoBy ::
  (Cache.Key k, Binary v, MonadA m) =>
  k -> m v -> StateT Cache m v
memoBy k act = Cache.memoS (const act) k

convertLiteralInteger :: (MonadA m, Typeable1 m) => Integer -> Convertor m
convertLiteralInteger i exprI =
  SugarExpr.make exprI . BodyLiteralInteger $
  LiteralInteger
  { liValue = i
  , liSetValue = setValue . Property.value <$> SugarInfer.resultStored exprI
  }
  where
    setValue iref val =
      ExprIRef.writeExprBody iref $
      ExprLens.bodyLiteralInteger # val

convertTag :: (MonadA m, Typeable1 m) => Guid -> Convertor m
convertTag tag exprI = do
  name <- getStoredNameS tag
  SugarExpr.make exprI . BodyTag $ TagG tag name

convertAtom :: (MonadA m, Typeable1 m) => String -> Convertor m
convertAtom str exprI =
  SugarExpr.make exprI $ BodyAtom str

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
  (Typeable1 m, MonadA m) =>
  Kind -> Maybe (ExprIRef.ExpressionIM m) -> Guid ->
  ( SugarInfer.ExprMM m
  , SugarInfer.ExprMM m
  ) ->
  SugarM m (RecordField m (ExpressionU m))
convertField k mIRef defaultGuid (tagExpr, expr) = do
  tagExprS <- SugarM.convertSubexpression tagExpr
  exprS <- SugarM.convertSubexpression expr
  return RecordField
    { _rfMItemActions =
      recordFieldActions defaultGuid <$> SugarInfer.resultMIRef tagExpr <*> mIRef
    , _rfTag = SugarExpr.removeSuccessfulType tagExprS
    , _rfExpr =
        case k of
        Val -> exprS
        Type -> SugarExpr.removeSuccessfulType exprS
    }

convertRecord ::
  (Typeable1 m, MonadA m) =>
  Expr.Record (SugarInfer.ExprMM m) ->
  Convertor m
convertRecord (Expr.Record k fields) exprI = do
  sFields <- mapM (convertField k (SugarInfer.resultMIRef exprI) defaultGuid) fields
  fmap SugarExpr.removeSuccessfulType .
    SugarExpr.make exprI $ BodyRecord
    Record
    { _rKind = k
    , _rFields =
        FieldList
        { _flItems = withNextHoles sFields
        , _flMAddFirstItem = addField <$> SugarInfer.resultMIRef exprI
        }
    }
  where
    defaultGuid = SugarInfer.resultGuid exprI
    withNextHoles (field : rest@(nextField:_)) =
      (field
       & rfExpr %~ SugarExpr.setNextHole (nextField ^. rfExpr))
      : withNextHoles rest
    withNextHoles xs = xs
    addField iref =
      writeRecordFields iref defaultGuid $ \recordFields -> do
        holeTagExpr <- DataOps.newHole
        holeExpr <- DataOps.newHole
        return
          ( ExprIRef.exprGuid holeTagExpr
          , (holeTagExpr, holeExpr) : recordFields
          )

convertGetField ::
  (MonadA m, Typeable1 m) =>
  Expr.GetField (SugarInfer.ExprMM m) ->
  SugarInfer.ExprMM m ->
  SugarM m (ExpressionU m)
convertGetField (Expr.GetField recExpr tagExpr) exprI = do
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
  case mVar of
    Just var ->
      fmap SugarExpr.removeSuccessfulType .
      SugarExpr.make exprI $ BodyGetVar var
    Nothing -> do
      recExprS <- SugarM.convertSubexpression recExpr
      tagExprS <- SugarM.convertSubexpression tagExpr
      SugarExpr.make exprI $ BodyGetField
        GetField
        { _gfRecord = recExprS
        , _gfTag = SugarExpr.removeSuccessfulType tagExprS
        }

convertExpressionI ::
  (Typeable1 m, MonadA m) =>
  SugarInfer.ExprMM m -> SugarM m (ExpressionU m)
convertExpressionI ee =
  ($ ee) $
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
isCompleteType :: Expr.Expression def () -> Bool
isCompleteType =
  Lens.nullOf (Lens.folding ExprUtil.subExpressions . ExprLens.exprHole)

convertExpressionPure ::
  (MonadA m, Typeable1 m, RandomGen g) =>
  Anchors.CodeProps m -> g ->
  ExprIRef.ExpressionM m () -> T m (ExpressionU m)
convertExpressionPure cp gen =
  SugarM.runPure cp convertExpressionI Map.empty Map.empty .
  SugarM.convertSubexpression .
  SugarInfer.resultFromPure gen

data ConventionalParams m = ConventionalParams
  { cpTags :: [Guid]
  , cpParamInfos :: Map Guid SugarM.TagParamInfo
  , cpRecordParamsInfos :: Map Guid (SugarM.RecordParamsInfo m)
  , cpParams :: [FuncParam MStoredName m (ExpressionU m)]
  , cpAddFirstParam :: T m Guid
  }

data FieldParam m = FieldParam
  { fpTagGuid :: Guid
  , fpTagExpr :: SugarInfer.ExprMM m
  , fpFieldType :: SugarInfer.ExprMM m
  }

mkRecordParams ::
  (MonadA m, Typeable1 m) =>
  SugarM.RecordParamsInfo m -> Guid -> [FieldParam m] ->
  SugarInfer.ExprMM m ->
  Maybe (ExprIRef.ExpressionIM m) ->
  Maybe (ExprIRef.ExpressionM m (Stored m)) ->
  SugarM m (ConventionalParams m)
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
    }
  where
    lamGuid = SugarInfer.resultGuid lambdaExprI
    mLambdaP = lambdaExprI ^. Expr.ePayload . SugarInfer.plStored
    mkParamInfo fp =
      Map.singleton (fpTagGuid fp) . SugarM.TagParamInfo paramGuid .
      Guid.combine lamGuid $ fpTagGuid fp
    mkParam fp = do
      typeS <- SugarM.convertSubexpression $ fpFieldType fp
      let
        guid = fpTagGuid fp
        tagExprGuid = fpTagExpr fp ^. plGuid
      addFuncParamName FuncParam
        { _fpName = Nothing
        , _fpGuid = guid
        , _fpId = Guid.combine lamGuid guid
        , _fpAltIds = []
        , _fpVarKind = FuncFieldParameter
        , _fpHiddenLambdaGuid = Nothing --TODO: First param to take lambda's guid?
        , _fpType = SugarExpr.removeSuccessfulType typeS
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

plGuid :: Lens' (Expr.Expression def (SugarInfer.Payload t i s)) Guid
plGuid = Expr.ePayload . SugarInfer.plGuid

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
  (MonadA m, Typeable1 m) =>
  SugarM.RecordParamsInfo m -> [Guid] -> SugarInfer.ExprMM m ->
  SugarM m
  ( [FuncParam MStoredName m (ExpressionU m)]
  , ConventionalParams m
  , SugarInfer.ExprMM m
  )
convertDefinitionParams recordParamsInfo usedTags expr =
  case expr ^. Expr.eBody of
  Expr.BodyLam lambda@(Expr.Lambda Val paramGuid paramType body)
    | SugarInfer.isPolymorphicFunc expr -> do
      -- Dependent:
      fp <- convertPositionalFuncParam lambda expr
      (depParams, convParams, deepBody) <- convertDefinitionParams recordParamsInfo usedTags body
      return (fp : depParams, convParams, deepBody)
    | otherwise ->
      -- Independent:
      case paramType ^. Expr.eBody of
      Expr.BodyRecord (Expr.Record Type fields)
        | ListUtils.isLengthAtLeast 2 fields
        , Just fieldParams <- traverse makeFieldParam fields
        , all ((`notElem` usedTags) . fpTagGuid) fieldParams -> do
          convParams <-
            mkRecordParams recordParamsInfo paramGuid fieldParams
            expr (paramType ^? SugarInfer.plIRef)
            (traverse (^. SugarInfer.plStored) body)
          return ([], convParams, body)
      _ -> do
        param <- convertPositionalFuncParam lambda expr
        let conventionalParams = singleConventionalParam stored param paramGuid paramType body
        pure ([], conventionalParams, body)
  _ -> return ([], emptyConventionalParams stored, expr)
  where
    stored =
      fromMaybe (error "Definition body is always stored!") $
      SugarInfer.resultStored expr
    makeFieldParam (tagExpr, typeExpr) = do
      fieldTagGuid <- tagExpr ^? ExprLens.exprTag
      pure FieldParam
        { fpTagGuid = fieldTagGuid
        , fpTagExpr = tagExpr
        , fpFieldType = typeExpr
        }

singleConventionalParam ::
  MonadA m =>
  Stored m -> FuncParam MStoredName m (ExpressionU m) ->
  Guid -> SugarInfer.ExprMM m -> SugarInfer.ExprMM m -> ConventionalParams m
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
  }
  where
    existingParamTag = ExprLens.bodyTag # existingParamGuid
    existingParamTypeIRef =
      fromMaybe (error "Only stored record param type is converted as record") $
      SugarInfer.resultMIRef existingParamType
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

emptyConventionalParams :: MonadA m => ExprIRef.ExpressionProperty m -> ConventionalParams m
emptyConventionalParams stored = ConventionalParams
  { cpTags = []
  , cpParamInfos = Map.empty
  , cpRecordParamsInfos = Map.empty
  , cpParams = []
  , cpAddFirstParam = lambdaWrap stored
  }

mExtractWhere ::
  Expr.Expression def a ->
  Maybe (Expr.Apply (Expr.Expression def a), Expr.Lambda (Expr.Expression def a))
mExtractWhere expr = do
  apply <- expr ^? ExprLens.exprApply
  -- TODO: Use exprKindedLam
  lambda <- apply ^? Expr.applyFunc . ExprLens.exprLam
  guard $ (lambda ^. Expr.lambdaKind) == Val
  -- paramType has to be Hole for this to be sugarred to Where
  lambda ^? Expr.lambdaParamType . ExprLens.exprHole
  return (apply, lambda)

convertWhereItems ::
  (MonadA m, Typeable1 m) =>
  [Guid] ->
  SugarInfer.ExprMM m ->
  SugarM m ([WhereItem MStoredName m (ExpressionU m)], SugarInfer.ExprMM m)
convertWhereItems usedTags expr =
  case mExtractWhere expr of
  Nothing -> return ([], expr)
  Just (apply, lambda) -> do
    let
      defGuid = lambda ^. Expr.lambdaParamId
      recordParamsInfo =
        SugarM.RecordParamsInfo defGuid $ pure defGuid
    value <-
      convertDefinitionContent recordParamsInfo usedTags $
      apply ^. Expr.applyArg
    let
      mkWIActions topLevelProp bodyStored =
        ListItemActions
        { _itemDelete = do
             deleteParamRef (lambda ^. Expr.lambdaParamId) bodyStored
             SugarInfer.replaceWith topLevelProp $ bodyStored ^. Expr.ePayload
        , _itemAddNext = fmap fst $ DataOps.redexWrap topLevelProp
        }
    name <- getStoredNameS defGuid
    let
      item = WhereItem
        { wiValue = value
        , wiGuid = defGuid
        , wiHiddenGuids =
            map SugarInfer.resultGuid [expr, lambda ^. Expr.lambdaParamType]
        , wiActions =
          mkWIActions <$>
          SugarInfer.resultStored expr <*>
          traverse (Lens.view SugarInfer.plStored) (lambda ^. Expr.lambdaResult)
        , wiName = name
        }
    (nextItems, whereBody) <- convertWhereItems usedTags $ lambda ^. Expr.lambdaResult
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
  Expr.Expression def (SugarInfer.Payload t inferred (Maybe stored)) -> stored
assertedGetProp _
  Expr.Expression
  { Expr._ePayload =
    SugarInfer.Payload { SugarInfer._plStored = Just prop } } = prop
assertedGetProp msg _ = error msg

convertDefinitionContent ::
  (MonadA m, Typeable1 m) =>
  SugarM.RecordParamsInfo m -> [Guid] -> SugarInfer.ExprMM m ->
  SugarM m (DefinitionContent MStoredName m (ExpressionU m))
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
        { dDepParams = depParams
        , dParams = cpParams convParams
        , dBody = bodyS
        , dWhereItems = whereItems
        , dAddFirstParam = cpAddFirstParam convParams
        , dAddInnermostWhereItem =
          fmap fst . DataOps.redexWrap $
          assertedGetProp "Where must be stored" whereBody
        }

loadConvertDefI ::
  (MonadA m, Typeable1 m) =>
  Anchors.CodeProps m -> DefI (Tag m) ->
  CT m (DefinitionU m)
loadConvertDefI cp defI =
  convertDefI =<< lift (Load.loadDefinitionClosure defI)
  where
    defGuid = IRef.guid defI
    convertDefBody (Definition.BodyBuiltin builtin) =
      lift . convertDefIBuiltin cp builtin defI
    convertDefBody (Definition.BodyExpression exprLoaded) =
      convertDefIExpression cp exprLoaded defI
    convertDefI (Definition.Definition defBody typeLoaded) = do
      bodyS <- convertDefBody defBody $ Load.propertyOfClosure <$> typeLoaded
      name <- lift $ SugarExpr.getStoredName defGuid
      return Definition
        { _drGuid = defGuid
        , _drName = name
        , _drBody = bodyS
        }

convertDefIBuiltin ::
  (Typeable1 m, MonadA m) => Anchors.CodeProps m ->
  Definition.Builtin -> DefI (Tag m) ->
  ExprIRef.ExpressionM m (Stored m) ->
  T m (DefinitionBody MStoredName m (ExpressionU m))
convertDefIBuiltin cp (Definition.Builtin name) defI defType =
  DefinitionBodyBuiltin <$> do
    defTypeS <- convertExpressionPure cp iDefTypeGen (void defType)
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
  (Typeable1 m, MonadA m) =>
  Anchors.CodeProps m -> Guid ->
  ExprIRef.ExpressionM m (Stored m) ->
  ExprIRef.ExpressionM m () -> Bool ->
  T m (DefinitionTypeInfo m (ExpressionU m))
makeTypeInfo cp defGuid defType inferredType success
  | not success || not (isCompleteType inferredType) =
    DefinitionIncompleteType <$> do
      defTypeS <- convertExpressionPure cp iDefTypeGen $ void defType
      inferredTypeS <- convertExpressionPure cp iNewTypeGen inferredType
      pure ShowIncompleteType
        { sitOldType = defTypeS
        , sitNewIncompleteType = inferredTypeS
        }
  | typesMatch = pure DefinitionNoTypeInfo
  | otherwise =
    DefinitionNewType <$> do
      defTypeS <- convertExpressionPure cp iDefTypeGen $ void defType
      inferredTypeS <- convertExpressionPure cp iNewTypeGen inferredType
      pure AcceptNewType
        { antOldType = defTypeS
        , antNewType = inferredTypeS
        , antAccept =
          Property.set (defType ^. Expr.ePayload) =<<
          ExprIRef.newExpression inferredType
        }
  where
    iDefTypeGen = SugarExpr.mkGen 1 3 defGuid
    iNewTypeGen = SugarExpr.mkGen 2 3 defGuid
    typesMatch = void defType `ExprUtil.alphaEq` inferredType

convertDefIExpression ::
  (MonadA m, Typeable1 m) => Anchors.CodeProps m ->
  Load.LoadedClosure (Tag m) -> DefI (Tag m) ->
  ExprIRef.ExpressionM m (Stored m) ->
  CT m (DefinitionBody MStoredName m (ExpressionU m))
convertDefIExpression cp exprLoaded defI defType = do
  inferredLoadedResult@SugarInfer.InferLoadedResult
    { SugarInfer._ilrExpr = ilrExpr
    , SugarInfer._ilrSuccess = success
    } <-
    SugarInfer.inferLoadedExpression
    inferLoadedGen (Just defI) exprLoaded initialInferState
  let
    inferredType =
      void . Infer.iType . iwcInferred $ SugarInfer.resultInferred ilrExpr
  typeInfo <-
    lift $ makeTypeInfo cp defGuid defType (void inferredType) success
  context <- lift $ SugarM.mkContext cp convertExpressionI (Just defI) (Just reinferRoot) inferredLoadedResult
  lift . SugarM.run context $ do
    content <-
      convertDefinitionContent recordParamsInfo [] $
      ilrExpr & traverse . SugarInfer.plInferred %~ Just
    return $ DefinitionBodyExpression DefinitionExpression
      { _deContent = content
      , _deTypeInfo = typeInfo
      }
  where
    initialInferState = Infer.initial (Just defI)
    reinferRoot key = do
      loaded <-
        lift $ do
          reloadedRoot <-
            ExprIRef.readExpression . Load.irefOfClosure $
            exprLoaded ^. Expr.ePayload
          SugarInfer.load (Just defI) (void reloadedRoot)
      memoBy (key, loaded, initialInferState, "reinfer root" :: String) .
        return . isJust $ uncurry (SugarInfer.inferMaybe_ loaded)
        initialInferState
    defGuid = IRef.guid defI
    recordParamsInfo = SugarM.RecordParamsInfo defGuid $ jumpToDefI cp defI
    inferLoadedGen = SugarExpr.mkGen 0 3 defGuid
