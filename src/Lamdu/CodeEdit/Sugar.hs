{-# LANGUAGE FlexibleContexts, OverloadedStrings, ConstraintKinds, TypeFamilies, Rank2Types, PatternGuards #-}
module Lamdu.CodeEdit.Sugar
  ( Definition(..), drName, drGuid, drType, drBody
  , DefinitionBody(..)
  , ListItemActions(..), itemAddNext, itemDelete
  , FuncParamActions(..), fpListItemActions, fpGetExample
  , DefinitionExpression(..), deContent, deIsTypeRedundant, deMNewType
  , DefinitionContent(..), DefinitionNewType(..)
  , DefinitionBuiltin(..)
  , Actions(..)
    , giveAsArg, callWithArg, callWithNextArg
    , setToHole, replaceWithNewHole, cut, giveAsArgToOperator
  , Body(..)
  , Payload(..), plInferredTypes, plActions, plNextHole
  , ExpressionP(..)
    , rGuid, rBody, rPayload, rHiddenGuids, rPresugaredExpression
  , NameSource(..), NameCollision(..), Name(..), MStoredName
  , DefinitionN
  , Expression, ExpressionN
  , BodyN
  , WhereItem(..)
  , ListItem(..), ListActions(..), List(..)
  , RecordField(..), rfMItemActions, rfTag, rfExpr
  , Kind(..), Record(..), FieldList(..), GetField(..)
  , GetVarType(..)
  , GetVar(..), gvIdentifier, gvName, gvJumpTo, gvVarType
  , GetParams(..), gpDefGuid, gpDefName, gpJumpTo
  , LabeledApply(..), laFunc, laArgs
  , Func(..)
  , FuncParamType(..)
  , FuncParam(..), fpName, fpGuid, fpId, fpAltIds, fpVarKind, fpHiddenLambdaGuid, fpType, fpMActions
  , Pi(..)
  , Section(..)
  , Hole(..), holeScope, holeMActions
  , HoleResultSeed(..)
  , ScopeItem
  , Scope(..), scopeLocals, scopeGlobals, scopeTags, scopeGetParams
  , HoleActions(..), holePaste, holeMDelete, holeResult, holeInferExprType
  , StorePoint
  , HoleResult(..)
    , holeResultInferred
    , holeResultConverted
    , holeResultPick, holeResultPickPrefix
  , holeResultHasHoles
  , LiteralInteger(..)
  , TagG(..), tagName, tagGuid
  , Inferred(..), iValue, iHole
  , Collapsed(..), pFuncGuid, pCompact, pFullExpression
  , HasParens(..)
  , loadConvertDefI
  , SugarExpr.removeTypes
  , PrefixAction, emptyPrefixAction
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens (LensLike, Lens')
import Control.Lens.Operators
import Control.Monad (guard, join, mplus, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), runState, mapStateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Function (on)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe, isJust)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Traversable (traverse)
import Data.Tuple (swap)
import Data.Typeable (Typeable1)
import Lamdu.CodeEdit.Sugar.Infer (InferredWC, Stored)
import Lamdu.CodeEdit.Sugar.Monad (SugarM)
import Lamdu.CodeEdit.Sugar.Types
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Infer.Conflicts (InferredWithConflicts(..), iwcInferredValues)
import System.Random (RandomGen)
import qualified Control.Lens as Lens
import qualified Data.Cache as Cache
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.CodeEdit.Infix as Infix
import qualified Lamdu.CodeEdit.Sugar.AddNames as AddNames
import qualified Lamdu.CodeEdit.Sugar.Convert.Apply as Apply
import qualified Lamdu.CodeEdit.Sugar.Expression as SugarExpr
import qualified Lamdu.CodeEdit.Sugar.Infer as SugarInfer
import qualified Lamdu.CodeEdit.Sugar.Monad as SugarM
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Load as Load
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Ops as DataOps
import qualified System.Random as Random

type Convertor m = SugarInfer.ExprMM m -> SugarM m (ExpressionU m)

onMatchingSubexprs ::
  MonadA m => (a -> m ()) ->
  (Expression.Expression def a -> Bool) ->
  Expression.Expression def a -> m ()
onMatchingSubexprs action predicate =
  Lens.mapMOf_ (Lens.folding ExprUtil.subExpressions . Lens.filtered predicate)
  (action . (^. Expression.ePayload))

toHole :: MonadA m => Stored m -> T m ()
toHole = void . DataOps.setToHole

isGetParamOf :: Guid -> Expression.Expression def a -> Bool
isGetParamOf =
  Lens.anyOf (Expression.eBody . ExprUtil.bodyParameterRef) . (==)

isGetFieldParam :: Guid -> Guid -> Expression.Expression def a -> Bool
isGetFieldParam param tagG =
  p . (^? Expression.eBody . Expression._BodyGetField)
  where
    p Nothing = False
    p (Just (Expression.GetField record tag)) =
      Lens.anyOf ExprUtil.exprBodyTag (== tagG) tag &&
      Lens.anyOf (Expression.eBody . ExprUtil.bodyParameterRef) (== param) record

deleteParamRef ::
  MonadA m => Guid -> Expression.Expression def (Stored m) -> T m ()
deleteParamRef =
  onMatchingSubexprs toHole . isGetParamOf

deleteFieldParamRef ::
  MonadA m => Guid -> Guid -> Expression.Expression def (Stored m) -> T m ()
deleteFieldParamRef param tagG =
  onMatchingSubexprs toHole $ isGetFieldParam param tagG

lambdaWrap :: MonadA m => Stored m -> T m Guid
lambdaWrap stored =
  f <$> DataOps.lambdaWrap stored
  where
    f (newParam, newLamI) = Guid.combine (DataIRef.exprGuid newLamI) newParam

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
  MonadA m => Guid -> Stored m -> Expression.Expression def (Stored m) ->
  FuncParamActions MStoredName m
mkPositionalFuncParamActions param lambdaProp body =
  FuncParamActions
  { _fpListItemActions =
    ListItemActions
    { _itemDelete = do
        deleteParamRef param body
        SugarInfer.replaceWith lambdaProp $ body ^. Expression.ePayload
    , _itemAddNext = lambdaWrap $ body ^. Expression.ePayload
    }
  , _fpGetExample = pure $ fakeExample param
  }

getStoredName :: MonadA m => Guid -> T m (Maybe String)
getStoredName guid = do
  name <- Transaction.getP $ Anchors.assocNameRef guid
  pure $
    if null name then Nothing else Just name

getStoredNameS :: MonadA m => Guid -> SugarM m (Maybe String)
getStoredNameS = SugarM.liftTransaction . getStoredName

addFuncParamName ::
  MonadA m => FuncParam MStoredName f expr ->
  SugarM m (FuncParam MStoredName f expr)
addFuncParamName fp = do
  name <- getStoredNameS $ fp ^. fpGuid
  pure fp { _fpName = name }

convertPositionalFuncParam ::
  (Typeable1 m, MonadA m) => Expression.Lambda (SugarInfer.ExprMM m) ->
  SugarInfer.ExprMM m ->
  SugarM m (FuncParam MStoredName m (ExpressionU m))
convertPositionalFuncParam (Expression.Lambda _k paramGuid paramType body) lamExprI = do
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
      <$> lamExprI ^. Expression.ePayload . SugarInfer.plStored
      <*> traverse (^. SugarInfer.plStored) body
    }
  where
    lamGuid = SugarInfer.resultGuid lamExprI

convertPositionalLambda ::
  (Typeable1 m, MonadA m) => Expression.Lambda (SugarInfer.ExprMM m) ->
  SugarInfer.ExprMM m ->
  SugarM m (FuncParam MStoredName m (ExpressionU m), ExpressionU m)
convertPositionalLambda lam lamExprI = do
  param <- convertPositionalFuncParam lam lamExprI
  result <- SugarM.convertSubexpression (lam ^. Expression.lambdaResult)
  return (param & fpType %~ SugarExpr.setNextHole result, result)

fAllParams :: Func MStoredName m expr -> [FuncParam MStoredName m expr]
fAllParams (Func depParams params _) = depParams ++ params

convertFunc ::
  (MonadA m, Typeable1 m) => Expression.Lambda (SugarInfer.ExprMM m) ->
  Convertor m
convertFunc lambda lamExprI = do
  (param, sBody) <- convertPositionalLambda lambda lamExprI
  let
    innerFunc =
      case sBody ^. rBody of
      BodyFunc _ func -> func
      _ -> Func [] [] sBody
    mNextParam = listToMaybe $ fAllParams innerFunc
    newParam = maybe id deleteToNextParam mNextParam param
    newFunc
      | SugarInfer.isPolymorphicFunc lamExprI = innerFunc & fDepParams %~ (newParam :)
      | otherwise = Func [] (newParam : fAllParams innerFunc) $ innerFunc ^. fBody
  SugarExpr.make lamExprI $ BodyFunc DontHaveParens newFunc
  where
    deleteToNextParam nextParam =
      Lens.set
      (fpMActions . Lens.mapped . fpListItemActions .
       itemDelete . Lens.sets fmap) $
      nextParam ^. fpId

convertPi :: (MonadA m, Typeable1 m) => Expression.Lambda (SugarInfer.ExprMM m) -> Convertor m
convertPi lambda exprI = do
  (param, sBody) <- convertPositionalLambda lambda exprI
  SugarExpr.make exprI $ BodyPi DontHaveParens
    Pi
    { pParam = Lens.over fpType SugarExpr.addApplyChildParens param
    , pResultType = SugarExpr.removeSuccessfulType sBody
    }

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
  Expression.VariableRef (DefI (Tag m)) -> Convertor m
convertGetVariable varRef exprI = do
  isInfix <- SugarM.liftTransaction $ Infix.isInfixVar varRef
  cp <- (^. SugarM.scCodeAnchors) <$> SugarM.readContext
  getParExpr <-
    case varRef of
    Expression.ParameterRef parGuid -> convertParameterRef parGuid exprI
    Expression.DefinitionRef defI -> do
      let defGuid = IRef.guid defI
      defName <- getStoredNameS defGuid
      SugarExpr.make exprI .
        BodyGetVar $ GetVar
        { _gvName = defName
        , _gvIdentifier = defGuid
        , _gvJumpTo = jumpToDefI cp defI
        , _gvVarType = GetDefinition
        }
  if isInfix
    then
      SugarExpr.make exprI .
      BodySection HaveParens $
      Section Nothing (SugarExpr.removeInferredTypes getParExpr) Nothing
    else return getParExpr

mkPaste :: MonadA m => Stored m -> SugarM m (Maybe (T m Guid))
mkPaste exprP = do
  clipboardsP <- SugarM.codeAnchor Anchors.clipboards
  clipboards <- SugarM.getP clipboardsP
  let
    mClipPop =
      case clipboards of
      [] -> Nothing
      (clip : clips) -> Just (clip, Transaction.setP clipboardsP clips)
  return $ doPaste (Property.set exprP) <$> mClipPop
  where
    doPaste replacer (clipDefI, popClip) = do
      clipDef <- Transaction.readIRef clipDefI
      let
        clip =
          case clipDef of
          Definition.Definition (Definition.BodyExpression defExpr) _ -> defExpr
          _ -> error "Clipboard contained a non-expression definition!"
      Transaction.deleteIRef clipDefI
      ~() <- popClip
      ~() <- replacer clip
      return $ DataIRef.exprGuid clip

inferOnTheSide ::
  MonadA m =>
  Infer.Context (DefI (Tag m)) ->
  Infer.Scope (DefI (Tag m)) ->
  Infer.Loaded (DefI (Tag m)) () ->
  Maybe (DataIRef.ExpressionM m ())
inferOnTheSide holeInferContext scope loaded =
  void . Infer.iType . Lens.view Expression.ePayload <$>
  SugarInfer.inferMaybe_ loaded sideInferContext node
  where
    (node, sideInferContext) =
      (`runState` holeInferContext) $ Infer.newNodeWithScope scope

seedExprEnv ::
  MonadA m => Anchors.CodeProps m -> HoleResultSeed m ->
  T m (DataIRef.ExpressionM m (Maybe (StorePoint (Tag m))), Maybe Guid)
seedExprEnv _ (ResultSeedExpression expr) = pure (expr, Nothing)
seedExprEnv cp (ResultSeedNewTag name) = do
  tag <- DataOps.makeNewPublicTag cp name
  pure (Nothing <$ ExprUtil._PureTagExpr # tag, Nothing)
seedExprEnv cp (ResultSeedNewDefinition name) = do
  defI <- DataOps.makeDefinition cp name
  DataOps.newPane cp defI
  let targetGuid = IRef.guid defI
  pure
    ( Nothing <$ ExprUtil._PureExpr . ExprUtil.bodyDefinitionRef # defI
    , Just targetGuid
    )

seedHashable :: HoleResultSeed m -> String
seedHashable (ResultSeedExpression expr) = show (void expr)
-- We want the new tag to have the same anim ids even as the name
-- changes, thus we ignore the name:
seedHashable (ResultSeedNewTag _) = "NewTag"
seedHashable (ResultSeedNewDefinition _) = "NewDefinition"

convertHoleResult ::
  (MonadA m, Typeable1 m) => SugarM.Context m -> Random.StdGen ->
  DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m))) -> T m (ExpressionU m)
convertHoleResult sugarContext gen res =
  SugarM.runPure
  (sugarContext ^. SugarM.scCodeAnchors)
  (sugarContext ^. SugarM.scConvertSubexpression)
  (sugarContext ^. SugarM.scTagParamInfos)
  (sugarContext ^. SugarM.scRecordParamsInfos) .
  SugarM.convertSubexpression .
  (traverse . SugarInfer.plInferred %~ Just) .
  (traverse . SugarInfer.plStored .~ Nothing) $
  SugarInfer.resultFromInferred gen res

genFromHashable :: Hashable a => a -> Random.StdGen
genFromHashable = Random.mkStdGen . hashWithSalt 0

makeHoleResult ::
  (Typeable1 m, MonadA m) => SugarM.Context m ->
  Infer.Inferred (DefI (Tag m)) ->
  Expression.Expression (DefI (Tag m))
  (SugarInfer.PayloadM m inferred (Stored m)) ->
  HoleResultSeed m -> CT m (Maybe (HoleResult MStoredName m))
makeHoleResult sugarContext inferred exprI seed =
  fmap mkHoleResult <$>
  mapStateT Transaction.forkScratch
  (lift . traverse addConverted . fst =<< makeInferredExpr)
  where
    cp = sugarContext ^. SugarM.scCodeAnchors
    makeInferredExpr = lift (seedExprEnv cp seed) >>= Lens._1 inferResult
    addConverted inferredResult = do
      converted <-
        convertHoleResult sugarContext gen $
        fst <$> inferredResult
      pure (converted, inferredResult)
    inferResult expr = do
      loaded <- lift $ SugarInfer.load Nothing expr
      let point = Infer.iPoint inferred
      memoBy (loaded, token, point, 'r') . return $
        SugarInfer.inferMaybe loaded (sugarContext ^. SugarM.scHoleInferState) point
    gen = genFromHashable (guid, seedHashable seed)
    guid = SugarInfer.resultGuid exprI
    token = (guid, sugarContext ^. SugarM.scMContextHash)
    mkHoleResult (fakeConverted, fakeInferredExpr) =
      HoleResult
      { _holeResultInferred = fst <$> fakeInferredExpr
      , _holeResultConverted = fakeConverted
      , _holeResultPick = pick
      , _holeResultPickPrefix = void pick
      }
    pick = do
      (finalExpr, mTargetGuid) <-
        Lens.mapped . Lens._1 %~
        unjust
        ("Arbitrary fake tag successfully inferred as hole result, " ++
         "but real new tag failed!") $
        Cache.unmemoS makeInferredExpr
      fmap (mplus mTargetGuid) . pickResult exprI $
        ExprUtil.randomizeParamIds gen finalExpr

unjust :: String -> Maybe a -> a
unjust = fromMaybe . error

memoBy ::
  (Cache.Key k, Binary v, MonadA m) =>
  k -> m v -> StateT Cache m v
memoBy k act = Cache.memoS (const act) k

getGlobal :: MonadA m => DefI (Tag m) -> T m (Scope MStoredName m)
getGlobal defI = do
  name <- getStoredName guid
  pure mempty
    { _scopeGlobals = [
      ( GetVar
        { _gvIdentifier = guid
        , _gvName = name
        , _gvJumpTo = errorJumpTo
        , _gvVarType = GetDefinition
        }
      , ExprUtil.pureExpression $ ExprUtil.bodyDefinitionRef # defI
      )
      ] }
  where
    guid = IRef.guid defI
    errorJumpTo = error "Jump to on scope item??"

getTag :: MonadA m => Guid -> T m (Scope MStoredName m)
getTag guid = do
  name <- getStoredName guid
  pure mempty
    { _scopeTags = [
      ( TagG
        { _tagGuid = guid
        , _tagName = name
        }
      , ExprUtil._PureTagExpr # guid
      )
    ] }

getScopeElement ::
  MonadA m => SugarM.Context m ->
  (Guid, Expression.Expression def a) -> T m (Scope MStoredName m)
getScopeElement sugarContext (parGuid, typeExpr) = do
  scopePar <- mkGetPar
  mconcat . (scopePar :) <$>
    mapM onScopeField
    (typeExpr ^..
     Expression.eBody . Expression._BodyRecord .
     Expression.recordFields . traverse . Lens._1 . ExprUtil.exprBodyTag)
  where
    mkGetPar =
      case Map.lookup parGuid recordParamsMap of
      Just (SugarM.RecordParamsInfo defGuid jumpTo) -> do
        defName <- getStoredName defGuid
        pure mempty
          { _scopeGetParams = [
            ( GetParams
              { _gpDefGuid = defGuid
              , _gpDefName = defName
              , _gpJumpTo = jumpTo
              }
            , getParam )
          ] }
      Nothing -> do
        parName <- getStoredName parGuid
        pure mempty
          { _scopeLocals = [
            ( GetVar
              { _gvIdentifier = parGuid
              , _gvName = parName
              , _gvJumpTo = errorJumpTo
              , _gvVarType = GetParameter
              }
            , getParam )
          ] }
    recordParamsMap = sugarContext ^. SugarM.scRecordParamsInfos
    errorJumpTo = error "Jump to on scope item??"
    exprTag = ExprUtil.pureExpression . Expression.BodyLeaf . Expression.Tag
    getParam = ExprUtil.pureExpression $ ExprUtil.bodyParameterRef # parGuid
    onScopeField tGuid = do
      name <- getStoredName tGuid
      pure mempty
        { _scopeLocals = [
          ( GetVar
            { _gvIdentifier = tGuid
            , _gvName = name
            , _gvJumpTo = errorJumpTo
            , _gvVarType = GetFieldParameter
            }
          , ExprUtil.pureExpression . Expression.BodyGetField $
            Expression.GetField getParam (exprTag tGuid)
          )
        ] }

convertTypeCheckedHoleH ::
  (MonadA m, Typeable1 m) => SugarM.Context m -> Maybe (T m Guid) ->
  InferredWC (Tag m) -> Convertor m
convertTypeCheckedHoleH sugarContext mPaste iwc exprI =
  chooseHoleType (iwcInferredValues iwc) plainHole inferredHole
  where
    eGuid = SugarInfer.resultGuid exprI
    inferState  = sugarContext ^. SugarM.scHoleInferState
    contextHash = sugarContext ^. SugarM.scMContextHash
    inferred = iwcInferred iwc
    scope = Infer.nScope $ Infer.iPoint inferred
    token = (eGuid, contextHash)
    inferExprType expr = do
      loaded <- lift $ SugarInfer.load Nothing expr
      memoBy (loaded, token, scope, 't') . return $
        inferOnTheSide inferState scope loaded
    mkHole =
      fmap Hole . traverse mkWritableHoleActions $
      traverse (SugarInfer.ntraversePayload pure id) exprI
    mkWritableHoleActions exprS = do
      globals <-
        SugarM.liftTransaction . Transaction.getP . Anchors.globals $
        sugarContext ^. SugarM.scCodeAnchors
      tags <-
        SugarM.liftTransaction . Transaction.getP . Anchors.tags $
        sugarContext ^. SugarM.scCodeAnchors
      pure HoleActions
        { _holePaste = mPaste
        , _holeMDelete = Nothing
        , _holeScope =
          mconcat . concat <$> sequence
          [ mapM (getScopeElement sugarContext) . Map.toList $
            Infer.iScope inferred
          , mapM getGlobal globals
          , mapM getTag tags
          ]
        , _holeInferExprType = inferExprType
        , _holeResult = makeHoleResult sugarContext inferred exprS
        }
    inferredHole x = do
      hole <- mkHole
      SugarExpr.make exprI .
        BodyInferred . (`Inferred` hole) =<<
        (SugarM.convertSubexpression . fmap SugarInfer.toPayloadMM .
         SugarInfer.resultFromPure (SugarExpr.mkGen 2 3 eGuid)) x
    plainHole =
      SugarExpr.make exprI . BodyHole =<< mkHole

chooseHoleType ::
  [DataIRef.ExpressionM m f] -> hole -> (DataIRef.ExpressionM m f -> hole) -> hole
chooseHoleType inferredVals plain inferred =
  case inferredVals of
  [Expression.Expression { Expression._eBody = Expression.BodyLeaf Expression.Hole }] -> plain
  [inferredVal] -> inferred inferredVal
  _ -> plain

pickResult ::
  MonadA m =>
  DataIRef.ExpressionM m (SugarInfer.PayloadM m i (Stored m)) ->
  DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)), Maybe (StorePoint (Tag m))) ->
  T m (Maybe Guid)
pickResult exprS =
  fmap
  ( fmap (DataIRef.exprGuid . Lens.view (Expression.ePayload . Lens._2))
  . listToMaybe . uninferredHoles . fmap swap
  ) .
  (DataIRef.writeExpressionWithStoredSubexpressions . Property.value . SugarInfer.resultStored) exprS .
  fmap (Lens.over (Lens._1 . Lens.mapped) unStorePoint . swap)

-- Also skip param types, those can usually be inferred later, so less
-- useful to fill immediately
uninferredHoles ::
  Expression.Expression def (Infer.Inferred def, a) ->
  [Expression.Expression def (Infer.Inferred def, a)]
uninferredHoles e =
  case e ^. Expression.eBody of
  Expression.BodyLeaf Expression.Hole -> [e]
  Expression.BodyApply (Expression.Apply func _)
    | (ExprUtil.isDependentPi . Infer.iType . Lens.view (Expression.ePayload . Lens._1)) func ->
      uninferredHoles func
  Expression.BodyLam (Expression.Lambda lamKind _ paramType result) ->
    uninferredHoles result ++ do
      guard $ lamKind == Type
      uninferredHoles paramType
  body -> Foldable.concatMap uninferredHoles body

holeResultHasHoles :: HoleResult name m -> Bool
holeResultHasHoles =
  not . null . uninferredHoles . fmap (flip (,) ()) . Lens.view holeResultInferred

convertHole :: (MonadA m, Typeable1 m) => Convertor m
convertHole exprI =
  maybe convertUntypedHole convertTypeCheckedHole $
  SugarInfer.resultInferred exprI
  where
    convertTypeCheckedHole inferred = do
      ctx <- SugarM.readContext
      mPaste <- fmap join . traverse mkPaste $ SugarInfer.resultStored exprI
      convertTypeCheckedHoleH ctx mPaste inferred exprI
    convertUntypedHole = SugarExpr.make exprI . BodyHole $ Hole Nothing

convertLiteralInteger :: (MonadA m, Typeable1 m) => Integer -> Convertor m
convertLiteralInteger i exprI =
  SugarExpr.make exprI . BodyLiteralInteger $
  LiteralInteger
  { liValue = i
  , liSetValue = setValue . Property.value <$> SugarInfer.resultStored exprI
  }
  where
    setValue iref =
      DataIRef.writeExprBody iref . Lens.review ExprUtil.bodyLiteralInteger

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
  DataIRef.ExpressionIM m -> result ->
  ( [(DataIRef.ExpressionIM m, DataIRef.ExpressionIM m)] ->
    T m
    ( result
    , [(DataIRef.ExpressionIM m, DataIRef.ExpressionIM m)]
    )
  ) ->
  T m result
writeRecordFields iref def f = do
  oldBody <- DataIRef.readExprBody iref
  case oldBody ^? Expression._BodyRecord of
    Nothing -> return def
    Just oldRecord -> do
      (res, newRecord) <- sideChannel Expression.recordFields f oldRecord
      DataIRef.writeExprBody iref $ Expression.BodyRecord newRecord
      return res

recordFieldActions ::
  MonadA m => Guid -> DataIRef.ExpressionIM m -> DataIRef.ExpressionIM m ->
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
        ( DataIRef.exprGuid tagHole
        , prevFields ++ field : (tagHole, exprHole) : nextFields
        )
    delete (prevFields, _, nextFields) =
      return
      ( case nextFields ++ reverse prevFields of
        [] -> defaultGuid
        ((nextTagExpr, _) : _) -> DataIRef.exprGuid nextTagExpr
      , prevFields ++ nextFields
      )
    splitFields f oldFields =
      case break ((== exprIRef) . fst) oldFields of
      (prevFields, field : nextFields) -> f (prevFields, field, nextFields)
      _ -> return (defaultGuid, oldFields)

convertField ::
  (Typeable1 m, MonadA m) =>
  Kind -> Maybe (DataIRef.ExpressionIM m) -> Guid ->
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
  Expression.Record (SugarInfer.ExprMM m) ->
  Convertor m
convertRecord (Expression.Record k fields) exprI = do
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
          ( DataIRef.exprGuid holeTagExpr
          , (holeTagExpr, holeExpr) : recordFields
          )

convertGetField ::
  (MonadA m, Typeable1 m) =>
  Expression.GetField (SugarInfer.ExprMM m) ->
  SugarInfer.ExprMM m ->
  SugarM m (ExpressionU m)
convertGetField (Expression.GetField recExpr tagExpr) exprI = do
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
    tag <- tagExpr ^? ExprUtil.exprBodyTag
    paramInfo <- Map.lookup tag tagParamInfos
    param <- recExpr ^? Expression.eBody . ExprUtil.bodyParameterRef
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
  case ee ^. Expression.eBody of
  Expression.BodyLam x@(Expression.Lambda Val _ _ _) -> convertFunc x
  Expression.BodyLam x@(Expression.Lambda Type _ _ _) -> convertPi x
  Expression.BodyApply x -> Apply.convert x
  Expression.BodyRecord x -> convertRecord x
  Expression.BodyGetField x -> convertGetField x
  Expression.BodyLeaf (Expression.GetVariable x) -> convertGetVariable x
  Expression.BodyLeaf (Expression.LiteralInteger x) -> convertLiteralInteger x
  Expression.BodyLeaf (Expression.Tag x) -> convertTag x
  Expression.BodyLeaf Expression.Hole -> convertHole
  Expression.BodyLeaf Expression.Set -> convertAtom "Set"
  Expression.BodyLeaf Expression.IntegerType -> convertAtom "Int"
  Expression.BodyLeaf Expression.TagType -> convertAtom "Tag"

-- Check no holes
isCompleteType :: Expression.Expression def () -> Bool
isCompleteType =
  Lens.nullOf
  ( Lens.folding ExprUtil.subExpressions
  . Expression.eBody . Expression._BodyLeaf . Expression._Hole
  )

convertExpressionPure ::
  (MonadA m, Typeable1 m, RandomGen g) =>
  Anchors.CodeProps m -> g ->
  DataIRef.ExpressionM m () -> T m (ExpressionU m)
convertExpressionPure cp gen =
  SugarM.runPure cp convertExpressionI Map.empty Map.empty .
  SugarM.convertSubexpression . fmap SugarInfer.toPayloadMM .
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
  Maybe (DataIRef.ExpressionIM m) ->
  Maybe (DataIRef.ExpressionM m (Stored m)) ->
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
    mLambdaP = lambdaExprI ^. Expression.ePayload . SugarInfer.plStored
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

plGuid :: Lens' (Expression.Expression def (SugarInfer.Payload t i s)) Guid
plGuid = Expression.ePayload . SugarInfer.plGuid

type ExprField m = (DataIRef.ExpressionIM m, DataIRef.ExpressionIM m)
rereadFieldParamTypes ::
  MonadA m =>
  Guid -> DataIRef.ExpressionIM m ->
  ([ExprField m] -> ExprField m -> [ExprField m] -> T m Guid) ->
  T m Guid
rereadFieldParamTypes tagExprGuid paramTypeI f = do
  paramType <- DataIRef.readExprBody paramTypeI
  let
    mBrokenFields =
      paramType ^? Expression._BodyRecord . ExprUtil.kindedRecordFields Type .
      (Lens.to . break) ((tagExprGuid ==) . DataIRef.exprGuid . fst)
  case mBrokenFields of
    Just (prevFields, theField : nextFields) -> f prevFields theField nextFields
    _ -> return tagExprGuid

rewriteFieldParamTypes ::
  MonadA m => DataIRef.ExpressionIM m -> [ExprField m] -> T m ()
rewriteFieldParamTypes paramTypeI fields =
  DataIRef.writeExprBody paramTypeI . Expression.BodyRecord $
  Expression.Record Type fields

addFieldParamAfter :: MonadA m => Guid -> Guid -> DataIRef.ExpressionIM m -> T m Guid
addFieldParamAfter lamGuid tagExprGuid paramTypeI =
  rereadFieldParamTypes tagExprGuid paramTypeI $
  \prevFields theField nextFields -> do
    fieldGuid <- Transaction.newKey
    tagExprI <- DataIRef.newExprBody $ ExprUtil.bodyTag # fieldGuid
    holeTypeI <- DataOps.newHole
    rewriteFieldParamTypes paramTypeI $
      prevFields ++ theField : (tagExprI, holeTypeI) : nextFields
    pure $ Guid.combine lamGuid fieldGuid

delFieldParam ::
  MonadA m => Guid -> DataIRef.ExpressionIM m -> Guid ->
  Stored m -> DataIRef.ExpressionM m (Stored m) -> T m Guid
delFieldParam tagExprGuid paramTypeI paramGuid lambdaP bodyStored =
  rereadFieldParamTypes tagExprGuid paramTypeI $
  \prevFields (tagExprI, _) nextFields -> do
    tagExpr <- DataIRef.readExprBody tagExprI
    case tagExpr ^? ExprUtil.bodyTag of
      Just tagG -> deleteFieldParamRef paramGuid tagG bodyStored
      Nothing -> return ()
    case prevFields ++ nextFields of
      [] -> error "We were given fewer than 2 field params, which should never happen"
      [(fieldTagI, fieldTypeI)] -> do
        fieldTag <- DataIRef.readExprBody fieldTagI
        let
          fieldTagGuid =
            fromMaybe (error "field params always have proper Tag expr") $
            fieldTag ^? ExprUtil.bodyTag
        DataIRef.writeExprBody (Property.value lambdaP) $
          ExprUtil.makeLambda fieldTagGuid fieldTypeI bodyI
        deleteParamRef paramGuid bodyStored
        let
          toGetParam iref =
            DataIRef.writeExprBody iref $ ExprUtil.bodyParameterRef # fieldTagGuid
        onMatchingSubexprs (toGetParam . Property.value)
          (isGetFieldParam paramGuid fieldTagGuid) bodyStored
        pure $ Guid.combine lamGuid fieldTagGuid
      newFields -> do
        rewriteFieldParamTypes paramTypeI newFields
        dest prevFields nextFields
  where
    lamGuid = DataIRef.exprGuid $ Property.value lambdaP
    bodyI = bodyStored ^. Expression.ePayload . Property.pVal
    dest prevFields nextFields =
      fromMaybe (DataIRef.exprGuid bodyI) . listToMaybe <$>
      traverse (getParamGuidFromTagExprI . fst)
      (nextFields ++ reverse prevFields)
    getParamGuidFromTagExprI tagExprI = do
      tagExpr <- DataIRef.readExprBody tagExprI
      pure . Guid.combine lamGuid .
        fromMaybe (error "field param must have tags") $
        tagExpr ^? ExprUtil.bodyTag

atLeastTwo :: [a] -> Bool
atLeastTwo (_:_:_) = True
atLeastTwo _ = False

convertDefinitionParams ::
  (MonadA m, Typeable1 m) =>
  SugarM.RecordParamsInfo m -> [Guid] -> SugarInfer.ExprMM m ->
  SugarM m
  ( [FuncParam MStoredName m (ExpressionU m)]
  , ConventionalParams m
  , SugarInfer.ExprMM m
  )
convertDefinitionParams recordParamsInfo usedTags expr =
  case expr ^. Expression.eBody of
  Expression.BodyLam lambda@(Expression.Lambda Val paramGuid paramType body)
    | SugarInfer.isPolymorphicFunc expr -> do
      -- Dependent:
      fp <- convertPositionalFuncParam lambda expr
      (depParams, convParams, deepBody) <- convertDefinitionParams recordParamsInfo usedTags body
      return (fp : depParams, convParams, deepBody)
    | otherwise ->
      -- Independent:
      case paramType ^. Expression.eBody of
      Expression.BodyRecord (Expression.Record Type fields)
        | atLeastTwo fields
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
      fieldTagGuid <- tagExpr ^? ExprUtil.exprBodyTag
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
    existingParamTag = ExprUtil.bodyTag # existingParamGuid
    existingParamTypeIRef =
      fromMaybe (error "Only stored record param type is converted as record") $
      SugarInfer.resultMIRef existingParamType
    bodyWithStored =
      fromMaybe (error "Definition body should be stored") $
      traverse (^. SugarInfer.plStored) body
    addSecondParam mkFields = do
      existingParamTagI <- DataIRef.newExprBody existingParamTag
      let existingParamField = (existingParamTagI, existingParamTypeIRef)
      (newTagGuid, newParamField) <- newField
      newParamTypeI <-
        DataIRef.newExprBody . Expression.BodyRecord . Expression.Record Type $
        mkFields existingParamField newParamField
      newParamsGuid <- Transaction.newKey
      DataIRef.writeExprBody (Property.value lamProp) $
        ExprUtil.makeLambda newParamsGuid newParamTypeI .
        Property.value $ bodyWithStored ^. Expression.ePayload
      let
        toGetField iref = do
          recordRef <- DataIRef.newExprBody $ ExprUtil.bodyParameterRef # newParamsGuid
          tagRef <- DataIRef.newExprBody existingParamTag
          DataIRef.writeExprBody iref $ Expression.BodyGetField Expression.GetField
            { Expression._getFieldRecord = recordRef
            , Expression._getFieldTag = tagRef
            }
      onMatchingSubexprs (toGetField . Property.value)
        (isGetParamOf existingParamGuid) bodyWithStored
      let lamGuid = DataIRef.exprGuid $ Property.value lamProp
      pure $ Guid.combine lamGuid newTagGuid

emptyConventionalParams :: MonadA m => DataIRef.ExpressionProperty m -> ConventionalParams m
emptyConventionalParams stored = ConventionalParams
  { cpTags = []
  , cpParamInfos = Map.empty
  , cpRecordParamsInfos = Map.empty
  , cpParams = []
  , cpAddFirstParam = lambdaWrap stored
  }

mExtractWhere ::
  Expression.Expression def a ->
  Maybe (Expression.Apply (Expression.Expression def a), Expression.Lambda (Expression.Expression def a))
mExtractWhere expr = do
  apply <- expr ^? Expression.eBody . Expression._BodyApply
  lambda <- apply ^? Expression.applyFunc . Expression.eBody . Expression._BodyLam
  guard $ (lambda ^. Expression.lambdaKind) == Val
  -- paramType has to be Hole for this to be sugarred to Where
  lambda ^? Expression.lambdaParamType . Expression.eBody . ExprUtil.bodyHole
  return (apply, lambda)

convertWhereItems ::
  (MonadA m, Typeable1 m) =>
  [Guid] ->
  SugarInfer.ExprMM m ->
  SugarM m ([WhereItem MStoredName m], SugarInfer.ExprMM m)
convertWhereItems usedTags expr =
  case mExtractWhere expr of
  Nothing -> return ([], expr)
  Just (apply, lambda) -> do
    let
      defGuid = lambda ^. Expression.lambdaParamId
      recordParamsInfo =
        SugarM.RecordParamsInfo defGuid $ pure defGuid
    value <-
      convertDefinitionContent recordParamsInfo usedTags $
      apply ^. Expression.applyArg
    let
      mkWIActions topLevelProp bodyStored =
        ListItemActions
        { _itemDelete = do
             deleteParamRef (lambda ^. Expression.lambdaParamId) bodyStored
             SugarInfer.replaceWith topLevelProp $ bodyStored ^. Expression.ePayload
        , _itemAddNext = fmap fst $ DataOps.redexWrap topLevelProp
        }
    name <- getStoredNameS defGuid
    let
      item = WhereItem
        { wiValue = value
        , wiGuid = defGuid
        , wiHiddenGuids =
            map SugarInfer.resultGuid [expr, lambda ^. Expression.lambdaParamType]
        , wiActions =
          mkWIActions <$>
          SugarInfer.resultStored expr <*>
          traverse (Lens.view SugarInfer.plStored) (lambda ^. Expression.lambdaResult)
        , wiName = name
        }
    (nextItems, whereBody) <- convertWhereItems usedTags $ lambda ^. Expression.lambdaResult
    return (item : nextItems, whereBody)

newField ::
  MonadA m => T m (Guid, (DataIRef.ExpressionIM m, DataIRef.ExpressionIM m))
newField = do
  tag <- Transaction.newKey
  newTagI <- DataIRef.newExprBody (ExprUtil.bodyTag # tag)
  holeI <- DataOps.newHole
  return (tag, (newTagI, holeI))

addFirstFieldParam :: MonadA m => Guid -> DataIRef.ExpressionIM m -> T m Guid
addFirstFieldParam lamGuid recordI = do
  recordBody <- DataIRef.readExprBody recordI
  case recordBody ^? Expression._BodyRecord . ExprUtil.kindedRecordFields Type of
    Just fields -> do
      (newTagGuid, field) <- newField
      DataIRef.writeExprBody recordI $
        Expression.BodyRecord . Expression.Record Type $ field : fields
      pure $ Guid.combine lamGuid newTagGuid
    _ -> pure $ DataIRef.exprGuid recordI

assertedGetProp ::
  String ->
  Expression.Expression def (SugarInfer.Payload t inferred (Maybe stored)) -> stored
assertedGetProp _
  Expression.Expression
  { Expression._ePayload =
    SugarInfer.Payload { SugarInfer._plStored = Just prop } } = prop
assertedGetProp msg _ = error msg

convertDefinitionContent ::
  (MonadA m, Typeable1 m) =>
  SugarM.RecordParamsInfo m -> [Guid] -> SugarInfer.ExprMM m ->
  SugarM m (DefinitionContent MStoredName m)
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
  CT m (DefinitionN m)
loadConvertDefI cp defI =
  fmap AddNames.addToDef $ convertDefI =<< lift (Load.loadDefinitionClosure defI)
  where
    convertDefBody (Definition.BodyBuiltin builtin) =
      fmap return . convertDefIBuiltin builtin
    convertDefBody (Definition.BodyExpression exprLoaded) =
      convertDefIExpression cp exprLoaded
    convertDefI (Definition.Definition defBody typeLoaded) = do
      bodyS <- convertDefBody defBody defI $ Load.propertyOfClosure <$> typeLoaded
      typeS <-
        lift .
        convertExpressionPure cp (SugarExpr.mkGen 2 3 (IRef.guid defI)) $
        void typeLoaded
      let defGuid = IRef.guid defI
      name <- lift $ getStoredName defGuid
      return Definition
        { _drGuid = defGuid
        , _drName = name
        , _drBody = bodyS
        , _drType = typeS
        }

convertDefIBuiltin ::
  MonadA m => Definition.Builtin -> DefI (Tag m) ->
  DataIRef.ExpressionM m (Stored m) -> DefinitionBody MStoredName m
convertDefIBuiltin (Definition.Builtin name) defI typeI =
  DefinitionBodyBuiltin DefinitionBuiltin
    { biName = name
    , biMSetName = Just setName
    }
  where
    typeIRef = Property.value $ typeI ^. Expression.ePayload
    setName =
      Transaction.writeIRef defI .
      (`Definition.Definition` typeIRef) .
      Definition.BodyBuiltin . Definition.Builtin

makeNewTypeForDefinition ::
  (Typeable1 m, MonadA m, RandomGen gen) =>
  Anchors.CodeProps m -> Stored m -> DataIRef.ExpressionM m () -> Bool -> Bool ->
  gen -> T m (Maybe (DefinitionNewType MStoredName m))
makeNewTypeForDefinition cp typeIRef inferredTypeP typesMatch success iTypeGen
  | success && not typesMatch && isCompleteType inferredTypeP =
    Just <$> mkNewType
  | otherwise = return Nothing
  where
    mkNewType = do
      inferredTypeS <-
        convertExpressionPure cp iTypeGen inferredTypeP
      return DefinitionNewType
        { dntNewType = inferredTypeS
        , dntAcceptNewType =
          Property.set typeIRef =<<
          DataIRef.newExpression inferredTypeP
        }

convertDefIExpression ::
  (MonadA m, Typeable1 m) => Anchors.CodeProps m ->
  Load.LoadedClosure (Tag m) -> DefI (Tag m) ->
  DataIRef.ExpressionM m (Stored m) ->
  CT m (DefinitionBody MStoredName m)
convertDefIExpression cp exprLoaded defI typeI = do
  inferredLoadedResult@SugarInfer.InferLoadedResult
    { SugarInfer._ilrExpr = ilrExpr
    , SugarInfer._ilrSuccess = success
    } <-
    SugarInfer.inferLoadedExpression
    inferLoadedGen (Just defI) exprLoaded initialInferState
  let
    inferredTypeP =
      void . Infer.iType . iwcInferred $ SugarInfer.resultInferred ilrExpr
    typesMatch =
      on (==) ExprUtil.canonizeParamIds (void typeI) inferredTypeP
  mNewType <-
    lift $ makeNewTypeForDefinition cp (typeI ^. Expression.ePayload)
    inferredTypeP typesMatch success iTypeGen
  context <- lift $ SugarM.mkContext cp convertExpressionI (Just defI) (Just reinferRoot) inferredLoadedResult
  lift . SugarM.run context $ do
    content <-
      convertDefinitionContent recordParamsInfo [] $
      ilrExpr & traverse . SugarInfer.plInferred %~ Just
    return $ DefinitionBodyExpression DefinitionExpression
      { _deContent = content
      , _deMNewType = mNewType
      , _deIsTypeRedundant = success && typesMatch
      }
  where
    initialInferState = Infer.initial (Just defI)
    reinferRoot key = do
      loaded <-
        lift $ do
          reloadedRoot <-
            DataIRef.readExpression . Load.irefOfClosure $
            exprLoaded ^. Expression.ePayload
          SugarInfer.load (Just defI) (void reloadedRoot)
      memoBy (key, loaded, initialInferState, "reinfer root" :: String) .
        return . isJust $ uncurry (SugarInfer.inferMaybe_ loaded)
        initialInferState
    recordParamsInfo = SugarM.RecordParamsInfo defGuid $ jumpToDefI cp defI
    defGuid = IRef.guid defI
    iTypeGen = SugarExpr.mkGen 0 3 defGuid
    inferLoadedGen = SugarExpr.mkGen 1 3 defGuid
