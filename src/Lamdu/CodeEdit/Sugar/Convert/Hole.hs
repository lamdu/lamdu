{-# LANGUAGE ConstraintKinds #-}

module Lamdu.CodeEdit.Sugar.Convert.Hole
  ( convert, holeResultHasHoles
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens.Operators
import Control.Monad (MonadPlus(..), guard, join, void)
import Control.MonadA (MonadA)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), runState, mapStateT)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.IRef (Tag)
import Data.Store.Guid (Guid)
import Data.Traversable (traverse)
import Data.Tuple (swap)
import Data.Typeable (Typeable1)
import Lamdu.CodeEdit.Sugar.Infer (InferredWC, Stored)
import Lamdu.CodeEdit.Sugar.Monad (SugarM)
import Lamdu.CodeEdit.Sugar.Types
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Infer.Conflicts (InferredWithConflicts(..), iwcInferredValues)
import qualified Control.Lens as Lens
import qualified Data.Cache as Cache
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.CodeEdit.Sugar.Expression as SugarExpr
import qualified Lamdu.CodeEdit.Sugar.Infer as SugarInfer
import qualified Lamdu.CodeEdit.Sugar.Monad as SugarM
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Ops as DataOps
import qualified System.Random as Random

convert ::
  (MonadA m, Typeable1 m) =>
  SugarInfer.ExprMM m -> SugarM m (ExpressionU m)
convert exprI =
  maybe convertUntypedHole convertTypeCheckedHole $
  SugarInfer.resultInferred exprI
  where
    convertTypeCheckedHole inferred = do
      ctx <- SugarM.readContext
      mPaste <- fmap join . traverse mkPaste $ SugarInfer.resultStored exprI
      convertTypeCheckedHoleH ctx mPaste inferred exprI
    convertUntypedHole = SugarExpr.make exprI . BodyHole $ Hole Nothing

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
      return $ ExprIRef.exprGuid clip

convertTypeCheckedHoleH ::
  (MonadA m, Typeable1 m) => SugarM.Context m -> Maybe (T m Guid) ->
  InferredWC (Tag m) -> SugarInfer.ExprMM m -> SugarM m (ExpressionU m)
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
      traverse (Lens.sequenceOf SugarInfer.plStored) exprI
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
        , _holeInferredType = void $ Infer.iType inferred
        , _holeInferExprType = inferExprType
        , _holeResult = makeHoleResult sugarContext inferred exprS
        }
    inferredHole x = do
      hole <- mkHole
      val <-
        SugarM.convertSubexpression $
        SugarInfer.resultFromPure (SugarExpr.mkGen 2 3 eGuid) x
      SugarExpr.make exprI $ BodyInferred Inferred
        { _iHole = hole
        , _iValue = val
        , _iMAccept =
          accept x . Property.value <$> SugarInfer.resultStored exprI
        }
    accept what iref = do
      writtenExpr <-
        fmap fst <$>
        ExprIRef.writeExpression iref what
      pure . ExprIRef.exprGuid .
        maybe iref (^. Expr.ePayload) $
        writtenExpr ^?
        Lens.folding ExprUtil.subExpressions .
        Lens.filtered (Lens.has ExprLens.exprHole)
    plainHole =
      SugarExpr.make exprI . BodyHole =<< mkHole

chooseHoleType ::
  [ExprIRef.ExpressionM m f] -> hole -> (ExprIRef.ExpressionM m f -> hole) -> hole
chooseHoleType inferredVals plain inferred =
  case inferredVals of
  [Expr.Expression { Expr._eBody = Expr.BodyLeaf Expr.Hole }] -> plain
  [inferredVal] -> inferred inferredVal
  _ -> plain

memoBy ::
  (Cache.Key k, Binary v, MonadA m) =>
  k -> m v -> StateT Cache m v
memoBy k act = Cache.memoS (const act) k

inferOnTheSide ::
  MonadA m =>
  Infer.Context (DefI (Tag m)) ->
  Infer.Scope (DefI (Tag m)) ->
  Infer.Loaded (DefI (Tag m)) () ->
  Maybe (ExprIRef.ExpressionM m ())
inferOnTheSide holeInferContext scope loaded =
  void . Infer.iType . Lens.view Expr.ePayload <$>
  SugarInfer.inferMaybe_ loaded sideInferContext node
  where
    (node, sideInferContext) =
      (`runState` holeInferContext) $ Infer.newNodeWithScope scope

getScopeElement ::
  MonadA m => SugarM.Context m ->
  (Guid, Expr.Expression def a) -> T m (Scope MStoredName m)
getScopeElement sugarContext (parGuid, typeExpr) = do
  scopePar <- mkGetPar
  mconcat . (scopePar :) <$>
    mapM onScopeField
    (typeExpr ^..
     -- TODO: Use exprKindedRecordFields Type!
     ExprLens.exprRecord . Expr.recordFields . traverse . Lens._1 . ExprLens.exprTag)
  where
    mkGetPar =
      case Map.lookup parGuid recordParamsMap of
      Just (SugarM.RecordParamsInfo defGuid jumpTo) -> do
        defName <- SugarExpr.getStoredName defGuid
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
        parName <- SugarExpr.getStoredName parGuid
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
    exprTag = ExprUtil.pureExpression . Expr.BodyLeaf . Expr.Tag
    getParam = ExprUtil.pureExpression $ ExprLens.bodyParameterRef # parGuid
    onScopeField tGuid = do
      name <- SugarExpr.getStoredName tGuid
      pure mempty
        { _scopeLocals = [
          ( GetVar
            { _gvIdentifier = tGuid
            , _gvName = name
            , _gvJumpTo = errorJumpTo
            , _gvVarType = GetFieldParameter
            }
          , ExprUtil.pureExpression . Expr.BodyGetField $
            Expr.GetField getParam (exprTag tGuid)
          )
        ] }

getGlobal :: MonadA m => DefI (Tag m) -> T m (Scope MStoredName m)
getGlobal defI = do
  name <- SugarExpr.getStoredName guid
  pure mempty
    { _scopeGlobals = [
      ( GetVar
        { _gvIdentifier = guid
        , _gvName = name
        , _gvJumpTo = errorJumpTo
        , _gvVarType = GetDefinition
        }
      , ExprUtil.pureExpression $ ExprLens.bodyDefinitionRef # defI
      )
      ] }
  where
    guid = IRef.guid defI
    errorJumpTo = error "Jump to on scope item??"

getTag :: MonadA m => Guid -> T m (Scope MStoredName m)
getTag guid = do
  name <- SugarExpr.getStoredName guid
  pure mempty
    { _scopeTags = [
      ( TagG
        { _tagGuid = guid
        , _tagName = name
        }
      , ExprLens.pureExpr . ExprLens.bodyTag # guid
      )
    ] }

makeHoleResult ::
  (Typeable1 m, MonadA m) => SugarM.Context m ->
  Infer.Inferred (DefI (Tag m)) ->
  Expr.Expression (DefI (Tag m))
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

seedExprEnv ::
  MonadA m => Anchors.CodeProps m -> HoleResultSeed m ->
  T m (ExprStorePoint m, Maybe Guid)
seedExprEnv _ (ResultSeedExpression expr) = pure (expr, Nothing)
seedExprEnv cp (ResultSeedNewTag name) = do
  tag <- DataOps.makeNewPublicTag cp name
  pure (Nothing <$ ExprLens.pureExpr . ExprLens.bodyTag # tag, Nothing)
seedExprEnv cp (ResultSeedNewDefinition name) = do
  defI <- DataOps.newPublicDefinition cp name
  DataOps.newPane cp defI
  let targetGuid = IRef.guid defI
  pure
    ( Nothing <$ ExprLens.pureExpr . ExprLens.bodyDefinitionRef # defI
    , Just targetGuid
    )

convertHoleResult ::
  (MonadA m, Typeable1 m) => SugarM.Context m -> Random.StdGen ->
  ExprIRef.ExpressionM m (Infer.Inferred (DefI (Tag m))) -> T m (ExpressionU m)
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

seedHashable :: HoleResultSeed m -> String
seedHashable (ResultSeedExpression expr) = show (void expr)
-- We want the new tag to have the same anim ids even as the name
-- changes, thus we ignore the name:
seedHashable (ResultSeedNewTag _) = "NewTag"
seedHashable (ResultSeedNewDefinition _) = "NewDefinition"

unjust :: String -> Maybe a -> a
unjust = fromMaybe . error

pickResult ::
  MonadA m =>
  ExprIRef.ExpressionM m (SugarInfer.PayloadM m i (Stored m)) ->
  ExprIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)), Maybe (StorePoint (Tag m))) ->
  T m (Maybe Guid)
pickResult exprS =
  fmap
  ( fmap (ExprIRef.exprGuid . Lens.view (Expr.ePayload . Lens._2))
  . listToMaybe . orderedInnerHoles . fmap swap
  ) .
  (ExprIRef.writeExpressionWithStoredSubexpressions . Property.value . SugarInfer.resultStored) exprS .
  fmap (Lens.over (Lens._1 . Lens.mapped) unStorePoint . swap)

orderedInnerHoles ::
  Expr.Expression def (Infer.Inferred def, a) ->
  [Expr.Expression def (Infer.Inferred def, a)]
orderedInnerHoles e =
  case e ^. Expr.eBody of
  Expr.BodyApply (Expr.Apply func arg)
    | Lens.notNullOf (Expr.eBody . ExprLens.bodyHole) func ->
      -- This is a "type-error wrapper".
      -- Skip the conversion hole
      -- and go to inner holes in the expression first.
      uninferredHoles arg ++ [func]
  _ -> uninferredHoles e

-- Also skip param types, those can usually be inferred later, so less
-- useful to fill immediately
uninferredHoles ::
  Expr.Expression def (Infer.Inferred def, a) ->
  [Expr.Expression def (Infer.Inferred def, a)]
uninferredHoles e =
  case e ^. Expr.eBody of
  Expr.BodyLeaf Expr.Hole -> [e]
  Expr.BodyApply (Expr.Apply func _)
    | (ExprUtil.isDependentPi . Infer.iType . Lens.view (Expr.ePayload . Lens._1)) func ->
      uninferredHoles func
  Expr.BodyLam (Expr.Lambda lamKind _ paramType result) ->
    uninferredHoles result ++ do
      guard $ lamKind == Type
      uninferredHoles paramType
  body -> Foldable.concatMap uninferredHoles body

holeResultHasHoles :: HoleResult name m -> Bool
holeResultHasHoles =
  not . null . uninferredHoles . fmap (flip (,) ()) . Lens.view holeResultInferred
