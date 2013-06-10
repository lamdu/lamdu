{-# LANGUAGE ConstraintKinds #-}

module Lamdu.CodeEdit.Sugar.Convert.Hole
  ( convert, convertPlain, holeResultHasHoles
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens.Operators
import Control.Monad (MonadPlus(..), guard, join, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (runState, mapStateT)
import Control.MonadA (MonadA)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Traversable (sequenceA, traverse)
import Data.Tuple (swap)
import Data.Typeable (Typeable1)
import Lamdu.CodeEdit.Sugar.Infer (InferredWC, Stored)
import Lamdu.CodeEdit.Sugar.Monad (SugarM)
import Lamdu.CodeEdit.Sugar.Types
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Infer.Conflicts (InferredWithConflicts(..), iwcInferred, iwcInferredValues)
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
convert = convertH convertTypeCheckedHoleH

convertPlain ::
  (MonadA m, Typeable1 m) =>
  SugarInfer.ExprMM m -> SugarM m (ExpressionU m)
convertPlain = convertH convertPlainTyped

convertH ::
  (MonadA m, Typeable1 m) =>
  (InferredWC (Tag m) -> SugarInfer.ExprMM m -> SugarM m (ExpressionU m)) ->
  SugarInfer.ExprMM m -> SugarM m (ExpressionU m)
convertH convertTyped exprI =
  fmap fixWrap .
  maybe convertUntypedHole convertTypeCheckedHole $
  SugarInfer.resultInferred exprI
  where
    fixWrap expr =
      expr
      & rPayload . plActions . Lens.mapped . wrap .~
        (const . return) (expr ^. rGuid)
    convertTypeCheckedHole inferred = convertTyped inferred exprI
    convertUntypedHole = SugarExpr.make exprI . BodyHole $ Hole Nothing Nothing

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
  (MonadA m, Typeable1 m) =>
  InferredWC (Tag m) -> SugarInfer.ExprMM m -> SugarM m (ExpressionU m)
convertTypeCheckedHoleH iwc exprI =
  chooseHoleType (iwcInferredValues iwc)
  (convertPlainTyped iwc exprI)
  (convertInferred iwc exprI)

accept ::
  MonadA m => SugarM.Context m ->
  Infer.InferNode (DefI (Tag m)) ->
  ExprIRef.ExpressionM m a ->
  ExprIRef.ExpressionIM m ->
  T m (Maybe Guid)
accept sugarContext point expr iref = do
  loaded <- SugarInfer.load Nothing expr
  let
    exprInferred =
      unjust "The inferred value of a hole must type-check!" $
      SugarInfer.inferMaybe_ loaded inferState point
  pickResult iref $
    flip (,) Nothing <$> cleanUpInferredVal exprInferred
  where
    inferState = sugarContext ^. SugarM.scHoleInferState

convertInferred ::
  (MonadA m, Typeable1 m) =>
  InferredWC (Tag m) -> SugarInfer.ExprMM m ->
  ExprIRef.ExpressionM m () ->
  SugarM m (ExpressionU m)
convertInferred iwc exprI wvInferredVal = do
  sugarContext <- SugarM.readContext
  hole <- mkHole iwc exprI
  let
    gen expr = genFromHashable (eGuid, show (void expr))
  val <-
    SugarM.convertSubexpression $
    SugarInfer.resultFromPure (gen wvInferredVal) wvInferredVal
  -- wvInferredVal uses wvInferContext, but for "accept" purposes, we
  -- must use the holeInferContext:
  let
    inferredVal =
      ExprUtil.structureForType . void . Infer.iType $
      iwcInferred iwc
  SugarExpr.make exprI $ BodyInferred Inferred
    { _iHole = hole
    , _iValue = val
    , _iMAccept =
      fmap (fromMaybe eGuid) .
      accept sugarContext (Infer.iPoint (iwcInferred iwc))
      (ExprUtil.randomizeParamIds (gen inferredVal) inferredVal) .
      Property.value <$> SugarInfer.resultStored exprI
    }
  where
    eGuid = SugarInfer.resultGuid exprI

convertPlainTyped ::
  (MonadA m, Typeable1 m) =>
  InferredWC (Tag m) -> SugarInfer.ExprMM m ->
  SugarM m (ExpressionU m)
convertPlainTyped iwc exprI =
  SugarExpr.make exprI . BodyHole =<< mkHole iwc exprI

mkHole ::
  (MonadA m, Typeable1 m) =>
  InferredWC (Tag m) -> SugarInfer.ExprMM m ->
  SugarM m (Hole MStoredName m (ExpressionU m))
mkHole iwc exprI = do
  sugarContext <- SugarM.readContext
  mPaste <- fmap join . traverse mkPaste $ SugarInfer.resultStored exprI
  let
    inferState = sugarContext ^. SugarM.scHoleInferState
    contextHash = sugarContext ^. SugarM.scContextHash
    token = Cache.bsOfKey (eGuid, contextHash)
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
    inferExprType = inferOnTheSide token inferState $ Infer.nScope point
  mActions <-
    traverse mkWritableHoleActions $
    traverse (Lens.sequenceOf SugarInfer.plStored) exprI
  pure Hole
    { _holeMActions = mActions
    , _holeMArg = Nothing
    }
  where
    point = Infer.iPoint inferred
    eGuid = SugarInfer.resultGuid exprI
    inferred = iwcInferred iwc

cleanUpInferredVal ::
  Expr.Expression defa (Infer.Inferred defb) ->
  Expr.Expression defa (Infer.Inferred defb)
cleanUpInferredVal =
  (ExprLens.exprKindedLam Val . Lens._2 . Expr.eBody .~ bodyHole) .
  (ExprLens.exprApply . Lens.filtered isDependentApply .
   Expr.applyArg . Expr.eBody .~ bodyHole) .
  (Expr.eBody . Lens.traversed %~ cleanUpInferredVal)
  where
    isDependentApply =
      ExprUtil.isDependentPi . Infer.iType .
      (^. Expr.applyFunc . Expr.ePayload)
    bodyHole = ExprLens.bodyHole # ()

chooseHoleType ::
  [ExprIRef.ExpressionM m f] -> hole -> (ExprIRef.ExpressionM m f -> hole) -> hole
chooseHoleType inferredVals plain inferred =
  case inferredVals of
  [Expr.Expression { Expr._eBody = Expr.BodyLeaf Expr.Hole }] -> plain
  [inferredVal] -> inferred inferredVal
  _ -> plain

inferOnTheSide ::
  (MonadA m, Typeable1 m, Cache.Key t) => t ->
  Infer.Context (DefI (Tag m)) ->
  Infer.Scope (DefI (Tag m)) ->
  ExprIRef.ExpressionM m () ->
  CT m (Maybe (ExprIRef.ExpressionM m ()))
-- token represents the given holeInferContext
inferOnTheSide token holeInferContext scope expr =
  (fmap . fmap) (void . Infer.iType . (^. Expr.ePayload . Lens._1)) .
  SugarInfer.memoLoadInfer Nothing expr
  (Cache.bsOfKey (token, scope, "newNodeWithScope")) . swap $
  runState (Infer.newNodeWithScope scope) holeInferContext

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
    inferResult expr =
      SugarInfer.memoLoadInfer Nothing expr token
      (sugarContext ^. SugarM.scHoleInferState, Infer.iPoint inferred)
    gen = genFromHashable (guid, seedHashable seed)
    guid = SugarInfer.resultGuid exprI
    iref = Property.value $ SugarInfer.resultStored exprI
    token = Cache.bsOfKey (guid, sugarContext ^. SugarM.scContextHash)
    mkHoleResult (fakeConverted, fakeInferredExpr) =
      HoleResult
      { _holeResultInferred = fst <$> fakeInferredExpr
      , _holeResultConverted = fakeConverted
      , _holeResultPick = pick
      , _holeResultPickPrefix = void pick
      , _holeResultPickWrapped = do
          finalExpr <- fst <$> seedExprEnv cp seed
          written <-
            ExprIRef.writeExpressionWithStoredSubexpressions iref $
            fromStorePoint <$> holeWrap finalExpr
          pure . ExprIRef.exprGuid $ written ^. Expr.ePayload . Lens._1
      }
    fromStorePoint point = (unStorePoint <$> point, ())
    pick = do
      (finalExpr, mJumpTo) <-
        Cache.unmemoS makeInferredExpr
      mTargetGuid <- sequenceA mJumpTo
      fmap (mplus mTargetGuid) . pickResult iref .
        ExprUtil.randomizeParamIds gen $
        -- TODO: Makes no sense here anymore, move deeper inside
        -- makeInferredExpr:
        unjust
        ("Arbitrary fake tag successfully inferred as hole result, " ++
         "but real new tag failed!")
        finalExpr

holeWrap :: Expr.Expression def (Maybe a) -> Expr.Expression def (Maybe a)
holeWrap expr
  | Lens.has (ExprLens.exprApply . Expr.applyFunc . ExprLens.exprHole) expr =
    -- Don't rewrap already hole-wrapped results.
    expr
  | otherwise = Expr.Expression (ExprUtil.makeApply hole expr) Nothing
  where
    hole = Expr.Expression (ExprLens.bodyHole # ()) Nothing

seedExprEnv ::
  MonadA m => Anchors.CodeProps m -> HoleResultSeed m ->
  T m (ExprStorePoint m, Maybe (T m Guid))
seedExprEnv _ (ResultSeedExpression expr) = pure (expr, Nothing)
seedExprEnv cp (ResultSeedNewTag name) = do
  tag <- DataOps.makeNewPublicTag cp name
  pure (Nothing <$ ExprLens.pureExpr . ExprLens.bodyTag # tag, Nothing)
seedExprEnv cp (ResultSeedNewDefinition name) = do
  defI <- DataOps.newPublicDefinition cp name
  pure
    ( Nothing <$ ExprLens.pureExpr . ExprLens.bodyDefinitionRef # defI
    , Just $ IRef.guid defI <$ DataOps.newPane cp defI
    )

convertHoleResult ::
  (MonadA m, Typeable1 m) => SugarM.Context m -> Random.StdGen ->
  ExprIRef.ExpressionM m (Infer.Inferred (DefI (Tag m))) -> T m (ExpressionU m)
convertHoleResult sugarContext gen res =
  SugarM.run sugarContext
  { SugarM._scInferState = error "pure expression doesnt have infer state"
  , SugarM._scHoleInferState = error "pure expression doesnt have hole infer state"
  } .
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
  ExprIRef.ExpressionIM m ->
  ExprIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)), Maybe (StorePoint (Tag m))) ->
  T m (Maybe Guid)
pickResult exprIRef =
  fmap
  ( fmap (ExprIRef.exprGuid . Lens.view (Expr.ePayload . Lens._1))
  . listToMaybe . orderedInnerHoles
  ) .
  ExprIRef.writeExpressionWithStoredSubexpressions exprIRef .
  fmap ((Lens._1 . Lens.mapped %~ unStorePoint) . swap)

orderedInnerHoles ::
  Expr.Expression def (a, Infer.Inferred def) ->
  [Expr.Expression def (a, Infer.Inferred def)]
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
  Expr.Expression def (a, Infer.Inferred def) ->
  [Expr.Expression def (a, Infer.Inferred def)]
uninferredHoles e =
  case e ^. Expr.eBody of
  Expr.BodyLeaf Expr.Hole -> [e]
  Expr.BodyApply (Expr.Apply func _)
    | (ExprUtil.isDependentPi . Infer.iType . (^. Expr.ePayload . Lens._2)) func ->
      uninferredHoles func
  Expr.BodyLam (Expr.Lambda lamKind _ paramType result) ->
    uninferredHoles result ++ do
      guard $ lamKind == Type
      uninferredHoles paramType
  body -> Foldable.concatMap uninferredHoles body

holeResultHasHoles :: HoleResult name m -> Bool
holeResultHasHoles =
  not . null . uninferredHoles . fmap ((,) ()) . Lens.view holeResultInferred
