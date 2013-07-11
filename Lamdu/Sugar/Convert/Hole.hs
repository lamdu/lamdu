{-# LANGUAGE ConstraintKinds, DeriveFunctor, PatternGuards #-}

module Lamdu.Sugar.Convert.Hole
  ( convert, convertPlain, orderedInnerHoles
  ) where

import Control.Applicative (Applicative(..), (<$>), (<|>), (<$), Const(..))
import Control.Lens.Operators
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), mapStateT)
import Control.Monad.Trans.Writer (execWriter)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))
import Data.Monoid.Applicative (ApplicativeMonoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA, traverse)
import Data.Typeable (Typeable1)
import Lamdu.Data.Expression.IRef (DefIM)
import Lamdu.Data.Expression.Infer.Conflicts (InferredWithConflicts(..), iwcInferred)
import Lamdu.Sugar.Convert.Monad (ConvertM)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import Lamdu.Sugar.Types.Internal
import System.Random.Utils (genFromHashable)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Cache as Cache
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Infer as SugarInfer
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified System.Random as Random

convert ::
  (MonadA m, Typeable1 m, Monoid a) =>
  InputPayload m a -> ConvertM m (ExpressionU m a)
convert exprPl =
  convertPlain exprPl
  <&> rPayload . plActions . Lens._Just . mSetToHole .~ Nothing

convertPlain ::
  (MonadA m, Typeable1 m, Monoid a) =>
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertPlain exprPl =
  maybe convertUntypedHole convertPlainTyped (Lens.sequenceOf ipInferred exprPl)
  <&> rPayload . plActions . Lens._Just . wrap .~ WrapNotAllowed
  where
    convertUntypedHole =
      ConvertExpr.make exprPl $ BodyHole Hole
      { _holeMActions = Nothing
      , _holeMInferred = Nothing
      , _holeMArg = Nothing
      }
    convertPlainTyped plInferred =
      ConvertExpr.make exprPl . BodyHole =<< mkHole plInferred

mkPaste :: MonadA m => Stored m -> ConvertM m (Maybe (T m Guid))
mkPaste exprP = do
  clipboardsP <- ConvertM.codeAnchor Anchors.clipboards
  clipboards <- ConvertM.getP clipboardsP
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
          Definition.Body (Definition.ContentExpression defExpr) _ -> defExpr
          _ -> error "Clipboard contained a non-expression definition!"
      Transaction.deleteIRef clipDefI
      ~() <- popClip
      ~() <- replacer clip
      return $ ExprIRef.exprGuid clip

-- Sugar exports fpId of Lambda params as:
--   Guid.combine lamGuid paramGuid
--
-- So to be compatible with that in our idTranslations, we want to
-- change our param Guids to match that:
combineLamGuids :: Expr.Expression def Guid -> Expr.Expression def Guid
combineLamGuids =
  go Map.empty
  where
    go renames (Expr.Expression body guid) =
      -- TODO: Lens.outside
      (`Expr.Expression` guid) $
      case body of
      Expr.BodyLeaf (Expr.GetVariable (Expr.ParameterRef paramGuid))
        | Just newParamGuid <- Map.lookup paramGuid renames ->
          ExprLens.bodyParameterRef # newParamGuid
      Expr.BodyLam (Expr.Lam k paramGuid paramType result) ->
        Expr.BodyLam
        (Expr.Lam k newParamGuid
         (go renames paramType)
         (go (Map.insert paramGuid newParamGuid renames) result))
        where
          newParamGuid = Guid.combine guid paramGuid
      _ -> go renames <$> body

type IdTranslations m =
  ExprIRef.ExpressionM m (ExprIRef.ExpressionIM m) ->
  [(Guid, Guid)]

idTranslations ::
  ExprIRef.ExpressionM m (InputPayloadP inferred stored a) ->
  IdTranslations m
idTranslations convertedExpr writtenExpr =
  execWriter . runApplicativeMonoid . getConst $
  go
  (combineLamGuids ((^. ipGuid) <$> convertedExpr))
  (combineLamGuids (ExprIRef.exprGuid <$> writtenExpr))
  where
    go = ExprUtil.matchExpressionG tell tell mismatch
    mismatch
      (Expr.Expression (Expr.BodyLeaf (Expr.Tag tagx)) plx)
      (Expr.Expression (Expr.BodyLeaf (Expr.Tag tagy)) ply) =
        tell tagx tagy *>
        tell plx ply
    mismatch inferredVal (Expr.Expression (Expr.BodyLeaf Expr.Hole) guid) =
      -- This happens only when inferred val is accepted after
      -- cleanUpInferredVal such that inferred parts are written as
      -- holes. Then they will be re-inferred to same val, and their
      -- guids will be generated via InputExpr.makePure with a
      -- random-gen based on the hole guid.  Let's map the old
      -- inferred val guids to the ones the new inferred val will get
      -- in the hole:
      go inferredVal $
      ExprUtil.randomizeExpr (genFromHashable guid)
      (flip const <$> inferredVal)
    mismatch x y =
      error $
      unlines
      [ "Mismatch idTranslations: " ++ show x ++ ", " ++ show y
      , showExpr convertedExpr
      , showExpr writtenExpr
      ]
    showExpr expr = expr & ExprLens.exprDef .~ () & void & show
    tell src dst = Const . ApplicativeMonoid $ Writer.tell [(src, dst)]

mkHole ::
  (MonadA m, Typeable1 m, Monoid a) =>
  InputPayloadP (InferredWC m) (Maybe (Stored m)) a ->
  ConvertM m (Hole MStoredName m (ExpressionU m a))
mkHole exprPl = do
  sugarContext <- ConvertM.readContext
  let
    mkWritableHoleActions exprPlStored = do
      mPaste <- mkPaste $ exprPlStored ^. ipStored
      globals <-
        ConvertM.liftTransaction . Transaction.getP . Anchors.globals $
        sugarContext ^. ConvertM.scCodeAnchors
      tags <-
        ConvertM.liftTransaction . Transaction.getP . Anchors.tags $
        sugarContext ^. ConvertM.scCodeAnchors
      pure HoleActions
        { _holePaste = mPaste
        , _holeScope =
          mconcat . concat <$> sequence
          [ mapM (getScopeElement sugarContext) . Map.toList $
            Infer.iScope inferred
          , mapM getGlobal globals
          , mapM getTag tags
          ]
        , _holeInferExprType = inferExprType
        , holeResult = makeHoleResult sugarContext exprPlStored
        }
    inferExprType = inferOnTheSide sugarContext $ Infer.nScope point
  mActions <-
    exprPl
    & ipData .~ ()
    & Lens.sequenceOf ipStored
    & traverse mkWritableHoleActions
  pure Hole
    { _holeMActions = mActions
    , _holeMInferred = Just HoleInferred
      { hiInferred = iwcInferred $ exprPl ^. ipInferred
      , hiContext = sugarContext ^. ConvertM.scHoleInferContext
      }
    , _holeMArg = Nothing
    }
  where
    point = Infer.iNode inferred
    inferred = iwcInferred $ exprPl ^. ipInferred

inferOnTheSide ::
  (MonadA m, Typeable1 m) =>
  ConvertM.Context m ->
  Infer.Scope (DefIM m) ->
  ExprIRef.ExpressionM m () ->
  CT m (Maybe (ExprIRef.ExpressionM m ()))
-- token represents the given holeInferContext
inferOnTheSide sugarContext scope expr =
  (fmap . fmap) fst . runMaybeT .
  (`runStateT` (sugarContext ^. ConvertM.scHoleInferContext)) $ do
    node <- mkNewNode scope
    SugarInfer.memoLoadInfer Nothing expr node
      <&> void . Infer.iType . (^. Expr.ePayload . Lens._1)

mkNewNode :: (Typeable1 m, MonadA f) => Infer.Scope (DefIM m) -> StateT (InferContext m) f (Infer.Node (DefIM m))
mkNewNode scope = do
  newNode <- Lens.zoom icContext $ Infer.newNodeWithScope scope
  icHashKey %= Cache.bsOfKey . (,,) "new node" scope
  return newNode

getScopeElement ::
  MonadA m => ConvertM.Context m ->
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
      Just (ConvertM.RecordParamsInfo defGuid jumpTo) -> do
        defName <- ConvertExpr.getStoredName defGuid
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
        parName <- ConvertExpr.getStoredName parGuid
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
    recordParamsMap = sugarContext ^. ConvertM.scRecordParamsInfos
    errorJumpTo = error "Jump to on scope item??"
    exprTag = ExprUtil.pureExpression . Expr.BodyLeaf . Expr.Tag
    getParam = ExprLens.pureExpr . ExprLens.bodyParameterRef # parGuid
    onScopeField tGuid = do
      name <- ConvertExpr.getStoredName tGuid
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

getGlobal :: MonadA m => DefIM m -> T m (Scope MStoredName m)
getGlobal defI = do
  name <- ConvertExpr.getStoredName guid
  pure mempty
    { _scopeGlobals = [
      ( GetVar
        { _gvIdentifier = guid
        , _gvName = name
        , _gvJumpTo = errorJumpTo
        , _gvVarType = GetDefinition
        }
      , ExprLens.pureExpr . ExprLens.bodyDefinitionRef # defI
      )
      ] }
  where
    guid = IRef.guid defI
    errorJumpTo = error "Jump to on scope item??"

getTag :: MonadA m => Guid -> T m (Scope MStoredName m)
getTag guid = do
  name <- ConvertExpr.getStoredName guid
  pure mempty
    { _scopeTags = [
      ( TagG
        { _tagGuid = guid
        , _tagName = name
        }
      , ExprLens.pureExpr . ExprLens.bodyTag # guid
      )
    ] }

seedExprEnv ::
  MonadA m =>
  a -> Anchors.CodeProps m -> HoleResultSeed m a ->
  T m (ExprIRef.ExpressionM m a, Maybe (T m Guid))
seedExprEnv _ _ (ResultSeedExpression expr) = pure (expr, Nothing)
seedExprEnv emptyPl cp (ResultSeedNewTag name) = do
  tag <- DataOps.makeNewPublicTag cp name
  pure (emptyPl <$ ExprLens.pureExpr . ExprLens.bodyTag # tag, Nothing)
seedExprEnv emptyPl cp (ResultSeedNewDefinition name) = do
  defI <- DataOps.newPublicDefinition cp name
  let jumpToDef =
        IRef.guid defI <$ DataOps.newPane cp defI
  pure
    ( emptyPl <$ ExprLens.pureExpr . ExprLens.bodyDefinitionRef # defI
    , Just jumpToDef
    )

cachedFork :: MonadA m => CT m a -> CT m (a, Transaction.Changes)
cachedFork =
  mapStateT fork
  where
    fork ctTrans = do
      ((res, newCache), changes) <- Transaction.fork ctTrans
      return ((res, changes), newCache)

writeConvertTypeChecked ::
  (MonadA m, Monoid a) => Random.StdGen ->
  ConvertM.Context m -> Stored m ->
  ( ExprIRef.ExpressionM m (Infer.Inferred (DefIM m), MStorePoint m a)
  , InferContext m
  ) ->
  CT m
  ( ExpressionU m a
  , ExprIRef.ExpressionM m
    (InputPayloadP (Infer.Inferred (DefIM m)) (Stored m) a)
  , ExprIRef.ExpressionM m
    (InputPayloadP (Infer.Inferred (DefIM m)) (Stored m) a)
  )
writeConvertTypeChecked gen sugarContext holeStored (inferredExpr, newCtx) = do
  -- With the real stored guids:
  writtenExpr <-
    lift $ fmap toPayload .
    ExprIRef.addProperties (Property.set holeStored) <$>
    writeExprMStored (Property.value holeStored) (intoStorePoint <$> inferredExpr)
  let
    -- Replace the guids with consistently fake ones
    makeConsistentPayload (False, pl) guid = pl & ipGuid .~ guid
    makeConsistentPayload (True, pl) _ = pl
    consistentExpr =
      ExprUtil.randomizeExprAndParams gen $
      makeConsistentPayload <$> writtenExpr
    newSugarContext = sugarContext & ConvertM.scHoleInferContext .~ newCtx
  converted <-
    ConvertM.run newSugarContext . ConvertM.convertSubexpression $
    consistentExpr
    <&> ipStored %~ Just
    <&> ipInferred %~ Just . toIWC
  return
    ( converted
    , consistentExpr
    , snd <$> writtenExpr
    )
  where
    toIWC x = InferredWithConflicts x [] []
    intoStorePoint (inferred, (mStorePoint, a)) =
      (mStorePoint, (inferred, Lens.has Lens._Just mStorePoint, a))
    toPayload (stored, (inferred, wasStored, a)) = (,) wasStored InputPayload
      { _ipGuid = ExprIRef.exprGuid $ Property.value stored
      , _ipInferred = inferred
      , _ipStored = stored
      , _ipData = a
      }

makeHoleResult ::
  (Typeable1 m, MonadA m, Cache.Key a, Binary a, Monoid a) =>
  ConvertM.Context m ->
  InputPayloadP (InferredWC m) (Stored m) () ->
  Random.StdGen ->
  HoleResultSeed m (MStorePoint m a) ->
  CT m (Maybe (HoleResult MStoredName m a))
makeHoleResult sugarContext (InputPayload _guid iwc stored ()) gen seed = do
  ((fMJumpTo, mResult), forkedChanges) <- cachedFork $ do
    (fSeedExpr, fMJumpTo) <- lift $ seedExprEnv (Nothing, mempty) cp seed
    fMInferredExprCtx <-
      runMaybeT . (`runStateT` (sugarContext ^. ConvertM.scHoleInferContext)) $
      SugarInfer.memoLoadInfer Nothing fSeedExpr holePoint
    mResult <-
      traverse
      (writeConvertTypeChecked gen sugarContext stored)
      fMInferredExprCtx
    return (fMJumpTo, mResult)

  traverse (mkResult fMJumpTo (Transaction.merge forkedChanges)) mResult
  where
    mkResult fMJumpTo unfork (fConverted, fConsistentExpr, fWrittenExpr) = do
      let
        pick = unfork *> mkPickedResult fMJumpTo fConsistentExpr fWrittenExpr
        inferredExpr = (^. ipInferred) <$> fWrittenExpr
      pure HoleResult
        { _holeResultInferred = inferredExpr
        , _holeResultConverted = fConverted
        , _holeResultPick = pick
        , _holeResultHasHoles =
          not . null . uninferredHoles $ (,) () <$> inferredExpr
        }
    cp = sugarContext ^. ConvertM.scCodeAnchors
    holePoint = Infer.iNode $ iwcInferred iwc
    mkPickedResult mJumpTo consistentExpr writtenExpr = do
      mJumpGuid <- sequenceA mJumpTo
      let
        f payload = (payload ^. ipGuid, payload ^. ipInferred)
        mNextHole = listToMaybe . orderedInnerHoles $ f <$> writtenExpr
      pure
        PickedResult
        { _prMJumpTo =
          mJumpGuid <|>
          (^. Expr.ePayload . Lens._1) <$> mNextHole
        , _prIdTranslation =
          idTranslations consistentExpr $
          Property.value . (^. ipStored) <$> writtenExpr
        }

randomizeNonStoredParamIds ::
  Random.StdGen -> ExprStorePoint m a -> ExprStorePoint m a
randomizeNonStoredParamIds gen =
  ExprUtil.randomizeParamIdsG id nameGen Map.empty $ \_ _ pl -> pl
  where
    nameGen = ExprUtil.onNgMakeName f $ ExprUtil.randomNameGen gen
    f n prevFunc prevGuid pl@(mStorePoint, _)
      | Lens.has Lens._Just mStorePoint = (prevGuid, n)
      | otherwise = prevFunc prevGuid pl

writeExprMStored ::
  MonadA m =>
  ExprIRef.ExpressionIM m ->
  ExprStorePoint m a ->
  T m (ExprIRef.ExpressionM m (ExprIRef.ExpressionIM m, a))
writeExprMStored exprIRef exprMStorePoint = do
  key <- Transaction.newKey
  randomizeNonStoredParamIds (genFromHashable key) exprMStorePoint
    & Lens.mapped . Lens._1 . Lens._Just %~ unStorePoint
    & ExprIRef.writeExpressionWithStoredSubexpressions exprIRef

orderedInnerHoles ::
  Expr.Expression def (a, Infer.Inferred def) ->
  [Expr.Expression def (a, Infer.Inferred def)]
orderedInnerHoles e =
  case e ^. Expr.eBody of
  Expr.BodyApply (Expr.Apply func arg)
    | Lens.has ExprLens.exprHole func ->
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
  Expr.BodyLam (Expr.Lam lamKind _ paramType result) ->
    uninferredHoles result ++ do
      guard $ lamKind == KType
      uninferredHoles paramType
  body -> Foldable.concatMap uninferredHoles body
