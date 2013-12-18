{-# LANGUAGE ConstraintKinds, DeriveFunctor, PatternGuards #-}

module Lamdu.Sugar.Convert.Hole
  ( convert, convertPlain, orderedInnerHoles
  ) where

import Control.Applicative (Applicative(..), (<$>), (<|>), (<$), Const(..))
import Control.Lens.Operators
import Control.Monad (guard, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), evalStateT, mapStateT, runState)
import Control.Monad.Trans.Writer (execWriter)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Maybe.Utils(unsafeUnjust)
import Data.Monoid (Monoid(..))
import Data.Monoid.Applicative (ApplicativeMonoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA, traverse)
import Data.Typeable (Typeable1)
import Lamdu.Data.Expr.IRef (DefIM)
import Lamdu.Data.Infer.Deref (DerefedTV)
import Lamdu.Data.Infer.Load (ldDef)
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
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expr.IRef as ExprIRef
import qualified Lamdu.Data.Expr.Lens as ExprLens
import qualified Lamdu.Data.Expr.Utils as ExprUtil
import qualified Lamdu.Data.Infer as Infer
import qualified Lamdu.Data.Infer.Deref as InferDeref
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
combineLamGuids :: (a -> Guid) -> Expr.Expression def a -> Expr.Expression def a
combineLamGuids getGuid =
  go Map.empty
  where
    go renames (Expr.Expression body a) =
      -- TODO: Lens.outside
      (`Expr.Expression` a) $
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
          newParamGuid = Guid.combine (getGuid a) paramGuid
      _ -> go renames <$> body

markHoles :: Expr.Expression def a -> Expr.Expression def (Bool, a)
markHoles =
  (ExprLens.holePayloads . Lens._1 .~ True) .
  (Lens.mapped %~ (,) False)

aWhen :: Applicative f => Bool -> f () -> f ()
aWhen False _ = pure ()
aWhen True x = x

translateIfInferred ::
  (Guid -> Random.StdGen) ->
  InputPayloadP (Maybe (Inferred m)) stored a ->
  Guid ->
  [(Guid, Guid)]
translateIfInferred mkGen aIP bGuid = do
  guard $ Lens.nullOf ExprLens.exprHole inferredVal
  translateInferred mkGen inferredVal (aIP ^. ipGuid) bGuid
  where
    inferredVal =
      aIP ^? ipInferred . Lens._Just . InferDeref.dValue
      <&> void & fromMaybe ExprUtil.pureHole

translateInferred ::
  (Guid -> Random.StdGen) ->
  LoadedExpr m () -> Guid -> Guid ->
  [(Guid, Guid)]
translateInferred mkGen inferredVal aGuid bGuid =
  idTranslations mkGen (intoIP <$> aExpr) bExpr
  where
    mkExpr guid = ExprUtil.randomizeExprAndParams (mkGen guid) (id <$ inferredVal)
    intoIP guid = InputPayload
      { _ipGuid = guid
      , _ipInferred = Nothing
      , _ipStored = Nothing
      , _ipData = ()
      }
    aExpr = mkExpr aGuid
    bExpr = mkExpr bGuid

idTranslations ::
  (Guid -> Random.StdGen) ->
  LoadedExpr m (InputPayloadP (Maybe (Inferred m)) stored a) ->
  LoadedExpr m Guid ->
  [(Guid, Guid)]
idTranslations mkGen convertedExpr writtenExpr =
  execWriter . runApplicativeMonoid . getConst $
  go
  (convertedExpr & combineLamGuids (^. ipGuid) & markHoles)
  (writtenExpr & combineLamGuids id & markHoles)
  where
    go = ExprUtil.matchExpressionG tell match mismatch
    match (aIsHole, aIP) (bIsHole, bGuid)
      | aIsHole /= bIsHole = error "match between differing bodies?"
      | otherwise =
        tell (aIP ^. ipGuid) bGuid *>
        aWhen aIsHole (tells $ translateIfInferred mkGen aIP bGuid)
    mismatch x y =
      error $
      unlines
      [ "Mismatch idTranslations: " ++ show (void x) ++ ", " ++ show (void y)
      , showExpr convertedExpr
      , showExpr writtenExpr
      ]
    showExpr expr = expr & ExprLens.exprDef .~ () & void & show
    tells = Const . ApplicativeMonoid . Writer.tell
    tell src dst = tells [(src, dst)]

mkWritableHoleActions ::
  (MonadA m, Typeable1 m) =>
  InputPayloadP (Inferred m) (Stored m) () ->
  ConvertM m (HoleActions MStoredName m)
mkWritableHoleActions exprPlStored = do
  sugarContext <- ConvertM.readContext
  mPaste <- mkPaste $ exprPlStored ^. ipStored
  globals <-
    ConvertM.liftTransaction . Transaction.getP . Anchors.globals $
    sugarContext ^. ConvertM.scCodeAnchors
  tags <-
    ConvertM.liftTransaction . Transaction.getP . Anchors.tags $
    sugarContext ^. ConvertM.scCodeAnchors
  let
    (inferredScope, newSugarContext) =
      (`runState` sugarContext) .
      Lens.zoom (ConvertM.scHoleInferContext . icContext) .
      Infer.getScope $ inferred ^. InferDeref.dTV . Infer.tvVal
  pure HoleActions
    { _holePaste = mPaste
    , _holeScope =
      mconcat . concat <$> sequence
      [ mapM (getScopeElement sugarContext) . Map.toList $
        inferred ^. InferDeref.dScope
      , mapM getGlobal globals
      , mapM getTag tags
      ]
    , _holeInferExprType = inferOnTheSide newSugarContext inferredScope
    , holeResult = mkHoleResult sugarContext exprPlStored
    }
  where
    inferred = exprPlStored ^. ipInferred

mkHole ::
  (MonadA m, Typeable1 m, Monoid a) =>
  InputPayloadP (Inferred m) (Maybe (Stored m)) a ->
  ConvertM m (Hole MStoredName m (ExpressionU m a))
mkHole exprPl = do
  mActions <-
    exprPl
    & ipData .~ ()
    & Lens.sequenceOf ipStored
    & traverse mkWritableHoleActions
  holeInferred <- mkHoleInferred $ exprPl ^. ipInferred
  pure Hole
    { _holeMActions = mActions
    , _holeMInferred = Just holeInferred
    , _holeMArg = Nothing
    }

mkHoleInferred ::
  (Typeable1 m, MonadA m) =>
  DerefedTV (DefIM m) ->
  ConvertM m (HoleInferred MStoredName m)
mkHoleInferred inferred = do
  sugarContext <- ConvertM.readContext
  (inferredIVal, newCtx) <-
    SugarInfer.memoInferAt (inferred ^. InferDeref.dTV) iVal
    & (`runStateT` (sugarContext ^. ConvertM.scWithVarsInferContext))
    & runMaybeT
    <&> unsafeUnjust "Inference on inferred val must succeed"
    & ConvertM.liftCTransaction
    <&> Lens._1 . Lens.mapped %~ fst
  let
    mkConverted gen =
      inferredIVal
      <&> mkInputPayload
      & ExprUtil.randomizeExprAndParams gen
      & ConvertM.convertSubexpression
      & ConvertM.run (sugarContext & ConvertM.scInferContexts .~ newCtx)
    miValInStructureContext =
      truncatedIValue .
      either (error . show) id . -- TODO: Handle errors??
      (`evalStateT` (sugarContext ^. ConvertM.scStructureInferContext . icContext)) $
      InferDeref.deref
      (inferred ^. InferDeref.dContext)
      (inferred ^. InferDeref.dTV . Infer.tvVal)
  pure HoleInferred
    { _hiBaseValue = miValInStructureContext
    , _hiWithVarsValue = iVal
    , _hiType = void $ inferred ^. InferDeref.dType
    , _hiMakeConverted = mkConverted
    }
  where
    mkInputPayload i guid = InputPayload
      { _ipGuid = guid
      , _ipInferred = Just i
      , _ipStored = Nothing
      , _ipData = ()
      }
    iVal = truncatedIValue $ inferred ^. InferDeref.dValue
    truncatedIValue i =
      i & void & ExprLens.lambdaParamTypes .~ ExprUtil.pureHole

inferOnTheSide ::
  (MonadA m, Typeable1 m) =>
  ConvertM.Context m ->
  Infer.Scope (DefIM m) ->
  ExprIRef.ExpressionM m () ->
  CT m (Maybe (LoadedExpr m ()))
-- token represents the given holeInferContext
inferOnTheSide sugarContext scope expr =
  (fmap . fmap) fst . runMaybeT .
  (`runStateT` (sugarContext ^. ConvertM.scHoleInferContext)) $
  SugarInfer.load expr
  >>= SugarInfer.memoInfer scope
  <&> void . (^. Expr.ePayload . Lens._1 . InferDeref.dType)

getScopeElement ::
  MonadA m => ConvertM.Context m ->
  (Guid, Expr.Expression def a) -> T m (Scope MStoredName m)
getScopeElement sugarContext (parGuid, typeExpr) = do
  scopePar <- mkGetPar
  mconcat . (scopePar :) <$>
    mapM onScopeField
    (typeExpr ^..
     ExprLens.exprKindedRecordFields KType . traverse . Lens._1 . ExprLens.exprTag)
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

-- TODO: Put the result in scopeGlobals in the caller, not here?
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
  ( LoadedExpr m (DerefedTV (DefIM m), MStorePoint m a)
  , InferContext m
  ) ->
  CT m
  ( ExpressionU m a
  , LoadedExpr m
    (InputPayloadP (DerefedTV (DefIM m)) (Stored m) a)
  , LoadedExpr m
    (InputPayloadP (DerefedTV (DefIM m)) (Stored m) a)
  )
writeConvertTypeChecked gen sugarContext holeStored (inferredExpr, newCtx) = do
  -- With the real stored guids:
  writtenExpr <-
    lift $ fmap toPayload .
    ExprIRef.addProperties (^. ldDef) (Property.set holeStored) <$>
    writeExprMStored (Property.value holeStored) (intoStorePoint <$> inferredExpr)
  let
    -- Replace the guids with consistently fake ones
    makeConsistentPayload (False, pl) guid = pl & ipGuid .~ guid
    makeConsistentPayload (True, pl) _ = pl
    consistentExpr =
      ExprUtil.randomizeExprAndParams gen $
      makeConsistentPayload <$> writtenExpr
    newSugarContext = sugarContext & ConvertM.scInferContexts .~ newCtx
  converted <-
    ConvertM.run newSugarContext . ConvertM.convertSubexpression $
    consistentExpr
    <&> ipStored %~ Just
    <&> ipInferred %~ Just
  return
    ( converted
    , consistentExpr
    , snd <$> writtenExpr
    )
  where
    intoStorePoint (inferred, (mStorePoint, a)) =
      (mStorePoint, (inferred, Lens.has Lens._Just mStorePoint, a))
    toPayload (stored, (inferred, wasStored, a)) = (,) wasStored InputPayload
      { _ipGuid = ExprIRef.exprGuid $ Property.value stored
      , _ipInferred = inferred
      , _ipStored = stored
      , _ipData = a
      }

mkHoleResult ::
  (Typeable1 m, MonadA m, Cache.Key a, Binary a, Monoid a) =>
  ConvertM.Context m ->
  InputPayloadP (Inferred m) (Stored m) () ->
  (Guid -> Random.StdGen) ->
  HoleResultSeed m (MStorePoint m a) ->
  CT m (Maybe (HoleResult MStoredName m a))
mkHoleResult sugarContext (InputPayload guid derefed stored ()) mkGen seed = do
  ((fMJumpTo, mResult), forkedChanges) <- cachedFork $ do
    (fSeedExpr, fMJumpTo) <- lift $ seedExprEnv (Nothing, mempty) cp seed
    fMInferredExprCtx <-
      runMaybeT . (`runStateT` (sugarContext ^. ConvertM.scHoleInferContext)) $
      SugarInfer.load fSeedExpr >>= SugarInfer.memoInferAt holePoint
    mResult <-
      traverse
      (writeConvertTypeChecked (mkGen guid) sugarContext stored)
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
    holePoint = derefed ^. InferDeref.dTV
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
          idTranslations mkGen
          (consistentExpr <&> ipInferred %~ Just)
          (ExprIRef.exprGuid . Property.value . (^. ipStored) <$> writtenExpr)
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
  T m (LoadedExpr m (ExprIRef.ExpressionIM m, a))
writeExprMStored exprIRef exprMStorePoint = do
  key <- Transaction.newKey
  randomizeNonStoredParamIds (genFromHashable key) exprMStorePoint
    & Lens.mapped . Lens._1 . Lens._Just %~ unStorePoint
    & ExprIRef.writeExpressionWithStoredSubexpressions (^. ldDef) exprIRef

orderedInnerHoles ::
  Expr.Expression ldef (a, DerefedTV def) ->
  [Expr.Expression ldef (a, DerefedTV def)]
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
  Expr.Expression ldef (a, DerefedTV def) ->
  [Expr.Expression ldef (a, DerefedTV def)]
uninferredHoles e =
  case e ^. Expr.eBody of
  Expr.BodyLeaf Expr.Hole -> [e]
  Expr.BodyApply (Expr.Apply func _)
    | (ExprUtil.isDependentPi . (^. Expr.ePayload . Lens._2 . InferDeref.dType)) func ->
      uninferredHoles func
  Expr.BodyLam (Expr.Lam lamKind _ paramType result) ->
    uninferredHoles result ++ do
      guard $ lamKind == KType
      uninferredHoles paramType
  body -> Foldable.concatMap uninferredHoles body
