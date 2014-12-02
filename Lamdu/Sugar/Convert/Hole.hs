{-# LANGUAGE ConstraintKinds #-}

module Lamdu.Sugar.Convert.Hole
  ( convert, convertPlain, orderedInnerHoles
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Arrow ((&&&))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), evalStateT, evalState)
import Control.MonadA (MonadA)
import Data.Maybe.Utils(unsafeUnjust)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Data.String (IsString(..))
import Data.Traversable (traverse)
import Lamdu.Expr.IRef (DefIM)
import Lamdu.Expr.Type (Type(..))
import Lamdu.Expr.Val (Val(..))
import Lamdu.Sugar.Convert.Monad (ConvertM)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import Lamdu.Sugar.Types.Internal
import Lamdu.Suggest (suggestValueWith)
import System.Random.Utils (genFromHashable)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.Expression as ConvertExpr
import qualified Lamdu.Sugar.Convert.Infer as SugarInfer
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.InputExpr as InputExpr
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified System.Random as Random

type T = Transaction

convert ::
  (MonadA m, Monoid a) =>
  InputPayload m a -> ConvertM m (ExpressionU m a)
convert exprPl =
  convertPlain exprPl
  <&> rPayload . plActions . Lens._Just . setToHole .~ AlreadyAHole

convertPlain ::
  (MonadA m, Monoid a) =>
  InputPayload m a -> ConvertM m (ExpressionU m a)
convertPlain exprPl =
  mkHole exprPl
  <&> BodyHole
  >>= ConvertExpr.make exprPl
  <&> rPayload . plActions . Lens._Just . wrap .~ WrapNotAllowed

mkPaste ::
  MonadA m => ExprIRef.ValIProperty m -> ConvertM m (Maybe (T m EntityId))
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
          Definition.Body (Definition.ContentExpr defExpr) _ -> defExpr
          _ -> error "Clipboard contained a non-expression definition!"
      Transaction.deleteIRef clipDefI
      ~() <- popClip
      ~() <- replacer clip
      return $ EntityId.ofValI clip

inferOnTheSide ::
  (MonadA m) =>
  ConvertM.Context m -> Infer.Scope -> Val () ->
  T m (Maybe Type)
-- token represents the given holeInferContext
inferOnTheSide sugarContext scope val =
  runMaybeT . (`evalStateT` (sugarContext ^. ConvertM.scInferContext)) $
  SugarInfer.loadInferScope scope val
  <&> (^. V.payload . Lens._1 . Infer.plType)

mkWritableHoleActions ::
  (MonadA m) =>
  InputPayload m dummy -> ExprIRef.ValIProperty m ->
  ConvertM m (HoleActions MStoredName m)
mkWritableHoleActions exprPl stored = do
  sugarContext <- ConvertM.readContext
  mPaste <- mkPaste stored
  globals <-
    ConvertM.liftTransaction . Transaction.getP . Anchors.globals $
    sugarContext ^. ConvertM.scCodeAnchors
  tags <-
    ConvertM.liftTransaction . Transaction.getP . Anchors.tags $
    sugarContext ^. ConvertM.scCodeAnchors
  let inferredScope = inferred ^. Infer.plScope
  pure HoleActions
    { _holePaste = mPaste
    , _holeScope =
      mconcat . concat <$> sequence
      [ mapM (getScopeElement sugarContext) $ Map.toList $ Infer.scopeToTypeMap inferredScope
      , mapM getGlobal globals
      , mapM (getTag (exprPl ^. ipEntityId)) tags
      ]
    , _holeInferExprType = inferOnTheSide sugarContext inferredScope
    , holeResult = mkHoleResult sugarContext exprPl stored
    , _holeGuid = UniqueId.toGuid $ ExprIRef.unValI $ Property.value stored
    }
  where
    inferred = exprPl ^. ipInferred

mkHoleSuggested :: MonadA m => Infer.Payload -> ConvertM m (HoleSuggested MStoredName m)
mkHoleSuggested inferred = do
  sugarContext <- ConvertM.readContext
  (inferredIVal, newCtx) <-
    SugarInfer.loadInferScope (inferred ^. Infer.plScope) suggestedVal
    & (`runStateT` (sugarContext ^. ConvertM.scInferContext))
    & runMaybeT
    <&> unsafeUnjust "Inference on inferred val must succeed"
    & ConvertM.liftTransaction
  let
    mkConverted =
      inferredIVal
      <&> mkInputPayload . fst
      & consistentExprIds
      & ConvertM.convertSubexpression
      & ConvertM.run (sugarContext & ConvertM.scInferContext .~ newCtx)
  pure HoleSuggested
    { _hsSuggestedValue = suggestedVal
    , _hsType = inferred ^. Infer.plType
    , _hsMakeConverted = mkConverted
    }
  where
    consistentExprIds val = EntityId.randomizeExprAndParams (genFromHashable (void val)) val
    suggestedVal =
      (`evalState` (0 :: Int)) $
      suggestValueWith mkVar
      (inferred ^. Infer.plType)
    mkVar = do
      i <- State.get
      State.modify (+1)
      return . fromString $ "var" ++ show i
    mkInputPayload i guid entityId = InputPayload
      { _ipEntityId = entityId
      , _ipGuid = guid
      , _ipInferred = i
      , _ipStored = Nothing
      , _ipData = ()
      }

mkHole ::
  (MonadA m, Monoid a) =>
  InputPayload m a -> ConvertM m (Hole MStoredName m (ExpressionU m a))
mkHole exprPl = do
  mActions <- traverse (mkWritableHoleActions exprPl) (exprPl ^. ipStored)
  suggested <- mkHoleSuggested $ exprPl ^. ipInferred
  pure Hole
    { _holeMActions = mActions
    , _holeSuggested = suggested
    , _holeMArg = Nothing
    }

getScopeElement ::
  MonadA m => ConvertM.Context m ->
  (V.Var, Type) -> T m (Scope MStoredName m)
getScopeElement sugarContext (par, typeExpr) = do
  scopePar <- mkGetPar
  mconcat . (scopePar :) <$>
    mapM onScopeField
    (typeExpr ^.. ExprLens._TRecord . ExprLens.compositeTags)
  where
    mkGetPar =
      case Map.lookup par recordParamsMap of
      Just (ConvertM.RecordParamsInfo defName jumpTo) ->
        pure mempty
          { _scopeGetParams = [
            ( GetParams
              { _gpDefName = defName
              , _gpJumpTo = jumpTo
              }
            , getParam )
          ] }
      Nothing -> do
        parName <- ConvertExpr.makeStoredNameProperty par
        pure mempty
          { _scopeLocals = [
            ( GetVar
              { _gvName = parName
              , _gvJumpTo = errorJumpTo
              , _gvVarType = GetParameter
              }
            , getParam )
          ] }
    recordParamsMap = sugarContext ^. ConvertM.scRecordParamsInfos
    errorJumpTo = error "Jump to on scope item??"
    getParam = P.var par
    onScopeField tag = do
      name <- ConvertExpr.makeStoredNameProperty tag
      pure mempty
        { _scopeLocals = [
          ( GetVar
            { _gvName = name
            , _gvJumpTo = errorJumpTo
            , _gvVarType = GetFieldParameter
            }
          , P.getField getParam tag
          )
        ] }

-- TODO: Put the result in scopeGlobals in the caller, not here?
getGlobal :: MonadA m => DefIM m -> T m (Scope MStoredName m)
getGlobal defI = do
  name <- ConvertExpr.makeStoredNameProperty defI
  pure mempty
    { _scopeGlobals = [
      ( GetVar
        { _gvName = name
        , _gvJumpTo = errorJumpTo
        , _gvVarType = GetDefinition
        }
      , P.global $ ExprIRef.globalId defI
      )
      ] }
  where
    errorJumpTo = error "Jump to on scope item??"

getTag :: MonadA m => EntityId -> T.Tag -> T m (Scope MStoredName m)
getTag ctxEntityId tag = do
  name <- ConvertExpr.makeStoredNameProperty tag
  let
    tagG = TagG
      { _tagInstance = EntityId.augment (show (UniqueId.toGuid tag)) ctxEntityId
      , _tagVal = tag
      , _tagGName = name
      }
  pure mempty { _scopeTags = [(tagG, tag)] }

writeConvertTypeChecked ::
  (MonadA m, Monoid a) =>
  ConvertM.Context m -> ExprIRef.ValIProperty m ->
  Val (Infer.Payload, MStorePoint m a) ->
  T m
  ( ExpressionU m a
  , Val (InputPayload m a)
  , Val (ExprIRef.ValIProperty m, InputPayload m a)
  )
writeConvertTypeChecked sugarContext holeStored inferredVal = do
  -- With the real stored guids:
  writtenExpr <-
    fmap toPayload .
    ExprIRef.addProperties (Property.set holeStored) <$>
    writeExprMStored (Property.value holeStored) (intoStorePoint <$> inferredVal)
  let
    -- Replace the guids with consistent ones:

    -- The sugar convert must apply *inside* the forked transaction
    -- upon the *written* expr because we actually make use of the
    -- resulting actions (e.g: press ',' on a list hole result).
    -- However, the written expr goes crazy with new guids every time.
    --
    -- So, we do something a bit odd: Take the written expr with its
    -- in-tact stored allowing actions to be built correctly but
    -- replace the ipGuid/ipEntityId with determinstic/consistent
    -- pseudo-random generated ones that preserve proper animations
    -- and cursor navigation.

    makeConsistentPayload (False, (_, pl)) guid entityId = pl
      & ipEntityId .~ entityId
      & ipGuid .~ guid
    makeConsistentPayload (True, (_, pl)) _ _ = pl
    consistentExpr =
      writtenExpr
      <&> makeConsistentPayload
      & EntityId.randomizeExprAndParams (genFromHashable (void inferredVal))
  converted <-
    consistentExpr
    & ConvertM.convertSubexpression
    & ConvertM.run sugarContext
  return
    ( converted
    , consistentExpr
    , writtenExpr <&> snd
    )
  where
    intoStorePoint (inferred, (mStorePoint, a)) =
      (mStorePoint, (inferred, Lens.has Lens._Just mStorePoint, a))
    toPayload (stored, (inferred, wasStored, a)) = (,) wasStored $ (,) stored InputPayload
      { _ipEntityId = EntityId.ofValI $ Property.value stored
      , _ipGuid = IRef.guid $ ExprIRef.unValI $ Property.value stored
      , _ipInferred = inferred
      , _ipStored = Just stored
      , _ipData = a
      }

resultComplexityScore :: Val Infer.Payload -> [Int]
resultComplexityScore expr =
  [ length . show $ expr ^. V.payload . Infer.plType
  , length $ Foldable.toList expr
  ]

lambdas :: Val a -> [V.Lam (Val a)]
lambdas v =
  selfLams ++ recurse
  where
    selfLams =
      case v ^. V.body of
      V.BAbs lam -> [lam]
      _ -> []
    recurse = Foldable.concatMap lambdas (v ^. V.body)

idTranslations ::
  Val EntityId ->
  Val EntityId ->
  [(EntityId, EntityId)]
idTranslations src dest
  | V.alphaEq (void src) (void dest)
    = zip (Foldable.toList src) (Foldable.toList dest) ++
      zip (paramEntityIds src) (paramEntityIds dest)
  | otherwise = error "idTranslations of mismatching expressions"
  where
    lambdaParamEntityId (V.Lam paramId _) = EntityId.ofLambdaParam paramId
    paramEntityIds = map lambdaParamEntityId . lambdas

mkHoleResult ::
  (MonadA m, Monoid a) =>
  ConvertM.Context m ->
  InputPayload m dummy -> ExprIRef.ValIProperty m ->
  Val (MStorePoint m a) ->
  T m (Maybe (HoleResult MStoredName m a))
mkHoleResult sugarContext exprPl stored val =
  runMaybeT $ do
    (inferredVal, ctx) <-
      (`runStateT` (sugarContext ^. ConvertM.scInferContext))
      (SugarInfer.loadInferInto (exprPl ^. ipInferred) val)
    let newSugarContext = sugarContext & ConvertM.scInferContext .~ ctx
    ((fConverted, fConsistentExpr, fWrittenExpr), forkedChanges) <-
      lift $ Transaction.fork $
        writeConvertTypeChecked newSugarContext stored inferredVal
    return $ HoleResult
      { _holeResultComplexityScore = resultComplexityScore $ fst <$> inferredVal
      , _holeResultConverted = fConverted
      , _holeResultPick = mkPickedResult fConsistentExpr fWrittenExpr <$ Transaction.merge forkedChanges
      , _holeResultHasHoles = not . null $ orderedInnerHoles val
      }
  where
    mkPickedResult consistentExpr writtenExpr =
      PickedResult
      { _prMJumpTo =
        (orderedInnerHoles writtenExpr ^? Lens.traverse . V.payload . _2)
        <&> (^. ipGuid) &&& (^. ipEntityId)
      , _prIdTranslation =
        idTranslations
        (consistentExpr <&> (^. ipEntityId))
        (writtenExpr <&> EntityId.ofValI . Property.value . fst)
      }

randomizeNonStoredParamIds ::
  Random.StdGen -> ExprStorePoint m a -> ExprStorePoint m a
randomizeNonStoredParamIds gen =
  InputExpr.randomizeParamIdsG id nameGen Map.empty $ \_ _ pl -> pl
  where
    nameGen = InputExpr.onNgMakeName f $ InputExpr.randomNameGen gen
    f n _        prevEntityId (Just _, _) = (prevEntityId, n)
    f _ prevFunc prevEntityId pl@(Nothing, _) = prevFunc prevEntityId pl

writeExprMStored ::
  MonadA m =>
  ExprIRef.ValIM m ->
  ExprStorePoint m a ->
  T m (Val (ExprIRef.ValIM m, a))
writeExprMStored exprIRef exprMStorePoint = do
  key <- Transaction.newKey
  randomizeNonStoredParamIds (genFromHashable key) exprMStorePoint
    & Lens.mapped . Lens._1 . Lens._Just %~ unStorePoint
    & ExprIRef.writeValWithStoredSubexpressions exprIRef

orderedInnerHoles :: Val a -> [Val a]
orderedInnerHoles e =
  case e ^. V.body of
  V.BLeaf V.LHole -> [e]
  V.BApp (V.Apply func@(V.Val _ (V.BLeaf V.LHole)) arg) ->
      orderedInnerHoles arg ++ [func]
  body -> Foldable.concatMap orderedInnerHoles body
