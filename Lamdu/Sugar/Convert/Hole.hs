{-# LANGUAGE ConstraintKinds, OverloadedStrings, RankNTypes #-}

module Lamdu.Sugar.Convert.Hole
  ( convert, convertPlain, orderedInnerHoles
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Arrow ((&&&))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad (join, void, liftM)
import Control.Monad.ListT (ListT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT(..), evalState, mapStateT)
import Control.MonadA (MonadA)
import Data.Maybe (maybeToList)
import Data.Maybe.Utils (unsafeUnjust)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.String (IsString(..))
import Data.Traversable (traverse, sequenceA)
import Lamdu.Expr.IRef (DefI)
import Lamdu.Expr.Type (Type(..))
import Lamdu.Expr.Val (Val(..))
import Lamdu.Infer.Unify (unify)
import Lamdu.Sugar.Convert.Expression.Actions (addActions)
import Lamdu.Sugar.Convert.Monad (ConvertM)
import Lamdu.Sugar.Internal
import Lamdu.Sugar.Types
import Lamdu.Suggest (suggestValueWith)
import System.Random.Utils (genFromHashable)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Foldable as Foldable
import qualified Data.List.Class as ListClass
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.GenIds as GenIds
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.IRef.Infer as IRefInfer
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Convert.GetVar as ConvertGetVar
import qualified Lamdu.Sugar.Convert.Input as Input
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import qualified System.Random as Random

type T = Transaction

type ExprStorePoint m a = Val (Maybe (ExprIRef.ValI m), a)

convert ::
  (MonadA m, Monoid a) =>
  Input.Payload m a -> ConvertM m (ExpressionU m a)
convert exprPl =
  convertPlain Nothing exprPl
  <&> rPayload . plActions . Lens._Just . setToHole .~ AlreadyAHole

convertPlain ::
  (MonadA m, Monoid a) =>
  Maybe (Val (Input.Payload m a)) -> Input.Payload m a -> ConvertM m (ExpressionU m a)
convertPlain mInjectedArg exprPl =
  mkHole mInjectedArg exprPl
  <&> BodyHole
  >>= addActions exprPl
  <&> rPayload . plActions . Lens._Just . wrap .~ WrapNotAllowed

mkPaste ::
  MonadA m => ExprIRef.ValIProperty m -> ConvertM m (Maybe (T m EntityId))
mkPaste exprP = do
  clipboardsP <- ConvertM.codeAnchor Anchors.clipboards
  clipboards <- ConvertM.getP clipboardsP
  typeProtectedSetToVal <- ConvertM.typeProtectedSetToVal
  case clipboards of
    [] -> return Nothing
    (clipDefI : clips) ->
      return $ Just $ do
        Definition.BodyExpr (Definition.Expr pasteExpr _) <- Transaction.readIRef clipDefI
        Transaction.deleteIRef clipDefI
        Transaction.setP clipboardsP clips
        EntityId.ofValI <$> typeProtectedSetToVal exprP pasteExpr

mkWritableHoleActions ::
  (MonadA m) =>
  Maybe (Val (Input.Payload m a)) ->
  Input.Payload m a -> ExprIRef.ValIProperty m ->
  ConvertM m (HoleActions Guid m)
mkWritableHoleActions mInjectedArg exprPl stored = do
  sugarContext <- ConvertM.readContext
  mPaste <- mkPaste stored
  globals <-
    ConvertM.liftTransaction . Transaction.getP . Anchors.globals $
    sugarContext ^. ConvertM.scCodeAnchors
  let inferredScope = inferred ^. Infer.plScope
  pure HoleActions
    { _holePaste = mPaste
    , _holeScope =
      return $
      -- ^ We wrap this in a (T m) so that AddNames can place the
      -- name-getting penalty under a transaction that the GUI may
      -- avoid using
      (concatMap (getLocalScopeItems sugarContext) . Map.toList . Infer.scopeToTypeMap) inferredScope ++
      map getGlobalScopeItem (filter (/= sugarContext ^. ConvertM.scDefI) globals)
    , _holeResults = mkHoleResults mInjectedArg sugarContext exprPl stored
    , _holeGuid = UniqueId.toGuid $ ExprIRef.unValI $ Property.value stored
    }
  where
    inferred = exprPl ^. Input.inferred

-- Ignoring alpha-renames:
consistentExprIds :: EntityId -> Val (Guid -> EntityId -> a) -> Val a
consistentExprIds holeEntityId val =
  EntityId.randomizeExprAndParams gen val
  where
    gen =
      genFromHashable
        ( holeEntityId
        , void $ GenIds.randomizeParamIds (genFromHashable holeEntityId) val
        )

mkHoleSuggested :: MonadA m => EntityId -> Infer.Payload -> ConvertM m (HoleSuggested Guid m)
mkHoleSuggested holeEntityId inferred = do
  sugarContext <- ConvertM.readContext
  let
    mkConverted = do
      (inferredIVal, newCtx) <-
        IRefInfer.loadInferInto inferred suggestedVal
        & (`runStateT` (sugarContext ^. ConvertM.scInferContext))
        & runMaybeT
        <&> unsafeUnjust "Inference on inferred val must succeed"
      inferredIVal
        <&> Input.mkUnstoredPayload () . fst
        & consistentExprIds holeEntityId
        & ConvertM.convertSubexpression
        & ConvertM.run (sugarContext & ConvertM.scInferContext .~ newCtx)
  pure HoleSuggested
    { _hsValue = suggestedVal
    , _hsMakeConverted = mkConverted
    }
  where
    suggestedVal =
      (`evalState` (0 :: Int)) $
      suggestValueWith mkVar
      (inferred ^. Infer.plType)
    mkVar = do
      i <- State.get
      State.modify (+1)
      return . fromString $ "var" ++ show i

mkHole ::
  (MonadA m, Monoid a) =>
  Maybe (Val (Input.Payload m a)) ->
  Input.Payload m a -> ConvertM m (Hole Guid m (ExpressionU m a))
mkHole mInjectedArg exprPl = do
  mActions <- traverse (mkWritableHoleActions mInjectedArg exprPl) (exprPl ^. Input.mStored)
  suggested <- mkHoleSuggested (exprPl ^. Input.entityId) $ exprPl ^. Input.inferred
  pure Hole
    { _holeMActions = mActions
    , _holeSuggested = suggested
    , _holeMArg = Nothing
    }

getLocalScopeItems ::
  MonadA m => ConvertM.Context m -> (V.Var, Type) -> [ScopeItem Guid m]
getLocalScopeItems sugarContext (par, typeExpr) =
  ScopeItem
  { _siGetVar = ConvertGetVar.convertVar sugarContext par
  , _siVal = getParam
  } :
  map onScopeField
  (typeExpr ^.. ExprLens._TRecord . ExprLens.compositeTags)
  where
    getParam = P.var par
    onScopeField tag =
      ScopeItem
      { _siGetVar =
        GetVar
        { _gvName = UniqueId.toGuid tag
        , _gvJumpTo = error "Jump to on scope item??"
        , _gvVarType = GetFieldParameter
        }
      , _siVal = P.getField getParam tag
      }

-- TODO: Put the result in scopeGlobals in the caller, not here?
getGlobalScopeItem :: MonadA m => DefI m -> ScopeItem Guid m
getGlobalScopeItem defI =
    ScopeItem
    { _siGetVar =
      GetVar
      { _gvName = UniqueId.toGuid defI
      , _gvJumpTo = errorJumpTo
      , _gvVarType = GetDefinition
      }
    , _siVal = P.global $ ExprIRef.globalId defI
    }
  where
    errorJumpTo = error "Jump to on scope item??"

type HoleResultVal m a = Val (Infer.Payload, (Maybe (ExprIRef.ValI m), a))

markNotInjected :: HoleResultVal n () -> HoleResultVal n IsInjected
markNotInjected val = val <&> _2 . _2 .~ NotInjected

writeConvertTypeChecked ::
  (MonadA m, Monoid a) =>
  EntityId -> ConvertM.Context m -> ExprIRef.ValIProperty m ->
  HoleResultVal m a ->
  T m
  ( ExpressionU m a
  , Val (Input.Payload m a)
  , Val (ExprIRef.ValIProperty m, Input.Payload m a)
  )
writeConvertTypeChecked holeEntityId sugarContext holeStored inferredVal = do
  -- With the real stored guids:
  writtenExpr <-
    inferredVal
    <&> intoStorePoint
    & writeExprMStored (Property.value holeStored)
    <&> ExprIRef.addProperties (Property.set holeStored)
    <&> fmap toPayload
  let
    -- Replace the guids with consistent ones:

    -- The sugar convert must apply *inside* the forked transaction
    -- upon the *written* expr because we actually make use of the
    -- resulting actions (e.g: press ',' on a list hole result).
    -- However, the written expr goes crazy with new guids every time.
    --
    -- So, we do something a bit odd: Take the written expr with its
    -- in-tact stored allowing actions to be built correctly but
    -- replace the Input.guid/Input.entityId with determinstic/consistent
    -- pseudo-random generated ones that preserve proper animations
    -- and cursor navigation.

    makeConsistentPayload (False, (_, pl)) guid entityId = pl
      & Input.entityId .~ entityId
      & Input.guid .~ guid
    makeConsistentPayload (True, (_, pl)) _ _ = pl
    consistentExpr =
      writtenExpr
      <&> makeConsistentPayload
      & consistentExprIds holeEntityId
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
    toPayload (stored, (inferred, wasStored, a)) =
      (,) wasStored $ (,) stored $ Input.mkPayload a (inferred, stored)

resultTypeScore :: Type -> [Int]
resultTypeScore (T.TVar _) = [0]
resultTypeScore (T.TInst _ p) = 2 : maximum ([] : map resultTypeScore (Map.elems p))
resultTypeScore (T.TFun a r) = 2 : max (resultTypeScore a) (resultTypeScore r)
resultTypeScore (T.TRecord c) =
  compositeScore c
  where
    compositeScore (T.CEmpty) = [2]
    compositeScore (T.CVar _) = [1]
    compositeScore (T.CExtend _ t r) = 2 : max (resultTypeScore t) (compositeScore r)

resultScore :: Val Infer.Payload -> [Int]
resultScore (Val pl body) =
  bodyTopLevelScore : resultTypeScore (pl ^. Infer.plType) ++
  (body & Foldable.toList <&> resultScore & ([]:) & maximum)
  where
    bodyTopLevelScore =
      case body of
      V.BApp (V.Apply (Val _ (V.BLeaf V.LHole)) _) -> 10
      V.BLeaf V.LHole -> 1
      _ -> 0

idTranslations ::
  Val EntityId ->
  Val EntityId ->
  [(EntityId, EntityId)]
idTranslations src dest
  | V.alphaEq (void src) (void dest)
    = concat
      [ pairUp V.payload
      , pairUp params
      , pairUpTags ExprLens._BRecExtend EntityId.ofRecExtendTag
      , pairUpTags ExprLens._BGetField EntityId.ofGetFieldTag
      , pairUp getLambdaTagParams
      ]
  | otherwise = error "idTranslations of mismatching expressions"
  where
    pairUp l = zip (src ^.. ExprLens.subExprs . l) (dest ^.. ExprLens.subExprs . l)
    pairUpTags prism toEntityId =
      pairUp $
      Lens.filtered (Lens.has (V.body . prism)) . V.payload . Lens.to toEntityId
    getLambdaTagEntityIds (V.GetField (V.Val _ (V.BLeaf (V.LVar var))) tag) =
      [EntityId.ofLambdaTagParam var tag]
    getLambdaTagEntityIds _ = []
    getLambdaTagParams =
      V.body . ExprLens._BGetField . Lens.folding getLambdaTagEntityIds
    params =
      V.body . ExprLens._BAbs . V.lamParamId .
      Lens.to EntityId.ofLambdaParam

maybeTtoListT :: Monad m => MaybeT m a -> ListT m a
maybeTtoListT = ListClass.joinL . liftM (ListClass.fromList . maybeToList) . runMaybeT

eitherToListT :: Monad m => Either t a -> ListT m a
eitherToListT (Left _) = mempty
eitherToListT (Right x) = return x

applyFormArg :: (Infer.Payload, a) -> Val (Infer.Payload, a)
applyFormArg pl =
  Val pl $
  case pl ^. _1 . Infer.plType of
  T.TRecord T.CEmpty -> V.BLeaf V.LRecEmpty
  T.TRecord (T.CExtend f typ rest) ->
    V.BRecExtend $
      V.RecExtend f
      (Val (pl & _1 . Infer.plType .~ typ) (V.BLeaf V.LHole)) $
      applyFormArg (pl & _1 . Infer.plType .~ T.TRecord rest)
  _ -> V.BLeaf V.LHole

applyForms :: Val (Infer.Payload, a) -> [Val (Infer.Payload, a)]
applyForms v@(Val _ V.BAbs {}) = [v]
applyForms val =
  case val ^. V.payload . _1 . Infer.plType of
  T.TFun arg res ->
    applyForms $
    Val (plWithType res) $ V.BApp $ V.Apply val $
    applyFormArg (plWithType arg)
  _ -> []
  & (++ [val])
  where
    plWithType t =
      val ^. V.payload
      & _1 . Infer.plType .~ t

holeWrap :: Type -> Val (Infer.Payload, a) -> Val (Infer.Payload, a)
holeWrap resultType val =
  Val (pl & _1 . Infer.plType .~ resultType) $
  V.BApp $ V.Apply func val
  where
    pl = val ^. V.payload
    func = Val (pl & _1 . Infer.plType .~ funcType) $ V.BLeaf V.LHole
    funcType = T.TFun (pl ^. _1 . Infer.plType) resultType

replaceEachUnwrappedHole :: Applicative f => (a -> f (Val a)) -> Val a -> [f (Val a)]
replaceEachUnwrappedHole replaceHole =
  map fst . filter snd . (`runStateT` False) . go
  where
    go oldVal@(Val x body) = do
      alreadyReplaced <- State.get
      if alreadyReplaced
        then return (pure oldVal)
        else
          case body of
          V.BLeaf V.LHole ->
            join $ lift
              [ replace x
              , return $ pure oldVal
              ]
          V.BApp (V.Apply (Val f (V.BLeaf V.LHole)) arg@(Val _ (V.BLeaf V.LHole))) ->
            join $ lift
              [ replace f
                <&> fmap (Val x . V.BApp . (`V.Apply` arg))
              , return $ pure oldVal
              ]
          _ -> traverse go body <&> fmap (Val x) . sequenceA
    replace x =
      do
        State.put True
        return $ replaceHole x

stateEitherSequence :: Monad m => StateT s (Either l) r -> StateT s m (Either l r)
stateEitherSequence (StateT f) =
  StateT $ \s0 ->
  case f s0 of
  Right (r, s1) -> return (Right r, s1)
  Left l -> return (Left l, s0)

holeResultsInject ::
  Monad m =>
  Val (Input.Payload n a) -> HoleResultVal n () ->
  StateT Infer.Context (ListT m) (HoleResultVal n IsInjected)
holeResultsInject injectedArg val =
  do
    (Monoid.First (Just injectPointPl), filledVal) <-
      val
      & markNotInjected
      & replaceEachUnwrappedHole inject
      & ListClass.fromList
      & lift
    unify injectedType (injectPointPl ^. _1 . Infer.plType)
      & Infer.run
      & mapStateT eitherToListT
    return filledVal
  where
    onInjectedPayload pl =
        ( pl ^. Input.inferred
        , (pl ^? Input.mStored . Lens._Just . Property.pVal, NotInjected)
        )
    inject pl =
        ( Monoid.First (Just pl)
        , injectedArg
          <&> onInjectedPayload
          & V.payload . _2 . _2 .~ Injected
        )
    injectedType = injectedArg ^. V.payload . Input.inferred . Infer.plType

mkHoleResultVals ::
  MonadA m =>
  Maybe (Val (Input.Payload m a)) ->
  Input.Payload m dummy ->
  Val () ->
  StateT Infer.Context (ListT (T m)) (HoleResultVal m IsInjected)
mkHoleResultVals mInjectedArg exprPl base =
  do
    inferredBase <-
      IRefInfer.loadInferScope scopeAtHole base
      & mapStateT maybeTtoListT
      <&> Lens.traversed . _2 %~ (,) Nothing
    form <- lift $ ListClass.fromList $ applyForms inferredBase
    let formType = form ^. V.payload . _1 . Infer.plType
    injected <- maybe (return . markNotInjected) holeResultsInject mInjectedArg form
    unifyResult <-
      unify holeType formType
      & Infer.run
      & stateEitherSequence
    return $
      case unifyResult of
      Right _ -> injected
      Left _ -> holeWrap holeType injected
  where
    holeType = exprPl ^. Input.inferred . Infer.plType
    scopeAtHole = exprPl ^. Input.inferred . Infer.plScope

mkHoleResult ::
  MonadA m =>
  ConvertM.Context m -> EntityId ->
  ExprIRef.ValIProperty m -> HoleResultVal m IsInjected ->
  T m (HoleResult Guid m)
mkHoleResult sugarContext entityId stored val =
  do
    ((fConverted, fConsistentExpr, fWrittenExpr), forkedChanges) <-
      Transaction.fork $
      writeConvertTypeChecked entityId
      sugarContext stored val
    return
      HoleResult
      { _holeResultConverted = fConverted
      , _holeResultPick = mkPickedResult fConsistentExpr fWrittenExpr <$ Transaction.merge forkedChanges
      , _holeResultHoleTarget =
        orderedInnerHoles fConsistentExpr ^? Lens.traversed . V.payload . Input.entityId
      }
  where
    mkPickedResult consistentExpr writtenExpr =
      PickedResult
      { _prMJumpTo =
        (orderedInnerHoles writtenExpr ^? Lens.traverse . V.payload . _2)
        <&> (^. Input.guid) &&& (^. Input.entityId)
      , _prIdTranslation =
        idTranslations
        (consistentExpr <&> (^. Input.entityId))
        (writtenExpr <&> EntityId.ofValI . Property.value . fst)
      }

mkHoleResults ::
  MonadA m =>
  Maybe (Val (Input.Payload m a)) ->
  ConvertM.Context m ->
  Input.Payload m dummy -> ExprIRef.ValIProperty m ->
  Val () ->
  ListT (T m) (HoleResultScore, T m (HoleResult Guid m))
mkHoleResults mInjectedArg sugarContext exprPl stored base =
  do
    (val, inferContext) <-
      mkHoleResultVals mInjectedArg exprPl base
      & (`runStateT` (sugarContext ^. ConvertM.scInferContext))
    let newSugarContext = sugarContext & ConvertM.scInferContext .~ inferContext
    return
      ( resultScore (fst <$> val)
      , mkHoleResult newSugarContext (exprPl ^. Input.entityId) stored val
      )

randomizeNonStoredParamIds ::
  Random.StdGen -> ExprStorePoint m a -> ExprStorePoint m a
randomizeNonStoredParamIds gen =
  GenIds.randomizeParamIdsG id nameGen Map.empty $ \_ _ pl -> pl
  where
    nameGen = GenIds.onNgMakeName f $ GenIds.randomNameGen gen
    f n _        prevEntityId (Just _, _) = (prevEntityId, n)
    f _ prevFunc prevEntityId pl@(Nothing, _) = prevFunc prevEntityId pl

writeExprMStored ::
  MonadA m =>
  ExprIRef.ValI m ->
  ExprStorePoint m a ->
  T m (Val (ExprIRef.ValI m, a))
writeExprMStored exprIRef exprMStorePoint = do
  key <- Transaction.newKey
  exprMStorePoint
    & randomizeNonStoredParamIds (genFromHashable key)
    & ExprIRef.writeValWithStoredSubexpressions exprIRef

orderedInnerHoles :: Val a -> [Val a]
orderedInnerHoles e =
  case e ^. V.body of
  V.BLeaf V.LHole -> [e]
  V.BApp (V.Apply func@(V.Val _ (V.BLeaf V.LHole)) arg) ->
      orderedInnerHoles arg ++ [func]
  body -> Foldable.concatMap orderedInnerHoles body
