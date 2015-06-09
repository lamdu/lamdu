{-# LANGUAGE ConstraintKinds, OverloadedStrings, RankNTypes #-}

module Lamdu.Sugar.Convert.Hole
  ( convert, convertPlain, orderedInnerHoles
  ) where

import           Control.Applicative (Applicative(..), (<$>), (<$), (<|>))
import qualified Control.Applicative as Applicative
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad (join, void, liftM)
import           Control.Monad.ListT (ListT)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.State (StateT(..), evalState, mapStateT)
import qualified Control.Monad.Trans.State as State
import           Control.MonadA (MonadA)
import qualified Data.Foldable as Foldable
import qualified Data.List.Class as ListClass
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import           Data.Monoid (Monoid(..))
import qualified Data.Monoid as Monoid
import           Data.Store.Guid (Guid)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.String (IsString(..))
import           Data.Traversable (traverse, sequenceA)
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Expr.GenIds as GenIds
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.IRef as ExprIRef
import qualified Lamdu.Expr.IRef.Infer as IRefInfer
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Pure as P
import           Lamdu.Expr.Type (Type(..))
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.UniqueId as UniqueId
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.Infer as Infer
import           Lamdu.Infer.Unify (unify)
import           Lamdu.Infer.Update (update)
import qualified Lamdu.Infer.Update as Update
import           Lamdu.Sugar.Convert.Expression.Actions (addActions)
import qualified Lamdu.Sugar.Convert.GetVar as ConvertGetVar
import qualified Lamdu.Sugar.Convert.Input as Input
import           Lamdu.Sugar.Convert.Monad (ConvertM)
import qualified Lamdu.Sugar.Convert.Monad as ConvertM
import           Lamdu.Sugar.Internal
import qualified Lamdu.Sugar.Internal.EntityId as EntityId
import           Lamdu.Sugar.Types
import           Lamdu.Suggest (suggestValueWith)
import qualified System.Random as Random
import           System.Random.Utils (genFromHashable)
import           Text.PrettyPrint.HughesPJClass (prettyShow)

type T = Transaction

type ExprStorePoint m a = Val (Maybe (ExprIRef.ValI m), a)

convert ::
  (MonadA m, Monoid a) =>
  Input.Payload m a -> ConvertM m (ExpressionU m a)
convert exprPl =
  convertPlain Nothing exprPl
  <&> rPayload . plActions . Lens._Just . setToHole .~ AlreadyAHole
  <&> rPayload . plActions . Lens._Just . cut .~ Nothing

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
      (concatMap (getLocalScopeGetVars sugarContext) . Map.toList . Infer.scopeToTypeMap) inferredScope ++
      map getGlobalScopeGetVar (filter (/= sugarContext ^. ConvertM.scDefI) globals)
    , _holeResults = mkHoleResults mInjectedArg sugarContext exprPl stored
    , _holeGuid = UniqueId.toGuid $ ExprIRef.unValI $ Property.value stored
    }
  where
    inferred = exprPl ^. Input.inferred

-- Ignoring alpha-renames:
consistentExprIds :: EntityId -> Val (EntityId -> a) -> Val a
consistentExprIds = EntityId.randomizeExprAndParams . genFromHashable

mkHoleSuggested :: Infer.Payload -> Val ()
mkHoleSuggested inferred =
  inferred ^. Infer.plType
  & suggestValueWith mkVar
  & (`evalState` (0 :: Int))
  where
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
  pure Hole
    { _holeMActions = mActions
    , _holeSuggested = mkHoleSuggested (exprPl ^. Input.inferred)
    , _holeMArg = Nothing
    }

getLocalScopeGetVars ::
  MonadA m => ConvertM.Context m -> (V.Var, Type) -> [ScopeGetVar Guid m]
getLocalScopeGetVars sugarContext (par, typeExpr) =
  ScopeGetVar
  { _sgvGetVar = ConvertGetVar.convertVar sugarContext par typeExpr
  , _sgvVal = P.var par
  } :
  map mkFieldParam fieldTags
  where
    fieldTags =
      ( sugarContext ^@..
        ConvertM.scTagParamInfos .>
        ( Lens.itraversed <.
          Lens.to ConvertM.tpiFromParameters ) <.
          Lens.filtered (== par)
      ) <&> fst
    mkFieldParam tag =
      ScopeGetVar
      { _sgvGetVar =
        GetVarNamed NamedVar
        { _nvName = UniqueId.toGuid tag
        , _nvJumpTo = error "Jump to on scope item??"
        , _nvVarType = GetFieldParameter
        }
      , _sgvVal = P.getField (P.var par) tag
      }

-- TODO: Put the result in scopeGlobals in the caller, not here?
getGlobalScopeGetVar :: MonadA m => DefI m -> ScopeGetVar Guid m
getGlobalScopeGetVar defI =
    ScopeGetVar
    { _sgvGetVar =
      GetVarNamed NamedVar
      { _nvName = UniqueId.toGuid defI
      , _nvJumpTo = errorJumpTo
      , _nvVarType = GetDefinition
      }
    , _sgvVal = P.global $ ExprIRef.globalId defI
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

    makeConsistentPayload (False, (_, pl)) entityId = pl
      & Input.entityId .~ entityId
    makeConsistentPayload (True, (_, pl)) _ = pl
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
      -- TODO: Evaluate hole results instead of Map.empty?
      (,) wasStored $ (,) stored $ Input.mkPayload a inferred Map.empty Map.empty stored

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
  Val (EntityId, Type) ->
  Val EntityId ->
  [(EntityId, EntityId)]
idTranslations consistentExpr dest
  | V.alphaEq (void src) (void dest)
    = concat
      [ pairUp V.payload
      , pairUp params
      , pairUpTags ExprLens._BRecExtend EntityId.ofRecExtendTag
      , pairUpTags ExprLens._BGetField EntityId.ofGetFieldTag
      , pairUpLambdaRecordParams (consistentExpr <&> snd) dest
      ]
  | otherwise = error "idTranslations of mismatching expressions"
  where
    pairUpLambdaRecordParams aVal bVal =
      case (aVal, bVal) of
      (V.Val srcType (V.BAbs (V.Lam avar _)),
       V.Val _ (V.BAbs (V.Lam bvar _)))
        -- TODO: Use a _TRecord prism alternative that verifies the
        -- record is closed
        -> [ ( EntityId.ofLambdaTagParam avar tag
             , EntityId.ofLambdaTagParam bvar tag
             )
           | tag <-
               srcType ^..
               ExprLens._TFun . _1 . ExprLens._TRecord . ExprLens.compositeTags
           ] ++ recurse
      _ -> recurse
      where
        recurse =
          zipWith pairUpLambdaRecordParams
          (aVal ^.. V.body . Lens.folded)
          (bVal ^.. V.body . Lens.folded)
          & concat
    src = consistentExpr <&> fst
    pairUp l = zip (src ^.. ExprLens.subExprs . l) (dest ^.. ExprLens.subExprs . l)
    pairUpTags prism toEntityId =
      pairUp $
      Lens.filtered (Lens.has (V.body . prism)) . V.payload . Lens.to toEntityId
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
      (Val (plSameScope typ) (V.BLeaf V.LHole)) $
      applyFormArg (plSameScope (T.TRecord rest))
  _ -> V.BLeaf V.LHole
  where
    plSameScope typ = pl & _1 . Infer.plType .~ typ

applyForms ::
  MonadA m =>
  a -> Val (Infer.Payload, a) ->
  StateT Infer.Context (ListT m) (Val (Infer.Payload, a))
applyForms _ v@(Val _ V.BAbs {}) = return v
applyForms empty val =
  case inferPl ^. Infer.plType of
  T.TFun arg res ->
    applyForms empty $
    Val (plSameScope res) $ V.BApp $ V.Apply val $
    applyFormArg (plSameScope arg)
  T.TVar tv
    | any (`Lens.has` val)
      [ ExprLens.valVar
      , ExprLens.valGetField . V.getFieldRecord . ExprLens.valVar
      ] ->
      -- a variable that's compatible with a function type
      do
        arg <- freshVar "af"
        res <- freshVar "af"
        let varTyp = T.TFun arg res
        unify varTyp (T.TVar tv)
          & Infer.run & mapStateT assertSuccess
        return $ Val (plSameScope res) $ V.BApp $ V.Apply val $
          Val (plSameScope arg) (V.BLeaf V.LHole)
  _ -> Applicative.empty
  <|> return val
  where
    assertSuccess (Left err) =
      fail $
      "Unify of a tv with function type should always succeed, but failed: " ++
      prettyShow err
    assertSuccess (Right x) = return x
    freshVar = Infer.run . Infer.freshInferredVar
    inferPl = val ^. V.payload . _1
    plSameScope t = (inferPl & Infer.plType .~ t, empty)

holeWrap :: a -> Type -> Val (Infer.Payload, a) -> Val (Infer.Payload, a)
holeWrap empty resultType val =
  Val (plSameScope resultType, empty) $
  V.BApp $ V.Apply func val
  where
    plSameScope typ = inferPl & Infer.plType .~ typ
    inferPl = val ^. V.payload . _1
    func = Val (plSameScope funcType, empty) $ V.BLeaf V.LHole
    funcType = T.TFun (inferPl ^. Infer.plType) resultType

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
    form <- applyForms (Nothing, ()) inferredBase
    let formType = form ^. V.payload . _1 . Infer.plType
    injected <- maybe (return . markNotInjected) holeResultsInject mInjectedArg form
    unifyResult <- unify holeType formType & liftInfer
    injectedUpdated <- Update.inferredVal injected & liftUpdate
    case unifyResult of
      Right {} -> return injectedUpdated
      Left {} ->
        do
          updatedHoleType <- update holeType & liftUpdate
          return $ holeWrap (Nothing, NotInjected) updatedHoleType injectedUpdated
  where
    liftUpdate = State.gets . Update.run
    liftInfer = stateEitherSequence . Infer.run
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
        <&> (^. Input.entityId)
      , _prIdTranslation =
        idTranslations
        ( consistentExpr <&>
          \input ->
          ( input ^. Input.entityId
          , input ^. Input.inferred . Infer.plType
          ) )
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
