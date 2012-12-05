{-# LANGUAGE OverloadedStrings, ConstraintKinds, TypeFamilies #-}
module Editor.CodeEdit.Sugar
  ( Definition(..), DefinitionBody(..)
  , ListItemActions(..), itemAddNext, itemDelete
  , FuncParamActions(..), fpListItemActions, fpGetExample
  , DefinitionExpression(..), DefinitionContent(..), DefinitionNewType(..)
  , DefinitionBuiltin(..)
  , Actions(..)
  , ExpressionBody(..)
  , Payload(..), plInferredTypes, plActions, plNextHole
  , ExpressionP(..), rGuid, rExpressionBody, rPayload
  , Expression
  , WhereItem(..)
  , Func(..)
  , FuncParam(..), fpGuid, fpHiddenLambdaGuid, fpType, fpMActions
  , Pi(..)
  , Section(..)
  , Hole(..), HoleActions(..), HoleResult, holeResultHasHoles
  , LiteralInteger(..)
  , Inferred(..)
  , Polymorphic(..)
  , HasParens(..)
  , loadConvertDefinition
  -- , loadConvertExpression
  , removeTypes
  ) where

import Control.Applicative ((<$>), Applicative(..))
import Control.Arrow (first)
import Control.Lens ((^.))
import Control.Monad ((<=<), liftM, join, mplus, void, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (runState)
import Control.Monad.Trans.Writer (runWriter)
import Data.Function (on)
import Data.List.Utils (sortOn)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Monoid (Any(..))
import Data.Store.Guid (Guid)
import Data.Tuple (swap)
import Editor.CodeEdit.Sugar.Config (SugarConfig)
import Editor.CodeEdit.Sugar.Infer (InferredWC, NoInferred, Stored, NoStored)
import Editor.CodeEdit.Sugar.Monad (SugarM)
import Editor.CodeEdit.Sugar.Types -- see export list
import Editor.Data.Infer.Conflicts (InferredWithConflicts(..), iwcInferredTypes, iwcInferredValues)
import System.Random (RandomGen)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.Cache as Cache
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Data.Traversable as Traversable
import qualified Editor.Anchors as Anchors
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.CodeEdit.Sugar.Infer as SugarInfer
import qualified Editor.CodeEdit.Sugar.Monad as SugarM
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef
import qualified Editor.Data.Infer as Infer
import qualified Editor.Data.Load as Load
import qualified Editor.Data.Ops as DataOps
import qualified System.Random as Random
import qualified System.Random.Utils as RandomUtils

type PayloadMM m = SugarInfer.Payload (Maybe InferredWC) (Maybe (Stored (T m)))
type DefI = DataIRef.DefinitionIRef
type Convertor m =
  Data.Expression DefI (PayloadMM m) ->
  SugarM m (Expression m)

mkCutter :: m ~ Anchors.ViewM => DataIRef.Expression -> T m Guid -> T m Guid
mkCutter iref replaceWithHole = do
  Anchors.modP Anchors.clipboards (iref:)
  replaceWithHole

mkActions :: m ~ Anchors.ViewM => Stored (T m) -> Actions m
mkActions stored =
  Actions
  { giveAsArg = guidify $ DataOps.giveAsArg stored
  , replace = doReplace
  , cut = mkCutter (Property.value stored) doReplace
  , giveAsArgToOperator = guidify . DataOps.giveAsArgToOperator stored
  }
  where
    guidify = liftM DataIRef.exprGuid
    doReplace = guidify $ DataOps.replaceWithHole stored

mkGen :: Int -> Int -> Guid -> Random.StdGen
mkGen select count =
  Random.mkStdGen . (+select) . (*count) . BinaryUtils.decodeS . Guid.bs

resultGuid ::
  Data.Expression def (SugarInfer.Payload inferred stored) -> Guid
resultGuid = Lens.view (Data.ePayload . SugarInfer.plGuid)

resultStored ::
  Data.Expression def (SugarInfer.Payload inferred stored) -> stored
resultStored = Lens.view (Data.ePayload . SugarInfer.plStored)

resultInferred ::
  Data.Expression def (SugarInfer.Payload inferred stored) -> inferred
resultInferred = Lens.view (Data.ePayload . SugarInfer.plInferred)

toPayloadMM :: SugarInfer.Payload NoInferred NoStored -> PayloadMM m
toPayloadMM =
  Lens.set SugarInfer.plInferred Nothing .
  Lens.set SugarInfer.plStored Nothing

mkExpression ::
  m ~ Anchors.ViewM =>
  Data.Expression DefI (SugarInfer.Payload (Maybe InferredWC) (Maybe (Stored (T m)))) ->
  ExpressionBody m (Expression m) -> SugarM m (Expression m)
mkExpression result expr = do
  inferredTypesRefs <-
    zipWithM
    ( fmap (convertExpressionI . fmap toPayloadMM)
    . SugarInfer.resultFromPure
    ) seeds types
  return
    Expression
    { _rGuid = resultGuid result
    , _rExpressionBody = expr
    , _rPayload = Payload
      { _plInferredTypes = inferredTypesRefs
      , _plActions = mkActions <$> resultStored result
      , _plNextHole = Nothing
      }
    }
  where
    seeds = RandomUtils.splits . mkGen 0 3 $ resultGuid result
    types = maybe [] iwcInferredTypes $ resultInferred result

mkDelete
  :: Monad m
  => Stored (T m)
  -> Stored (T m)
  -> T m Guid
mkDelete parentP replacerP = do
  Property.set parentP replacerI
  return $ DataIRef.exprGuid replacerI
  where
    replacerI = Property.value replacerP

mkFuncParamActions ::
  m ~ Anchors.ViewM =>
  SugarM.Context ->
  Data.Expression DefI (PayloadMM m) ->
  SugarInfer.Stored (T m) ->
  Data.Lambda (SugarInfer.Stored (T m)) ->
  Stored (T m) ->
  FuncParamActions m
mkFuncParamActions
  _ctx bodyI lambdaProp (Data.Lambda param _paramType _) replacerProp =
  FuncParamActions
  { _fpListItemActions =
    ListItemActions
    { _itemDelete = do
        mapM_ deleteIfParamRef $ Data.subExpressions bodyI
        mkDelete lambdaProp replacerProp
    , _itemAddNext = liftM fst $ DataOps.lambdaWrap replacerProp
    }
  , _fpGetExample =
      return .
      Expression (Guid.augment "EXAMPLE" param)
      (ExpressionAtom "Examples not implemented") $
      Payload [] Nothing Nothing
      -- exampleP <-
      --   -- TODO: move to Anchors
      --   lift . Anchors.nonEmptyAssocDataRef "example" param .
      --   DataIRef.newExprBody $ Data.ExpressionLeaf Data.Hole
      -- exampleLoaded <- lift $ Load.loadExpressionClosure exampleP
      -- inferredExample <-
      --   -- TODO: Nothing -> Just defI
      --   SugarInfer.inferLoadedExpression ??gen?? Nothing exampleLoaded newInferNode
      -- lift $ convertStoredExpression (inferredExample ^. SugarInfer.ilrExpr)
      --   ctx { SugarM.scInferState = inferredExample ^. SugarInfer.ilrInferContext }
  }
  where
    -- (paramTypeIWC, _) = paramType
    deleteIfParamRef expr =
      case (resultStored expr, expr ^. Data.eValue) of
      (Just prop, Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef p)))
        | p == param -> void $ DataOps.replaceWithHole prop
      _ -> return ()
    -- scope = Infer.nScope . Infer.iPoint $ iwcInferred lambdaIWC
    -- paramTypeRef =
    --   Infer.tvVal . Infer.nRefs . Infer.iPoint $ iwcInferred paramTypeIWC
    -- newInferNode =
    --   Infer.newTypedNodeWithScope scope paramTypeRef $ SugarM.scInferState ctx

convertLambda ::
  m ~ Anchors.ViewM =>
  Data.Lambda (Data.Expression DefI (PayloadMM m)) ->
  Data.Expression DefI (PayloadMM m) ->
  SugarM m (FuncParam m (Expression m), Expression m)
convertLambda lam@(Data.Lambda param paramTypeI bodyI) expr = do
  sBody <- convertExpressionI bodyI
  typeExpr <- convertExpressionI paramTypeI
  ctx <- SugarM.readContext
  let
    fp = FuncParam
      { _fpGuid = param
      , _fpHiddenLambdaGuid = Nothing
      , _fpType = removeRedundantTypes typeExpr
      , _fpMActions =
        mkFuncParamActions ctx bodyI
        <$> resultStored expr
        <*> Traversable.mapM resultStored lam
        <*> resultStored bodyI
      }
  return (fp, sBody)

convertFunc
  :: m ~ Anchors.ViewM
  => Data.Lambda (Data.Expression DefI (PayloadMM m))
  -> Convertor m
convertFunc lambda exprI = do
  (param, sBody) <- convertLambda lambda exprI
  mkExpression exprI .
    ExpressionFunc DontHaveParens $
    case sBody ^. rExpressionBody of
      ExpressionFunc _ (Func nextParams body) ->
        case nextParams of
        [] -> error "Func must have at least 1 param!"
        (nextParam : _) ->
          Func (deleteToNextParam nextParam param : nextParams) body
      _ -> Func [param] sBody
  where
    deleteToNextParam nextParam =
      Lens.set (fpMActions . Lens.mapped . fpListItemActions .  itemDelete . Lens.sets liftM) $ nextParam ^. fpGuid

convertPi
  :: m ~ Anchors.ViewM
  => Data.Lambda (Data.Expression DefI (PayloadMM m))
  -> Convertor m
convertPi lambda exprI = do
  (param, sBody) <- convertLambda lambda exprI
  mkExpression exprI $ ExpressionPi DontHaveParens
    Pi
    { pParam = Lens.over fpType addApplyChildParens param
    , pResultType = removeRedundantTypes sBody
    }

addParens :: ExpressionP m pl -> ExpressionP m pl
addParens =
  Lens.over rExpressionBody addParensBody
  where
    addParensBody (ExpressionInferred (Inferred val hole)) =
      ExpressionInferred $ Inferred (addParens val) hole
    addParensBody (ExpressionPolymorphic (Polymorphic g compact full)) =
      ExpressionPolymorphic . Polymorphic g compact $
      addParens full
    addParensBody x = Lens.set eHasParens HaveParens x

addApplyChildParens :: Expression m -> Expression m
addApplyChildParens x =
  case x ^. rExpressionBody of
  ExpressionApply{} -> x
  ExpressionPolymorphic{} -> x
  _ -> addParens x

isPolymorphicFunc :: Data.Expression DefI (PayloadMM m) -> Bool
isPolymorphicFunc funcI =
  maybe False
  (Data.isDependentPi . Infer.iType . iwcInferred) $
  resultInferred funcI

convertApply :: m ~ Anchors.ViewM => Data.Apply (Data.Expression DefI (PayloadMM m)) -> Convertor m
convertApply (Data.Apply funcI argI) exprI = do
  funcS <- convertExpressionI funcI
  argS <- convertExpressionI argI
  let apply = Data.Apply (funcS, funcI) (argS, argI)
  case funcS ^. rExpressionBody of
    ExpressionSection _ section ->
      applyOnSection section apply exprI
    _ ->
      convertApplyPrefix apply exprI

removeInferredTypes :: Expression m -> Expression m
removeInferredTypes = Lens.set (rPayload . plInferredTypes) []

removeRedundantTypes :: Expression m -> Expression m
removeRedundantTypes =
  Lens.over (rPayload . plInferredTypes) removeIfNoErrors
  where
    removeIfNoErrors [_] = []
    removeIfNoErrors xs = xs

setNextHole :: Expression m -> Expression m -> Expression m
setNextHole possibleHole =
  case possibleHole ^. rExpressionBody of
  ExpressionHole{} ->
    (fmap . Lens.over plNextHole . flip mplus . Just) possibleHole
  _ -> id

applyOnSection ::
  m ~ Anchors.ViewM =>
  Section (Expression m) -> Data.Apply (Expression m, Data.Expression DefI (PayloadMM m)) -> Convertor m
applyOnSection (Section Nothing op Nothing) (Data.Apply (_, funcI) arg@(argRef, _)) exprI
  | isPolymorphicFunc funcI = do
    newOpRef <-
      convertApplyPrefix (Data.Apply (op, funcI) arg) exprI
    mkExpression exprI . ExpressionSection DontHaveParens $
      Section Nothing (removeRedundantTypes newOpRef) Nothing
  | otherwise =
    mkExpression exprI . ExpressionSection DontHaveParens $
    Section (Just (addApplyChildParens argRef)) op Nothing
applyOnSection (Section (Just left) op Nothing) (Data.Apply _ (argRef, _)) exprI =
  mkExpression exprI . ExpressionSection DontHaveParens $
  on (Section . Just) (setNextHole right) left op (Just right)
  where
    -- TODO: Handle left/right-associativity
    isSameOp (ExpressionPolymorphic p0) (ExpressionPolymorphic p1) =
      on (==) pCompact p0 p1
    isSameOp (ExpressionGetVariable v0) (ExpressionGetVariable v1) =
      v0 == v1
    isSameOp _ _ = False
    right =
      case argRef ^. rExpressionBody of
      ExpressionSection _ (Section (Just _) rightOp (Just _))
        | on isSameOp (Lens.view rExpressionBody) op rightOp -> argRef
      _ -> addApplyChildParens argRef
applyOnSection _ apply exprI = convertApplyPrefix apply exprI

convertApplyPrefix ::
  m ~ Anchors.ViewM =>
  Data.Apply (Expression m, Data.Expression DefI (PayloadMM m)) -> Convertor m
convertApplyPrefix (Data.Apply (funcRef, funcI) (argRef, _)) exprI
  | isPolymorphicFunc funcI =
    case funcRef ^. rExpressionBody of
    ExpressionPolymorphic (Polymorphic g compact full) ->
      makePolymorphic g compact =<< makeApply full
    ExpressionGetVariable getVar ->
      makePolymorphic (resultGuid funcI) getVar =<< makeFullApply
    _ -> makeFullApply
  | otherwise = makeFullApply
  where
    newArgRef = addParens argRef
    newFuncRef =
      setNextHole newArgRef .
      addApplyChildParens .
      removeRedundantTypes $
      funcRef
    expandedGuid = Guid.combine (resultGuid exprI) $ Guid.fromString "polyExpanded"
    makeFullApply = makeApply newFuncRef
    makeApply f =
      mkExpression exprI . ExpressionApply DontHaveParens $
      Data.Apply f newArgRef
    makePolymorphic g compact fullExpression =
      mkExpression exprI $ ExpressionPolymorphic Polymorphic
        { pFuncGuid = g
        , pCompact = compact
        , pFullExpression =
          Lens.set rGuid expandedGuid $ removeInferredTypes fullExpression
        }

convertGetVariable :: m ~ Anchors.ViewM => Data.VariableRef DefI -> Convertor m
convertGetVariable varRef exprI = do
  isInfix <- SugarM.liftTransaction $ Infix.isInfixVar varRef
  getVarExpr <-
    mkExpression exprI $ ExpressionGetVariable varRef
  if isInfix
    then
      mkExpression exprI .
      ExpressionSection HaveParens $
      Section Nothing (removeInferredTypes getVarExpr) Nothing
    else return getVarExpr

mkPaste ::
  m ~ Anchors.ViewM =>
  Stored (T m) -> SugarM m (Maybe (T m Guid))
mkPaste exprP = do
  clipboardsP <- SugarM.liftTransaction Anchors.clipboards
  let
    mClipPop =
      case Property.value clipboardsP of
      [] -> Nothing
      (clip : clips) -> Just (clip, Property.set clipboardsP clips)
  return $ fmap (doPaste (Property.set exprP)) mClipPop
  where
    doPaste replacer (clip, popClip) = do
      ~() <- popClip
      ~() <- replacer clip
      return $ DataIRef.exprGuid clip

pureHole :: Data.Expression DefI ()
pureHole = Data.pureExpression $ Data.ExpressionLeaf Data.Hole

countArrows :: Data.Expression DefI () -> Int
countArrows Data.Expression
  { Data._eValue =
    Data.ExpressionPi (Data.Lambda _ _ resultType)
  } = 1 + countArrows resultType
countArrows _ = 0

-- TODO: Return a record, not a tuple
countPis :: Data.Expression DefI () -> (Int, Int)
countPis e@Data.Expression
  { Data._eValue =
    Data.ExpressionPi (Data.Lambda _ _ resultType)
  }
  | Data.isDependentPi e = first (1+) $ countPis resultType
  | otherwise = (0, 1 + countArrows resultType)
countPis _ = (0, 0)

applyForms
  :: Data.Expression DefI ()
  -> Data.Expression DefI () -> [Data.Expression DefI ()]
applyForms _ e@Data.Expression{ Data._eValue = Data.ExpressionLambda {} } =
  [e]
applyForms exprType expr =
  reverse . take (1 + arrows) $ iterate addApply withDepPisApplied
  where
    withDepPisApplied = iterate addApply expr !! depPis
    (depPis, arrows) = countPis exprType
    addApply = Data.pureExpression . (`Data.makeApply` pureHole)

-- Fill partial holes in an expression. Parital holes are those whose
-- inferred (filler) value itself is not complete, so will not be a
-- useful auto-inferred value. By auto-filling those, we allow the
-- user a chance to access all the partiality that needs filling more
-- easily.
fillPartialHolesInExpression ::
  Monad m =>
  (Data.Expression DefI () ->
   m (Maybe (Data.Expression DefI (Infer.Inferred DefI)))) ->
  Data.Expression DefI (Infer.Inferred DefI) ->
  m [Data.Expression DefI (Infer.Inferred DefI)]
fillPartialHolesInExpression check oldExpr =
  liftM ((++ [oldExpr]) . maybeToList) .
  recheck . runWriter $ fillHoleExpr oldExpr
  where
    recheck (newExpr, Any True) = check newExpr
    recheck (_, Any False) = return Nothing
    fillHoleExpr expr@(Data.Expression (Data.ExpressionLeaf Data.Hole) inferred) =
      let inferredVal = void $ Infer.iValue inferred
      in
        case inferredVal ^. Data.eValue of
        Data.ExpressionLeaf Data.Hole -> return $ void expr
        _ | isCompleteType inferredVal -> return $ void expr
          | otherwise -> do
            -- Hole inferred value has holes to fill, no use leaving it as
            -- auto-inferred, just fill it:
            Writer.tell $ Any True
            return inferredVal
    fillHoleExpr (Data.Expression body _) =
      liftM Data.pureExpression $ Traversable.mapM fillHoleExpr body

resultComplexityScore :: HoleResult -> Int
resultComplexityScore =
  sum . map (subtract 2 . length . Foldable.toList . Infer.iType) .
  Foldable.toList

inferApplyForms ::
  Monad m =>
  (Data.Expression DefI () -> T m [HoleResult]) -> Data.Expression DefI () ->
  (Infer.InferNode DefI, Infer.Context DefI) -> T m [HoleResult]
inferApplyForms processRes expr (node, inferContext) =
  liftM (sortOn resultComplexityScore) . makeApplyForms =<<
  SugarInfer.inferMaybe_ expr inferContext node
  where
    makeApplyForms Nothing = return []
    makeApplyForms (Just i) =
      liftM concat . mapM processRes $
      (applyForms . void) (Infer.iType (i ^. Data.ePayload)) expr

convertInferredHoleH ::
  m ~ Anchors.ViewM =>
  SugarM.Context -> Maybe (T m Guid) ->
  InferredWC -> Convertor m
convertInferredHoleH
  sugarContext mPaste iwc exprI =
    chooseHoleType (iwcInferredValues iwc) plainHole inferredHole
  where
    eGuid = resultGuid exprI
    SugarM.Context
      { SugarM.scHoleInferState = inferState
      , SugarM.scConfig = config
      , SugarM.scMContextHash = contextHash
      } = sugarContext
    inferred = iwcInferred iwc
    scope = Infer.nScope $ Infer.iPoint inferred
    check expr = SugarInfer.inferMaybe_ expr inferState $ Infer.iPoint inferred

    memoBy expr act = Cache.memoS (const act) (expr, eGuid, contextHash)

    inferResults processRes expr =
      memoBy expr . inferApplyForms processRes expr .
      (`runState` inferState) $ Infer.newNodeWithScope scope
    onScopeElement (param, _typeExpr) = param
    mkHole processRes = Hole
      { holeScope =
        map onScopeElement . Map.toList $ Infer.iScope inferred
      , holeInferResults = inferResults processRes
      , holeMActions = mkHoleActions <$> resultStored exprI
      }
    mkHoleActions stored =
      HoleActions
      { holePickResult = pickResult eGuid stored
      , holePaste = mPaste
      , holeConvertResult = convertHoleResult config
      }
    filledHole =
      mkHole $
      maybe (return []) (fillPartialHolesInExpression check) <=< check
    inferredHole =
      mkExpression exprI .
      ExpressionInferred . (`Inferred` filledHole) <=<
      convertExpressionI . fmap toPayloadMM .
      SugarInfer.resultFromPure (mkGen 2 3 eGuid)
    plainHole =
      wrapOperatorHole exprI <=<
      mkExpression exprI . ExpressionHole $ mkHole (liftM maybeToList . check)

wrapOperatorHole ::
  m ~ Anchors.ViewM => Data.Expression DefI (PayloadMM m) -> Expression m -> SugarM m (Expression m)
wrapOperatorHole exprI holeExpr = do
  searchTermRef <- SugarM.liftTransaction . Anchors.assocSearchTermRef $ resultGuid exprI
  if isOperatorName $ Property.value searchTermRef
    then
      -- TODO: Ok to mkExpression with same exprI here?
      mkExpression exprI . ExpressionSection DontHaveParens $
      Section Nothing (removeInferredTypes holeExpr) Nothing
    else return holeExpr

isOperatorName :: String -> Bool
isOperatorName name =
  not (null name) && all (`elem` Config.operatorChars) name

chooseHoleType ::
  [Data.Expression DefI f] -> hole -> (Data.Expression DefI f -> hole) -> hole
chooseHoleType inferredVals plain inferred =
  case inferredVals of
  [Data.Expression { Data._eValue = Data.ExpressionLeaf Data.Hole }] -> plain
  [inferredVal] -> inferred inferredVal
  _ -> plain

pickResult ::
  (Monad f, m ~ Anchors.ViewM) =>
  Guid -> Stored (T m) ->
  Data.Expression DefI (Infer.Inferred DefI) ->
  T f (Guid, Actions m)
pickResult defaultDest irefP =
  liftM
  ( flip (,) (mkActions irefP)
  . maybe defaultDest
    (DataIRef.exprGuid . Lens.view (Data.ePayload . Lens._2))
  . listToMaybe . uninferredHoles . fmap swap
  )
  . DataIRef.writeExpression (Property.value irefP)

-- Also skip param types, those can usually be inferred later, so less
-- useful to fill immediately
uninferredHoles ::
  Data.Expression DefI (Infer.Inferred DefI, a) ->
  [Data.Expression DefI (Infer.Inferred DefI, a)]
uninferredHoles
  Data.Expression { Data._eValue = Data.ExpressionApply (Data.Apply func arg) }
  | (Data.isDependentPi . Infer.iType . Lens.view (Data.ePayload . Lens._1)) func =
    uninferredHoles func
  | otherwise = uninferredHoles func ++ uninferredHoles arg
uninferredHoles e@Data.Expression { Data._eValue = Data.ExpressionLeaf Data.Hole } = [e]
uninferredHoles Data.Expression
  { Data._eValue = Data.ExpressionPi (Data.Lambda _ paramType resultType) } =
    uninferredHoles resultType ++ uninferredHoles paramType
uninferredHoles Data.Expression
  { Data._eValue = Data.ExpressionLambda (Data.Lambda _ paramType result) } =
    uninferredHoles result ++ uninferredHoles paramType
uninferredHoles Data.Expression { Data._eValue = body } =
  Foldable.concatMap uninferredHoles body

holeResultHasHoles :: HoleResult -> Bool
holeResultHasHoles = not . null . uninferredHoles . fmap (flip (,) ())

convertHole :: m ~ Anchors.ViewM => Convertor m
convertHole exprI =
  maybe convertUninferredHole convertInferredHole $
  resultInferred exprI
  where
    convertInferredHole inferred = do
      ctx <- SugarM.readContext
      mPaste <- liftM join . Traversable.mapM mkPaste $ resultStored exprI
      convertInferredHoleH ctx mPaste inferred exprI
    convertUninferredHole =
      mkExpression exprI $ ExpressionHole Hole
      { holeScope = []
      , holeInferResults = const $ return []
      , holeMActions = Nothing
      }

convertLiteralInteger :: m ~ Anchors.ViewM => Integer -> Convertor m
convertLiteralInteger i exprI =
  mkExpression exprI . ExpressionLiteralInteger $
  LiteralInteger
  { liValue = i
  , liSetValue = writeIRef <$> resultStored exprI
  }
  where
    writeIRef prop =
      DataIRef.writeExprBody (Property.value prop) .
      Data.makeLiteralInteger

convertAtom :: m ~ Anchors.ViewM => String -> Convertor m
convertAtom name exprI =
  mkExpression exprI $ ExpressionAtom name

convertExpressionI ::
  m ~ Anchors.ViewM =>
  Data.Expression DefI (PayloadMM m) -> SugarM m (Expression m)
convertExpressionI ee =
  ($ ee) $
  case ee ^. Data.eValue of
  Data.ExpressionLambda x -> convertFunc x
  Data.ExpressionPi x -> convertPi x
  Data.ExpressionApply x -> convertApply x
  Data.ExpressionLeaf (Data.GetVariable x) -> convertGetVariable x
  Data.ExpressionLeaf (Data.LiteralInteger x) -> convertLiteralInteger x
  Data.ExpressionLeaf Data.Hole -> convertHole
  Data.ExpressionLeaf Data.Set -> convertAtom "Set"
  Data.ExpressionLeaf Data.IntegerType -> convertAtom "Int"

-- Check no holes
isCompleteType :: Data.Expression DefI () -> Bool
isCompleteType = not . any (isHole . Lens.view Data.eValue) . Data.subExpressions
  where
    isHole (Data.ExpressionLeaf Data.Hole) = True
    isHole _ = False

convertHoleResult ::
  m ~ Anchors.ViewM => SugarConfig -> HoleResult -> T m (Expression m)
convertHoleResult config =
  SugarM.runPure config . convertExpressionI .
  Lens.over (Lens.mapped . SugarInfer.plInferred) Just .
  Lens.set (Lens.mapped . SugarInfer.plStored) Nothing .
  SugarInfer.resultFromInferred

convertExpressionPure ::
  (m ~ Anchors.ViewM, RandomGen g) =>
  g -> SugarConfig -> Data.Expression DefI () -> T m (Expression m)
convertExpressionPure gen config =
  SugarM.runPure config . convertExpressionI . fmap toPayloadMM .
  SugarInfer.resultFromPure gen

convertDefinitionParams ::
  m ~ Anchors.ViewM =>
  SugarM.Context -> Data.Expression DefI (PayloadMM m) ->
  T m
  ( [FuncParam m (Expression m)]
  , Data.Expression DefI (PayloadMM m)
  )
convertDefinitionParams ctx expr =
  case expr ^. Data.eValue of
  Data.ExpressionLambda lam@(Data.Lambda param paramType body) -> do
    paramTypeS <- convertResult paramType ctx
    let
      -- TODO: This is code duplication
      fp = FuncParam
        { _fpGuid = param
        , _fpHiddenLambdaGuid = Just $ resultGuid expr
        , _fpType = removeRedundantTypes paramTypeS
        , _fpMActions =
          mkFuncParamActions ctx body
          <$> resultStored expr
          <*> Traversable.mapM resultStored lam
          <*> resultStored body
        }
    (nextFPs, funcBody) <- convertDefinitionParams ctx body
    return (fp : nextFPs, funcBody)
  _ -> return ([], expr)

convertWhereItems ::
  m ~ Anchors.ViewM =>
  SugarM.Context -> Data.Expression DefI (PayloadMM m) ->
  T m ([WhereItem m], Data.Expression DefI (PayloadMM m))
convertWhereItems ctx
  topLevel@Data.Expression
  { Data._eValue = Data.ExpressionApply apply@Data.Apply
  { Data._applyFunc = Data.Expression
  { Data._eValue = Data.ExpressionLambda lambda@Data.Lambda
  { Data._lambdaParamId = param
  , Data._lambdaParamType = Data.Expression
  { Data._eValue = Data.ExpressionLeaf Data.Hole
  }}}}} = do
    value <- convertDefinitionContent ctx $ apply ^. Data.applyArg
    let
      body = lambda ^. Data.lambdaBody
      mkWIActions topLevelProp bodyProp =
        ListItemActions
        { _itemDelete = mkDelete topLevelProp bodyProp
        , _itemAddNext = liftM fst $ DataOps.redexWrap topLevelProp
        }
      item = WhereItem
        { wiValue = value
        , wiGuid = param
        , wiHiddenGuids =
            map resultGuid [topLevel, lambda ^. Data.lambdaParamType]
        , wiActions =
          mkWIActions <$>
          resultStored topLevel <*>
          resultStored body
        }
    (nextItems, whereBody) <- convertWhereItems ctx body
    return (item : nextItems, whereBody)
convertWhereItems _ expr = return ([], expr)

convertDefinitionContent ::
  m ~ Anchors.ViewM =>
  SugarM.Context -> Data.Expression DefI (PayloadMM m) ->
  T m (DefinitionContent m)
convertDefinitionContent sugarContext expr = do
  (params, funcBody) <- convertDefinitionParams sugarContext expr
  (whereItems, whereBody) <- convertWhereItems sugarContext funcBody
  bodyS <- convertResult whereBody sugarContext
  return DefinitionContent
    { dBody = bodyS
    , dParameters = params
    , dWhereItems = whereItems
    , dAddFirstParam = error "TODO1" -- liftM fst . DataOps.lambdaWrap $ getProp expr
    , dAddInnermostWhereItem =
        error "TODO2"
        -- liftM fst . DataOps.redexWrap $ getProp whereBody
    }

loadConvertDefinition ::
  m ~ Anchors.ViewM => SugarConfig -> DefI ->
  CT m (Definition m)
loadConvertDefinition config defI =
  convertDefinition =<< lift (Load.loadDefinitionClosure defI)
  where
    convertDefBody (Data.DefinitionBuiltin builtin) =
      fmap return . convertDefinitionBuiltin builtin
    convertDefBody (Data.DefinitionExpression exprLoaded) =
      convertDefinitionExpression config exprLoaded
    convertDefinition (Data.Definition defBody typeLoaded) = do
      body <- convertDefBody defBody defI typeLoaded
      typeS <-
        lift .
        convertExpressionPure (mkGen 2 3 (IRef.guid defI)) config $
        void typeLoaded
      return Definition
        { drGuid = IRef.guid defI
        , drBody = body
        , drType = typeS
        }

convertDefinitionBuiltin ::
  Monad m =>
  Data.Builtin -> DefI ->
  Load.LoadedClosure -> DefinitionBody m
convertDefinitionBuiltin (Data.Builtin name) defI typeIRef =
  DefinitionBodyBuiltin DefinitionBuiltin
    { biName = name
    , biMSetName = Just setName
    }
  where
    typeI = typeIRef ^. Data.ePayload
    setName =
      Transaction.writeIRef defI . (`Data.Definition` Load.irefOfClosure typeI) .
      Data.DefinitionBuiltin . Data.Builtin

convertDefinitionExpression ::
  m ~ Anchors.ViewM => SugarConfig ->
  Load.LoadedClosure -> DefI -> Load.LoadedClosure ->
  CT m (DefinitionBody m)
convertDefinitionExpression config exprLoaded defI typeI = do
  inferredLoadedResult <-
    SugarInfer.inferLoadedExpression
    inferLoadedGen (Just defI) exprLoaded $
    Infer.initial (Just defI)
  let
    inferredTypeP =
      void . Infer.iType . iwcInferred . resultInferred $
      inferredLoadedResult ^. SugarInfer.ilrExpr
    typesMatch =
      on (==) Data.canonizeParamIds (void typeI) inferredTypeP
    mkNewType = do
      inferredTypeS <-
        convertExpressionPure iTypeGen config
        inferredTypeP
      return DefinitionNewType
        { dntNewType = inferredTypeS
        , dntAcceptNewType =
          (Property.set . Load.propertyOfClosure) (typeI ^. Data.ePayload) =<<
          DataIRef.newExpression inferredTypeP
        }
    sugarContext = SugarM.mkContext config inferredLoadedResult
  content <-
    lift . convertDefinitionContent sugarContext .
    (fmap . Lens.over SugarInfer.plInferred) Just $
    inferredLoadedResult ^. SugarInfer.ilrExpr
  mNewType <-
    if inferredLoadedResult ^. SugarInfer.ilrSuccess && not typesMatch
    then liftM Just $ lift mkNewType
    else return Nothing
  return $ DefinitionBodyExpression DefinitionExpression
    { deContent = content
    , deMNewType = mNewType
    , deIsTypeRedundant = inferredLoadedResult ^. SugarInfer.ilrSuccess && typesMatch
    }
  where
    iTypeGen = mkGen 0 3 $ IRef.guid defI
    inferLoadedGen = mkGen 1 3 $ IRef.guid defI

--------------

-- loadConvertExpression ::
--   m ~ Anchors.ViewM =>
--   SugarConfig ->
--   Stored (T m) ->
--   CT m (Expression m)
-- loadConvertExpression config exprP =
--   convertLoadedExpression =<< lift (Load.loadExpressionClosure exprP)
--   where
--     convertLoadedExpression exprLoaded = do
--       inferResult <-
--         SugarInfer.inferLoadedExpression Nothing exprLoaded (Infer.initial Nothing)
--       lift . convertStoredExpression (inferResult ^. SugarInfer.ilrExpr) $
--         SugarM.mkContext config inferResult

convertResult ::
  m ~ Anchors.ViewM => Data.Expression DefI (PayloadMM m) -> SugarM.Context ->
  T m (Expression m)
convertResult expr sugarContext =
  SugarM.run sugarContext $ convertExpressionI expr

-- convertStoredExpression ::
--   m ~ Anchors.ViewM =>
--   Data.Expression DefI (SugarInfer.Stored (T m)) -> SugarM.Context ->
--   T m (Expression m)
-- convertStoredExpression expr sugarContext =
--   SugarM.run sugarContext . convertExpressionI $
--   SugarInfer.resultFromStored expr

removeTypes :: Expression m -> Expression m
removeTypes = Lens.set (Lens.mapped . plInferredTypes) []
