{-# LANGUAGE OverloadedStrings, ConstraintKinds #-}
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
  , loadConvertDefinition, loadConvertExpression
  , removeTypes
  ) where

import Control.Applicative ((<$>), (<$), Applicative(..))
import Control.Arrow (first)
import Control.Lens ((^.))
import Control.Monad ((<=<), liftM, mplus, void, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (runWriter)
import Data.Function (on)
import Data.Hashable (hash)
import Data.List.Utils (sortOn)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Monoid (Any(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Editor.CodeEdit.Sugar.Config (SugarConfig)
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
import qualified Editor.CodeEdit.Sugar.Monad as SugarM
import qualified Editor.CodeEdit.Sugar.Infer as SugarInfer
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.Data.IRef as DataIRef
import qualified Editor.Data.Infer as Infer
import qualified Editor.Data.Load as Load
import qualified Editor.Data.Ops as DataOps
import qualified System.Random as Random
import qualified System.Random.Utils as RandomUtils

type Convertor m = SugarInfer.ExprEntity m -> SugarM m (Expression m)

mkCutter :: Monad m => Data.ExpressionIRef -> T m Guid -> T m Guid
mkCutter iref replaceWithHole = do
  Anchors.modP Anchors.clipboards (iref:)
  replaceWithHole

mkActions :: Monad m => DataIRef.ExpressionProperty (T m) -> Actions m
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

mkExpression ::
  Monad m =>
  SugarInfer.ExprEntity m ->
  ExpressionBody m (Expression m) -> SugarM m (Expression m)
mkExpression ee expr = do
  inferredTypesRefs <-
    zipWithM (fmap convertExpressionI . SugarInfer.eeFromPure) seeds types
  return
    Expression
    { _rGuid = SugarInfer.eeGuid ee
    , _rExpressionBody = expr
    , _rPayload = Payload
      { _plInferredTypes = inferredTypesRefs
      , _plActions = mkActions <$> SugarInfer.eeProp ee
      , _plNextHole = Nothing
      }
    }
  where
    seeds = RandomUtils.splits . mkGen 0 2 $ SugarInfer.eeGuid ee
    types = maybe [] iwcInferredTypes . SugarInfer.eplInferred $ ee ^. Data.ePayload

mkDelete
  :: Monad m
  => DataIRef.ExpressionProperty (T m)
  -> DataIRef.ExpressionProperty (T m)
  -> T m Guid
mkDelete parentP replacerP = do
  Property.set parentP replacerI
  return $ DataIRef.exprGuid replacerI
  where
    replacerI = Property.value replacerP

mkFuncParamActions ::
  Monad m => SugarM.Context ->
  SugarInfer.ExprEntityStored m ->
  Data.Lambda (SugarInfer.ExprEntityStored m) ->
  DataIRef.ExpressionProperty (T m) ->
  FuncParamActions m
mkFuncParamActions
  ctx lambdaStored (Data.Lambda param paramType _) replacerP =
  FuncParamActions
  { _fpListItemActions =
    ListItemActions
    { _itemDelete = mkDelete (Infer.iStored lambdaInferred) replacerP
    , _itemAddNext = liftM fst $ DataOps.lambdaWrap replacerP
    }
  , _fpGetExample = do
      exampleP <-
        -- TODO: move to Anchors
        lift . Anchors.nonEmptyAssocDataRef "example" param .
        DataIRef.newExprBody $ Data.ExpressionLeaf Data.Hole
      exampleLoaded <- lift $ Load.loadExpressionProperty exampleP
      inferredExample <- SugarInfer.inferLoadedExpression Nothing exampleLoaded newInferNode
      lift $ convertStoredExpression (inferredExample ^. SugarInfer.ierExpr)
        ctx { SugarM.scInferState = inferredExample ^. SugarInfer.ierRefmap }
  }
  where
    lambdaInferred = iwcInferred lambdaStored
    scope = Infer.nScope $ Infer.iPoint lambdaInferred
    paramTypeRef =
      Infer.tvVal . Infer.nRefs . Infer.iPoint $ iwcInferred paramType
    newInferNode =
      Infer.newTypedNodeWithScope scope paramTypeRef $ SugarM.scInferState ctx

convertLambda
  :: Monad m
  => Data.Lambda (SugarInfer.ExprEntity m)
  -> SugarInfer.ExprEntity m -> SugarM m (FuncParam m (Expression m), Expression m)
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
        mkFuncParamActions ctx
        <$> SugarInfer.eeStored expr
        <*> Traversable.mapM SugarInfer.eeStored lam
        <*> SugarInfer.eeProp bodyI
      }
  return (fp, sBody)

convertFunc
  :: Monad m
  => Data.Lambda (SugarInfer.ExprEntity m)
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
  :: Monad m
  => Data.Lambda (SugarInfer.ExprEntity m)
  -> Convertor m
convertPi lambda exprI = do
  (param, sBody) <- convertLambda lambda exprI
  mkExpression exprI $ ExpressionPi DontHaveParens
    Pi
    { pParam = Lens.over fpType addApplyChildParens param
    , pResultType = removeRedundantTypes sBody
    }

addParens :: ExpressionP m pl -> ExpressionP m pl
addParens = Lens.over rExpressionBody addParensBody

addParensBody
  :: ExpressionBody m (ExpressionP m pl)
     -> ExpressionBody m (ExpressionP m pl)
addParensBody (ExpressionInferred (Inferred val hole)) =
  ExpressionInferred $ Inferred (addParens val) hole
addParensBody (ExpressionPolymorphic (Polymorphic g compact full)) =
  ExpressionPolymorphic . Polymorphic g compact $
  addParens full
addParensBody x = Lens.set eHasParens HaveParens x

addApplyChildParens :: Expression m -> Expression m
addApplyChildParens =
  Lens.over rExpressionBody f
  where
    f x@ExpressionApply{} = x
    f x@ExpressionPolymorphic{} = x
    f x = addParensBody x

isPolymorphicFunc :: SugarInfer.ExprEntity m -> Bool
isPolymorphicFunc funcI =
  maybe False
  (Data.isDependentPi . Infer.iType . iwcInferred) .
  SugarInfer.eplInferred $ funcI ^. Data.ePayload

convertApply :: Monad m => Data.Apply (SugarInfer.ExprEntity m) -> Convertor m
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

isSameOp :: ExpressionBody m expr -> ExpressionBody m expr -> Bool
isSameOp (ExpressionPolymorphic p0) (ExpressionPolymorphic p1) =
  on (==) pCompact p0 p1
isSameOp (ExpressionGetVariable v0) (ExpressionGetVariable v1) =
  v0 == v1
isSameOp _ _ = False

setNextHole :: Expression m -> Expression m -> Expression m
setNextHole possibleHole =
  case possibleHole ^. rExpressionBody of
  ExpressionHole{} ->
    (fmap . Lens.over plNextHole . flip mplus . Just) possibleHole
  _ -> id

applyOnSection ::
  Monad m =>
  Section (Expression m) -> Data.Apply (Expression m, SugarInfer.ExprEntity m) -> Convertor m
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
    right =
      case argRef ^. rExpressionBody of
      ExpressionSection _ (Section (Just _) rightOp (Just _))
        | on isSameOp (Lens.view rExpressionBody) op rightOp -> argRef
      _ -> addApplyChildParens argRef
applyOnSection _ apply exprI = convertApplyPrefix apply exprI

convertApplyPrefix ::
  Monad m =>
  Data.Apply (Expression m, SugarInfer.ExprEntity m) -> Convertor m
convertApplyPrefix (Data.Apply (funcRef, funcI) (argRef, _)) exprI
  | isPolymorphicFunc funcI =
    case funcRef ^. rExpressionBody of
    ExpressionPolymorphic (Polymorphic g compact full) ->
      makePolymorphic g compact =<< makeApply full
    ExpressionGetVariable getVar ->
      makePolymorphic (SugarInfer.eeGuid funcI) getVar =<< makeFullApply
    _ -> makeFullApply
  | otherwise = makeFullApply
  where
    newArgRef = addParens argRef
    newFuncRef =
      setNextHole newArgRef .
      addApplyChildParens .
      removeRedundantTypes $
      funcRef
    expandedGuid = Guid.combine (SugarInfer.eeGuid exprI) $ Guid.fromString "polyExpanded"
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

isHole :: Data.ExpressionBody a -> Bool
isHole (Data.ExpressionLeaf Data.Hole) = True
isHole _ = False

convertGetVariable :: Monad m => Data.VariableRef -> Convertor m
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

mkPaste :: Monad m => DataIRef.ExpressionProperty (T m) -> SugarM m (Maybe (T m Guid))
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

pureHole :: Data.PureExpression
pureHole = Data.pureExpression $ Data.ExpressionLeaf Data.Hole

countArrows :: Data.PureExpression -> Int
countArrows Data.Expression
  { Data._eValue =
    Data.ExpressionPi (Data.Lambda _ _ resultType)
  } = 1 + countArrows resultType
countArrows _ = 0

-- TODO: Return a record, not a tuple
countPis :: Data.PureExpression -> (Int, Int)
countPis e@Data.Expression
  { Data._eValue =
    Data.ExpressionPi (Data.Lambda _ _ resultType)
  }
  | Data.isDependentPi e = first (1+) $ countPis resultType
  | otherwise = (0, 1 + countArrows resultType)
countPis _ = (0, 0)

applyForms
  :: Data.PureExpression
  -> Data.PureExpression -> [Data.PureExpression]
applyForms _ e@Data.Expression{ Data._eValue = Data.ExpressionLambda {} } =
  [e]
applyForms exprType expr =
  reverse . take (1 + arrows) $ iterate addApply withDepPisApplied
  where
    withDepPisApplied = iterate addApply expr !! depPis
    (depPis, arrows) = countPis exprType
    addApply = Data.pureExpression . (`Data.makeApply` pureHole)

convertReadOnlyHole :: Monad m => Convertor m
convertReadOnlyHole exprI =
  mkExpression exprI $ ExpressionHole Hole
  { holeScope = []
  , holeInferResults = const $ return []
  , holeMActions = Nothing
  }

-- Fill partial holes in an expression. Parital holes are those whose
-- inferred (filler) value itself is not complete, so will not be a
-- useful auto-inferred value. By auto-filling those, we allow the
-- user a chance to access all the partiality that needs filling more
-- easily.
fillPartialHolesInExpression ::
  Monad m =>
  (Data.PureExpression -> m (Maybe (Infer.Expression a))) ->
  Infer.Expression a -> m [Infer.Expression a]
fillPartialHolesInExpression check oldExpr =
  liftM ((++ [oldExpr]) . maybeToList) .
  recheck . runWriter $ fillHoleExpr oldExpr
  where
    recheck (newExpr, Any True) = check newExpr
    recheck (_, Any False) = return Nothing
    fillHoleExpr expr@(Data.Expression (Data.ExpressionLeaf Data.Hole) hInferred) =
      let inferredVal = Infer.iValue hInferred
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

convertWritableHole :: Monad m => SugarInfer.ExprEntityStored m -> Convertor m
convertWritableHole eeInferred exprI = do
  ctx <- SugarM.readContext
  mPaste <- mkPaste . Infer.iStored $ iwcInferred eeInferred
  convertWritableHoleH ctx mPaste eeInferred exprI

inferApplyForms ::
  Monad m =>
  (Data.PureExpression -> T m [HoleResult]) -> Data.PureExpression ->
  (Infer.RefMap, Infer.InferNode) -> T m [HoleResult]
inferApplyForms processRes expr (refMap, node) =
  liftM (sortOn resultComplexityScore) . makeApplyForms =<<
  SugarInfer.inferMaybe expr refMap node
  where
    makeApplyForms Nothing = return []
    makeApplyForms (Just i) =
      liftM concat . mapM processRes $
      applyForms (Infer.iType (i ^. Data.ePayload)) expr

convertWritableHoleH ::
  Monad m =>
  SugarM.Context -> Maybe (T m Guid) ->
  SugarInfer.ExprEntityStored m -> Convertor m
convertWritableHoleH (SugarM.Context inferState config contextHash) mPaste eeInferred exprI =
  chooseHoleType (iwcInferredValues eeInferred) plainHole inferredHole
  where
    inferred = iwcInferred eeInferred
    scope = Infer.nScope $ Infer.iPoint inferred
    check expr = SugarInfer.inferMaybe expr inferState $ Infer.iPoint inferred

    memoBy expr act = Cache.memoS (const act) (expr, eGuid, contextHash)

    inferResults processRes expr =
      memoBy expr . inferApplyForms processRes expr $
      Infer.newNodeWithScope scope inferState
    onScopeElement (param, _typeExpr) = param
    mkHole processRes = Hole
      { holeScope =
        map onScopeElement . Map.toList $ Infer.iScope inferred
      , holeInferResults = inferResults processRes
      , holeMActions = Just HoleActions
          { holePickResult = pickResult eGuid $ Infer.iStored inferred
          , holePaste = mPaste
          , holeConvertResult = convertHoleResult config
          }
      }
    filledHole =
      mkHole (maybe (return []) (fillPartialHolesInExpression check) <=< check)
    inferredHole inferredVal =
      mkExpression exprI .
      ExpressionInferred . (`Inferred` filledHole) =<<
      convertExpressionI
      (SugarInfer.eeFromPure (mkGen 1 2 eGuid) inferredVal)
    plainHole =
      wrapOperatorHole exprI <=<
      mkExpression exprI . ExpressionHole $ mkHole (liftM maybeToList . check)
    eGuid = SugarInfer.eeGuid exprI

wrapOperatorHole ::
  Monad m => SugarInfer.ExprEntity m -> Expression m -> SugarM m (Expression m)
wrapOperatorHole exprI holeExpr = do
  searchTermRef <- SugarM.liftTransaction . Anchors.assocSearchTermRef $ SugarInfer.eeGuid exprI
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
  [Data.Expression f] -> hole -> (Data.Expression f -> hole) -> hole
chooseHoleType inferredVals plain inferred =
  case inferredVals of
  [Data.Expression { Data._eValue = Data.ExpressionLeaf Data.Hole }] -> plain
  [inferredVal] -> inferred inferredVal
  _ -> plain

pickResult ::
  (Monad f, Monad m) =>
  Guid -> DataIRef.ExpressionProperty (T m) ->
  Data.Expression (Infer.Inferred a) ->
  Transaction t f (Guid, Actions m)
pickResult defaultDest irefP =
  liftM
  ( flip (,) (mkActions irefP)
  . maybe defaultDest
    (DataIRef.exprGuid . Infer.iStored . Lens.view Data.ePayload)
  . listToMaybe . uninferredHoles . fmap intoStored
  )
  . DataIRef.writeExpression (Property.value irefP)
  where
    intoStored (exprIRef, inferred) = (fmap . const) exprIRef inferred

-- Also skip param types, those can usually be inferred later, so less
-- useful to fill immediately
uninferredHoles :: Infer.Expression a -> [Infer.Expression a]
uninferredHoles Data.Expression { Data._eValue = Data.ExpressionApply (Data.Apply func arg) } =
  if (Data.isDependentPi . Infer.iType . Lens.view Data.ePayload) func
  then uninferredHoles func
  else uninferredHoles func ++ uninferredHoles arg
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
holeResultHasHoles = not . null . uninferredHoles

convertHole :: Monad m => Convertor m
convertHole exprI =
  maybe convertReadOnlyHole convertWritableHole mStored exprI
  where
    mStored = f =<< SugarInfer.eplInferred (exprI ^. Data.ePayload)
    f entity = fmap (g entity) $ (Infer.iStored . iwcInferred) entity
    g entity stored =
      (atEeiInferred . fmap . const) stored entity
    atEeiInferred j x = x { iwcInferred = j $ iwcInferred x }

convertLiteralInteger :: Monad m => Integer -> Convertor m
convertLiteralInteger i exprI =
  mkExpression exprI . ExpressionLiteralInteger $
  LiteralInteger
  { liValue = i
  , liSetValue = writeIRef <$> SugarInfer.eeProp exprI
  }
  where
    writeIRef prop =
      DataIRef.writeExprBody (Property.value prop) .
      Data.makeLiteralInteger

convertAtom :: Monad m => String -> Convertor m
convertAtom name exprI =
  mkExpression exprI $ ExpressionAtom name

convertExpressionI :: Monad m => SugarInfer.ExprEntity m -> SugarM m (Expression m)
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
isCompleteType :: Data.PureExpression -> Bool
isCompleteType = not . any (isHole . Lens.view Data.eValue) . Data.subExpressions

convertHoleResult ::
  Monad m => SugarConfig -> HoleResult -> T m (Expression m)
convertHoleResult config holeResult =
  SugarM.runPure config . convertExpressionI . Data.randomizeExpr gen $
  fmap toInferredExpr holeResult
  where
    gen = Random.mkStdGen . hash . show $ void holeResult
    toInferredExpr inferred =
      flip SugarInfer.EntityPayload $
      Just InferredWithConflicts
      { iwcInferred = Nothing <$ inferred
      , iwcTypeConflicts = []
      , iwcValueConflicts = []
      }

convertExpressionPure ::
  (Monad m, RandomGen g) =>
  g -> SugarConfig -> Data.PureExpression -> T m (Expression m)
convertExpressionPure gen config =
  SugarM.runPure config . convertExpressionI . SugarInfer.eeFromPure gen

convertDefinitionParams ::
  Monad m =>
  SugarM.Context -> Data.Expression (SugarInfer.ExprEntityStored m) ->
  T m ([FuncParam m (Expression m)], Data.Expression (SugarInfer.ExprEntityStored m))
convertDefinitionParams ctx expr =
  case expr ^. Data.eValue of
  Data.ExpressionLambda lam@(Data.Lambda param paramType body) -> do
    paramTypeS <- convertStoredExpression paramType ctx
    let
      fp = FuncParam
        { _fpGuid = param
        , _fpHiddenLambdaGuid = Just . SugarInfer.iwcGuid $ expr ^. Data.ePayload
        , _fpType = removeRedundantTypes paramTypeS
        , _fpMActions =
          Just $
          mkFuncParamActions ctx
          (expr ^. Data.ePayload) (Lens.view Data.ePayload <$> lam)
          (storedIRefP body)
        }
    (nextFPs, funcBody) <- convertDefinitionParams ctx body
    return (fp : nextFPs, funcBody)
  _ -> return ([], expr)
  where
    storedIRefP = Infer.iStored . iwcInferred . Lens.view Data.ePayload

convertWhereItems ::
  Monad m =>
  SugarM.Context -> Data.Expression (SugarInfer.ExprEntityStored m) ->
  T m ([WhereItem m], Data.Expression (SugarInfer.ExprEntityStored m))
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
      item = WhereItem
        { wiValue = value
        , wiGuid = param
        , wiHiddenGuids =
            map (SugarInfer.iwcGuid . Lens.view Data.ePayload)
            [ topLevel
            , lambda ^. Data.lambdaParamType
            ]
        , wiActions =
            ListItemActions
            { _itemDelete = mkDelete (prop topLevel) (prop body)
            , _itemAddNext = liftM fst . DataOps.redexWrap $ prop topLevel
            }
        }
    (nextItems, whereBody) <- convertWhereItems ctx body
    return (item : nextItems, whereBody)
  where
    prop = Infer.iStored . iwcInferred . Lens.view Data.ePayload
convertWhereItems _ expr = return ([], expr)

convertDefinitionContent ::
  Monad m =>
  SugarM.Context -> Data.Expression (SugarInfer.ExprEntityStored m) ->
  T m (DefinitionContent m)
convertDefinitionContent sugarContext expr = do
  (params, funcBody) <- convertDefinitionParams sugarContext expr
  (whereItems, whereBody) <- convertWhereItems sugarContext funcBody
  bodyS <- convertStoredExpression whereBody sugarContext
  return DefinitionContent
    { dBody = bodyS
    , dParameters = params
    , dWhereItems = whereItems
    , dAddFirstParam = liftM fst . DataOps.lambdaWrap $ stored expr
    , dAddInnermostWhereItem =
        liftM fst . DataOps.redexWrap $ stored whereBody
    }
  where
    stored = Infer.iStored . iwcInferred . Lens.view Data.ePayload

loadConvertDefinition ::
  Monad m => SugarConfig -> Data.DefinitionIRef ->
  CT m (Definition m)
loadConvertDefinition config defI =
  -- TODO: defI given twice probably means the result of
  -- loadDefinition is missing some defI-dependent values
  convertDefinition =<< lift (Load.loadDefinition defI)
  where
    convertDefBody (Data.DefinitionBuiltin builtin) =
      fmap return . convertDefinitionBuiltin builtin
    convertDefBody (Data.DefinitionExpression exprLoaded) =
      convertDefinitionExpression config exprLoaded
    convertDefinition (Data.Definition defBody typeLoaded) = do
      body <- convertDefBody defBody defI typeLoaded
      typeS <-
        lift .
        convertExpressionPure (mkGen 1 2 (IRef.guid defI)) config .
        void $ Load.sExpr typeLoaded
      return Definition
        { drGuid = IRef.guid defI
        , drBody = body
        , drType = typeS
        }

convertDefinitionBuiltin ::
  Monad m =>
  Data.Builtin -> Data.DefinitionIRef ->
  Load.Loaded (T m) ->
  DefinitionBody m
convertDefinitionBuiltin (Data.Builtin name) defI (Load.Stored _ typeIRef) =
  DefinitionBodyBuiltin DefinitionBuiltin
    { biName = name
    , biMSetName = Just setName
    }
  where
    typeI = typeIRef ^. Data.ePayload
    setName =
      Transaction.writeIRef defI . (`Data.Definition` typeI) .
      Data.DefinitionBuiltin . Data.Builtin

convertDefinitionExpression ::
  Monad m => SugarConfig ->
  Load.Loaded (T m) ->
  Data.DefinitionIRef ->
  Load.Loaded (T m) ->
  CT m (DefinitionBody m)
convertDefinitionExpression config exprLoaded defI (Load.Stored setType typeIRef) = do
  inferredDef <- SugarInfer.inferLoadedExpression (Just defI) exprLoaded Infer.initial
  let
    inferredTypeP =
      Infer.iType . iwcInferred $ inferredDef ^. SugarInfer.ierExpr . Data.ePayload
    typesMatch = on (==) Data.canonizeParamIds (void typeIRef) inferredTypeP
    mkNewType = do
      inferredTypeS <-
        convertExpressionPure (mkGen 0 2 (IRef.guid defI)) config
        inferredTypeP
      return DefinitionNewType
        { dntNewType = inferredTypeS
        , dntAcceptNewType =
          setType =<< DataIRef.newExpression inferredTypeP
        }
    sugarContext =
      SugarM.mkContext config inferredDef
  content <-
    lift . convertDefinitionContent sugarContext $ inferredDef ^. SugarInfer.ierExpr
  mNewType <- lift $
    if inferredDef ^. SugarInfer.ierSuccess && not typesMatch && isCompleteType inferredTypeP
    then liftM Just mkNewType
    else return Nothing
  return $ DefinitionBodyExpression DefinitionExpression
    { deContent = content
    , deMNewType = mNewType
    , deIsTypeRedundant = inferredDef ^. SugarInfer.ierSuccess && typesMatch
    }

--------------

loadConvertExpression ::
  Monad m =>
  SugarConfig ->
  DataIRef.ExpressionProperty (T m) ->
  CT m (Expression m)
loadConvertExpression config exprP =
  convertLoadedExpression =<< lift (Load.loadExpressionProperty exprP)
  where
    convertLoadedExpression exprLoaded = do
      inferResult <- SugarInfer.inferLoadedExpression Nothing exprLoaded Infer.initial
      lift . convertStoredExpression (inferResult ^. SugarInfer.ierExpr) $
        SugarM.mkContext config inferResult

convertStoredExpression ::
  Monad m =>
  Data.Expression (SugarInfer.ExprEntityStored m) -> SugarM.Context ->
  T m (Expression m)
convertStoredExpression expr sugarContext =
  SugarM.run sugarContext . convertExpressionI $
  f <$> expr
  where
    f i = SugarInfer.EntityPayload
      { SugarInfer.eplGuid = SugarInfer.iwcGuid i
      , SugarInfer.eplInferred = Just $ Just <$> i
      }

removeTypes :: Expression m -> Expression m
removeTypes = Lens.set (Lens.mapped . plInferredTypes) []
