{-# LANGUAGE OverloadedStrings, ConstraintKinds, TypeFamilies #-}
module Lamdu.CodeEdit.Sugar
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
  , loadConvertDefI
  , removeTypes
  ) where

import Control.Applicative ((<$>), Applicative(..), liftA2)
import Control.Arrow (first)
import Control.Lens ((^.))
import Control.Monad ((<=<), join, mplus, void, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (runState)
import Control.Monad.Trans.Writer (runWriter)
import Control.MonadA (MonadA)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List.Utils (sortOn)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Monoid (Any(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Traversable (traverse)
import Data.Tuple (swap)
import Lamdu.CodeEdit.Sugar.Config (SugarConfig)
import Lamdu.CodeEdit.Sugar.Infer (InferredWC, NoInferred, Stored, NoStored)
import Lamdu.CodeEdit.Sugar.Monad (SugarM)
import Lamdu.CodeEdit.Sugar.Types -- see export list
import Lamdu.Data.IRef (DefI)
import Lamdu.Data.Infer.Conflicts (InferredWithConflicts(..), iwcInferredTypes, iwcInferredValues)
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
import qualified Lamdu.Anchors as Anchors
import qualified Lamdu.CodeEdit.Infix as Infix
import qualified Lamdu.CodeEdit.Sugar.Infer as SugarInfer
import qualified Lamdu.CodeEdit.Sugar.Monad as SugarM
import qualified Lamdu.Config as Config
import qualified Lamdu.Data as Data
import qualified Lamdu.Data.IRef as DataIRef
import qualified Lamdu.Data.Infer as Infer
import qualified Lamdu.Data.Load as Load
import qualified Lamdu.Data.Ops as DataOps
import qualified System.Random as Random
import qualified System.Random.Utils as RandomUtils

type PayloadMM m = SugarInfer.Payload (Maybe (InferredWC (Tag m))) (Maybe (Stored m))
type Convertor m =
  Data.Expression (DefI (Tag m)) (PayloadMM m) ->
  SugarM m (Expression m)

mkCutter :: m ~ Anchors.ViewM => DataIRef.Expression (Tag m) -> T m Guid -> T m Guid
mkCutter expr replaceWithHole = do
  _ <- DataOps.newClipboard expr
  replaceWithHole

mkActions :: m ~ Anchors.ViewM => Stored m -> Actions m
mkActions stored =
  Actions
  { giveAsArg = guidify $ DataOps.giveAsArg stored
  , replace = doReplace
  , cut = mkCutter (Property.value stored) doReplace
  , giveAsArgToOperator = guidify . DataOps.giveAsArgToOperator stored
  }
  where
    guidify = fmap DataIRef.exprGuid
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
  Data.Expression (DefI (Tag m)) (PayloadMM m) ->
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

replaceWith
  :: MonadA m
  => Stored m
  -> Stored m
  -> T m Guid
replaceWith parentP replacerP = do
  Property.set parentP replacerI
  return $ DataIRef.exprGuid replacerI
  where
    replacerI = Property.value replacerP

deleteParamRef ::
  MonadA m => Guid -> Data.Expression def (Stored m) -> T m ()
deleteParamRef param =
  traverse_ deleteIfParamRef . Data.subExpressions
  where
    deleteIfParamRef expr =
      case expr of
      Data.Expression
        (Data.ExpressionLeaf (Data.GetVariable (Data.ParameterRef p))) prop
        | p == param -> void $ DataOps.replaceWithHole prop
      _ -> return ()

mkFuncParamActions ::
  m ~ Anchors.ViewM =>
  Stored m ->
  Data.Lambda (Data.Expression def (Stored m)) ->
  FuncParamActions m
mkFuncParamActions lambdaProp (Data.Lambda param _paramType body) =
  FuncParamActions
  { _fpListItemActions =
    ListItemActions
    { _itemDelete = do
        deleteParamRef param body
        replaceWith lambdaProp $ body ^. Data.ePayload
    , _itemAddNext = fmap fst . DataOps.lambdaWrap $ body ^. Data.ePayload
    }
  , _fpGetExample =
      return .
      Expression (Guid.augment "EXAMPLE" param)
      (ExpressionAtom "NotImplemented") $
      Payload [] Nothing Nothing
  }

data IsDependent = Dependent | NonDependent
  deriving (Eq, Ord, Show)

convertFuncParam ::
  m ~ Anchors.ViewM =>
  Data.Lambda (Data.Expression (DefI (Tag m)) (PayloadMM m)) ->
  Data.Expression (DefI (Tag m)) (PayloadMM m) ->
  SugarM m (IsDependent, FuncParam m (Expression m))
convertFuncParam lam@(Data.Lambda paramGuid paramType _) expr = do
  paramTypeS <- convertExpressionI paramType
  let
    fp = FuncParam
      { _fpGuid = paramGuid
      , _fpHiddenLambdaGuid = Just $ resultGuid expr
      , _fpType = removeRedundantTypes paramTypeS
      , _fpMActions =
        mkFuncParamActions
        <$> resultStored expr
        <*> (traverse . traverse) (Lens.view SugarInfer.plStored) lam
      }
    isDependent
      | isPolymorphicFunc expr = Dependent
      | otherwise = NonDependent
  return (isDependent, fp)

convertLambda ::
  m ~ Anchors.ViewM =>
  Data.Lambda (Data.Expression (DefI (Tag m)) (PayloadMM m)) ->
  Data.Expression (DefI (Tag m)) (PayloadMM m) ->
  SugarM m ((IsDependent, FuncParam m (Expression m)), Expression m)
convertLambda lam expr =
  liftA2 (,)
  (convertFuncParam lam expr) $
  convertExpressionI (lam ^. Data.lambdaBody)

fAllParams :: Func m expr -> [FuncParam m expr]
fAllParams (Func depParams params _) = depParams ++ params

convertFunc
  :: m ~ Anchors.ViewM
  => Data.Lambda (Data.Expression (DefI (Tag m)) (PayloadMM m))
  -> Convertor m
convertFunc lambda exprI = do
  ((isDependent, param), sBody) <- convertLambda lambda exprI
  let
    innerFunc =
      case sBody ^. rExpressionBody of
      ExpressionFunc _ func -> func
      _ -> Func [] [] sBody
    mNextParam = listToMaybe $ fAllParams innerFunc
    newParam = maybe id deleteToNextParam mNextParam param
    newFunc =
      case isDependent of
      Dependent -> Lens.over fDepParams (newParam :) innerFunc
      NonDependent -> Func [] (newParam : fAllParams innerFunc) (innerFunc ^. fBody)
  mkExpression exprI $ ExpressionFunc DontHaveParens newFunc
  where
    deleteToNextParam nextParam =
      Lens.set
      (fpMActions . Lens.mapped . fpListItemActions .
       itemDelete . Lens.sets fmap) $
      nextParam ^. fpGuid

convertPi
  :: m ~ Anchors.ViewM
  => Data.Lambda (Data.Expression (DefI (Tag m)) (PayloadMM m))
  -> Convertor m
convertPi lambda exprI = do
  ((_, param), sBody) <- convertLambda lambda exprI
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

isPolymorphicFunc :: Data.Expression (DefI (Tag m)) (PayloadMM m) -> Bool
isPolymorphicFunc funcI =
  maybe False
  (Data.isDependentPi . Infer.iType . iwcInferred) $
  resultInferred funcI

convertApply :: m ~ Anchors.ViewM => Data.Apply (Data.Expression (DefI (Tag m)) (PayloadMM m)) -> Convertor m
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
  Section (Expression m) -> Data.Apply (Expression m, Data.Expression (DefI (Tag m)) (PayloadMM m)) -> Convertor m
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
  Data.Apply (Expression m, Data.Expression (DefI (Tag m)) (PayloadMM m)) -> Convertor m
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

convertGetVariable :: m ~ Anchors.ViewM => Data.VariableRef (DefI (Tag m)) -> Convertor m
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
  Stored m -> SugarM m (Maybe (T m Guid))
mkPaste exprP = do
  clipboardsP <- SugarM.liftTransaction Anchors.clipboards
  let
    mClipPop =
      case Property.value clipboardsP of
      [] -> Nothing
      (clip : clips) -> Just (clip, Property.set clipboardsP clips)
  return $ doPaste (Property.set exprP) <$> mClipPop
  where
    doPaste replacer (clipDefI, popClip) = do
      clipDef <- Transaction.readIRef clipDefI
      let
        clip =
          case clipDef of
          Data.Definition (Data.DefinitionExpression defExpr) _ -> defExpr
          _ -> error "Clipboard contained a non-expression definition!"
      Transaction.deleteIRef clipDefI
      ~() <- popClip
      ~() <- replacer clip
      return $ DataIRef.exprGuid clip

countArrows :: Data.Expression def () -> Int
countArrows Data.Expression
  { Data._eValue =
    Data.ExpressionPi (Data.Lambda _ _ resultType)
  } = 1 + countArrows resultType
countArrows _ = 0

-- TODO: Return a record, not a tuple
countPis :: Data.Expression def () -> (Int, Int)
countPis e@Data.Expression
  { Data._eValue =
    Data.ExpressionPi (Data.Lambda _ _ resultType)
  }
  | Data.isDependentPi e = first (1+) $ countPis resultType
  | otherwise = (0, 1 + countArrows resultType)
countPis _ = (0, 0)

applyForms
  :: Data.Expression def ()
  -> Data.Expression def () -> [Data.Expression def ()]
applyForms _ e@Data.Expression{ Data._eValue = Data.ExpressionLambda {} } =
  [e]
applyForms exprType expr =
  reverse . take (1 + arrows) $ iterate addApply withDepPisApplied
  where
    withDepPisApplied = iterate addApply expr !! depPis
    (depPis, arrows) = countPis exprType
    addApply = Data.pureExpression . (`Data.makeApply` Data.pureHole)

-- Fill partial holes in an expression. Parital holes are those whose
-- inferred (filler) value itself is not complete, so will not be a
-- useful auto-inferred value. By auto-filling those, we allow the
-- user a chance to access all the partiality that needs filling more
-- easily.
fillPartialHolesInExpression ::
  MonadA m =>
  (Data.Expression def () ->
   m (Maybe (Data.Expression def (Infer.Inferred def)))) ->
  Data.Expression def (Infer.Inferred def) ->
  m [Data.Expression def (Infer.Inferred def)]
fillPartialHolesInExpression check oldExpr =
  fmap ((++ [oldExpr]) . maybeToList) .
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
      fmap Data.pureExpression $ traverse fillHoleExpr body

resultComplexityScore :: HoleResult t -> Int
resultComplexityScore =
  sum . map (subtract 2 . length . Foldable.toList . Infer.iType) .
  Foldable.toList

inferApplyForms ::
  MonadA m =>
  (Data.Expression (DefI (Tag m)) () -> T m [HoleResult (Tag m)]) -> Data.Expression (DefI (Tag m)) () ->
  (Infer.InferNode (DefI (Tag m)), Infer.Context (DefI (Tag m))) -> T m [HoleResult (Tag m)]
inferApplyForms processRes expr (node, inferContext) =
  fmap (sortOn resultComplexityScore) . makeApplyForms =<<
  SugarInfer.inferMaybe_ expr inferContext node
  where
    makeApplyForms Nothing = return []
    makeApplyForms (Just i) =
      fmap concat . traverse processRes $
      (applyForms . void) (Infer.iType (i ^. Data.ePayload)) expr

convertInferredHoleH ::
  m ~ Anchors.ViewM =>
  SugarM.Context (Tag m) -> Maybe (T m Guid) ->
  InferredWC (Tag m) -> Convertor m
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
      mkExpression exprI . ExpressionHole $ mkHole (fmap maybeToList . check)

wrapOperatorHole ::
  m ~ Anchors.ViewM => Data.Expression (DefI (Tag m)) (PayloadMM m) -> Expression m -> SugarM m (Expression m)
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
  [Data.Expression (DefI (Tag m)) f] -> hole -> (Data.Expression (DefI (Tag m)) f -> hole) -> hole
chooseHoleType inferredVals plain inferred =
  case inferredVals of
  [Data.Expression { Data._eValue = Data.ExpressionLeaf Data.Hole }] -> plain
  [inferredVal] -> inferred inferredVal
  _ -> plain

pickResult ::
  m ~ Anchors.ViewM =>
  Guid -> Stored m ->
  Data.Expression (DefI (Tag m)) (Infer.Inferred (DefI (Tag m))) ->
  T m (Guid, Actions m)
pickResult defaultDest irefP =
  fmap
  ( flip (,) (mkActions irefP)
  . maybe defaultDest
    (DataIRef.exprGuid . Lens.view (Data.ePayload . Lens._2))
  . listToMaybe . uninferredHoles . fmap swap
  )
  . DataIRef.writeExpression (Property.value irefP)

-- Also skip param types, those can usually be inferred later, so less
-- useful to fill immediately
uninferredHoles ::
  Data.Expression def (Infer.Inferred def, a) ->
  [Data.Expression def (Infer.Inferred def, a)]
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

holeResultHasHoles :: HoleResult t -> Bool
holeResultHasHoles = not . null . uninferredHoles . fmap (flip (,) ())

convertHole :: m ~ Anchors.ViewM => Convertor m
convertHole exprI =
  maybe convertUninferredHole convertInferredHole $
  resultInferred exprI
  where
    convertInferredHole inferred = do
      ctx <- SugarM.readContext
      mPaste <- fmap join . traverse mkPaste $ resultStored exprI
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
  Data.Expression (DefI (Tag m)) (PayloadMM m) -> SugarM m (Expression m)
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
isCompleteType :: Data.Expression def () -> Bool
isCompleteType =
  Lens.nullOf
  ( Lens.folding Data.subExpressions
  . Data.eValue . Data.expressionLeaf . Data.hole
  )

convertHoleResult ::
  m ~ Anchors.ViewM => SugarConfig (Tag m) -> HoleResult (Tag m) -> T m (Expression m)
convertHoleResult config =
  SugarM.runPure config . convertExpressionI .
  Lens.over (Lens.mapped . SugarInfer.plInferred) Just .
  Lens.set (Lens.mapped . SugarInfer.plStored) Nothing .
  SugarInfer.resultFromInferred

convertExpressionPure ::
  (m ~ Anchors.ViewM, RandomGen g) =>
  g -> SugarConfig (Tag m) -> Data.Expression (DefI (Tag m)) () -> T m (Expression m)
convertExpressionPure gen config =
  SugarM.runPure config . convertExpressionI . fmap toPayloadMM .
  SugarInfer.resultFromPure gen

convertDefinitionParams ::
  m ~ Anchors.ViewM =>
  Data.Expression (DefI (Tag m)) (PayloadMM m) ->
  SugarM m
  ( [FuncParam m (Expression m)]
  , [FuncParam m (Expression m)]
  , Data.Expression (DefI (Tag m)) (PayloadMM m)
  )
convertDefinitionParams expr =
  case expr ^. Data.eValue of
  Data.ExpressionLambda lambda -> do
    (isDependent, fp) <- convertFuncParam lambda expr
    (depParams, params, deepBody) <-
      convertDefinitionParams $ lambda ^. Data.lambdaBody
    return $
      case isDependent of
      Dependent -> (fp : depParams, params, deepBody)
      NonDependent -> ([], fp : depParams ++ params, deepBody)
  _ -> return ([], [], expr)

convertWhereItems ::
  m ~ Anchors.ViewM =>
  Data.Expression (DefI (Tag m)) (PayloadMM m) ->
  SugarM m ([WhereItem m], Data.Expression (DefI (Tag m)) (PayloadMM m))
convertWhereItems
  topLevel@Data.Expression
  { Data._eValue = Data.ExpressionApply apply@Data.Apply
  { Data._applyFunc = Data.Expression
  { Data._eValue = Data.ExpressionLambda lambda@Data.Lambda
  { Data._lambdaParamId = param
  , Data._lambdaParamType = Data.Expression
  { Data._eValue = Data.ExpressionLeaf Data.Hole
  }
  , Data._lambdaBody = body
  }}}} = do
    value <- convertDefinitionContent $ apply ^. Data.applyArg
    let
      mkWIActions topLevelProp bodyStored =
        ListItemActions
        { _itemDelete = do
             deleteParamRef param bodyStored
             replaceWith topLevelProp $ bodyStored ^. Data.ePayload
        , _itemAddNext = fmap fst $ DataOps.redexWrap topLevelProp
        }
      item = WhereItem
        { wiValue = value
        , wiGuid = param
        , wiHiddenGuids =
            map resultGuid [topLevel, lambda ^. Data.lambdaParamType]
        , wiActions =
          mkWIActions <$>
          resultStored topLevel <*>
          traverse (Lens.view SugarInfer.plStored) body
        }
    (nextItems, whereBody) <- convertWhereItems body
    return (item : nextItems, whereBody)
convertWhereItems expr = return ([], expr)

addStoredParam ::
  MonadA m =>
  Data.Expression def (SugarInfer.Payload inferred (Maybe (Stored m))) ->
  T m Guid
addStoredParam
  Data.Expression
  { Data._ePayload =
    SugarInfer.Payload { SugarInfer._plStored = Just prop } } =
  fmap fst $ DataOps.lambdaWrap prop
addStoredParam
  Data.Expression
  { Data._eValue =
    Data.ExpressionLambda
    Data.Lambda { Data._lambdaBody = body } } =
  addStoredParam body
addStoredParam _ =
  error $
  "Non-stored can only be lambda added by implicit " ++
  "type-variables which must contain a stored in its body"

assertedGetProp ::
  String ->
  Data.Expression def (SugarInfer.Payload inferred (Maybe stored)) -> stored
assertedGetProp _
  Data.Expression
  { Data._ePayload =
    SugarInfer.Payload { SugarInfer._plStored = Just prop } } = prop
assertedGetProp msg _ = error msg

convertDefinitionContent ::
  m ~ Anchors.ViewM =>
  Data.Expression (DefI (Tag m)) (PayloadMM m) ->
  SugarM m (DefinitionContent m)
convertDefinitionContent expr = do
  (depParams, params, funcBody) <- convertDefinitionParams expr
  (whereItems, whereBody) <- convertWhereItems funcBody
  bodyS <- convertExpressionI whereBody
  return DefinitionContent
    { dFunc = Func depParams params bodyS
    , dWhereItems = whereItems
    , dAddFirstParam = addStoredParam expr
    , dAddInnermostWhereItem =
      fmap fst . DataOps.redexWrap $
      assertedGetProp "Where must be stored" whereBody
    }

loadConvertDefI ::
  m ~ Anchors.ViewM => SugarConfig (Tag m) -> DefI (Tag m) ->
  CT m (Definition m)
loadConvertDefI config defI =
  convertDefI =<< lift (Load.loadDefinitionClosure defI)
  where
    convertDefBody (Data.DefinitionBuiltin builtin) =
      fmap return . convertDefIBuiltin builtin
    convertDefBody (Data.DefinitionExpression exprLoaded) =
      convertDefIExpression config exprLoaded
    convertDefI (Data.Definition defBody typeLoaded) = do
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

convertDefIBuiltin ::
  MonadA m =>
  Data.Builtin -> DefI (Tag m) ->
  Load.LoadedClosure (Tag m) -> DefinitionBody m
convertDefIBuiltin (Data.Builtin name) defI typeIRef =
  DefinitionBodyBuiltin DefinitionBuiltin
    { biName = name
    , biMSetName = Just setName
    }
  where
    typeI = typeIRef ^. Data.ePayload
    setName =
      Transaction.writeIRef defI . (`Data.Definition` Load.irefOfClosure typeI) .
      Data.DefinitionBuiltin . Data.Builtin

convertDefIExpression ::
  m ~ Anchors.ViewM => SugarConfig (Tag m) ->
  Load.LoadedClosure (Tag m) -> DefI (Tag m) -> Load.LoadedClosure (Tag m) ->
  CT m (DefinitionBody m)
convertDefIExpression config exprLoaded defI typeI = do
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
  lift . SugarM.run (SugarM.mkContext config inferredLoadedResult) $ do
    content <-
      convertDefinitionContent .
      (fmap . Lens.over SugarInfer.plInferred) Just $
      inferredLoadedResult ^. SugarInfer.ilrExpr
    mNewType <-
      if inferredLoadedResult ^. SugarInfer.ilrSuccess && not typesMatch
      then fmap Just $ SugarM.liftTransaction mkNewType
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

removeTypes :: Expression m -> Expression m
removeTypes = Lens.set (Lens.mapped . plInferredTypes) []
