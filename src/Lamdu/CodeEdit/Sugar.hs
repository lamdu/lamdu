{-# LANGUAGE FlexibleContexts, OverloadedStrings, ConstraintKinds, TypeFamilies, Rank2Types #-}
module Lamdu.CodeEdit.Sugar
  ( Definition(..), DefinitionBody(..)
  , ListItemActions(..), itemAddNext, itemDelete
  , FuncParamActions(..), fpListItemActions, fpGetExample
  , DefinitionExpression(..), DefinitionContent(..), DefinitionNewType(..)
  , DefinitionBuiltin(..)
  , Actions(..)
    , giveAsArg, callWithArg, callWithNextArg
    , setToHole, replaceWithNewHole, cut, giveAsArgToOperator
  , ExpressionBody(..)
  , Payload(..), plInferredTypes, plActions, plNextHole
  , ExpressionP(..)
    , rGuid, rExpressionBody, rPayload, rHiddenGuids, rPresugaredExpression
  , Expression
  , WhereItem(..)
  , ListItem(..), ListActions(..), List(..)
  , RecordField(..), rfMItemActions, rfTag, rfExpr
  , Kind(..), Record(..), FieldList(..), GetField(..)
  , Func(..)
  , FuncParam(..), fpGuid, fpHiddenLambdaGuid, fpType, fpMActions
  , Pi(..)
  , Section(..)
  , Hole(..), holeScope, holeMActions
  , HoleActions(..)
    , holePaste, holeMDelete, holeResult, holeInferExprType
  , StorePoint
  , HoleResult(..)
    , holeResultInferred
    , holeResultConvert
    , holeResultPick, holeResultPickPrefix
  , holeResultHasHoles
  , LiteralInteger(..)
  , Inferred(..)
  , Polymorphic(..)
  , HasParens(..)
  , loadConvertDefI
  , removeTypes
  , PrefixAction, emptyPrefixAction
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens (Traversal')
import Control.Lens.Operators
import Control.Monad ((<=<), join, mplus, void, zipWithM, MonadPlus)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), runState, mapStateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Function (on)
import Data.Hashable (hashWithSalt)
import Data.Maybe (listToMaybe, isJust)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag, Tagged)
import Data.Traversable (traverse)
import Data.Tuple (swap)
import Data.Typeable (Typeable1)
import Lamdu.CodeEdit.Sugar.Infer (InferredWC, NoInferred(..), Stored, NoStored)
import Lamdu.CodeEdit.Sugar.Monad (SugarM)
import Lamdu.CodeEdit.Sugar.Types -- see export list
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Infer.Conflicts (InferredWithConflicts(..), iwcInferredTypes, iwcInferredValues)
import System.Random (RandomGen)
import qualified Control.Lens as Lens
import qualified Data.Binary.Utils as BinaryUtils
import qualified Data.Cache as Cache
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.CodeEdit.Infix as Infix
import qualified Lamdu.CodeEdit.Sugar.Infer as SugarInfer
import qualified Lamdu.CodeEdit.Sugar.Monad as SugarM
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Load as Load
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Ops as DataOps
import qualified System.Random as Random
import qualified System.Random.Utils as RandomUtils

type PayloadMM m =
  SugarInfer.Payload (Tag m) (Maybe (InferredWC (Tag m))) (Maybe (Stored m))
type Convertor m =
  DataIRef.ExpressionM m (PayloadMM m) ->
  SugarM m (Expression m)

mkCutter :: MonadA m => Anchors.CodeProps m -> DataIRef.ExpressionI (Tag m) -> T m Guid -> T m Guid
mkCutter cp expr replaceWithHole = do
  _ <- DataOps.newClipboard cp expr
  replaceWithHole

checkReinferSuccess :: MonadA m => SugarM.Context m -> String -> T m a -> CT m Bool
checkReinferSuccess sugarContext key act =
  case SugarM.scMReinferRoot sugarContext of
  Nothing -> pure False
  Just reinferRoot ->
    mapStateT Transaction.forkScratch $ do
      _ <- lift act
      reinferRoot key

guardReinferSuccess :: MonadA m => SugarM.Context m -> String -> T m a -> CT m (Maybe (T m a))
guardReinferSuccess sugarContext key act = do
  success <- checkReinferSuccess sugarContext key act
  pure $
    if success
    then Just act
    else Nothing

mkCallWithArg ::
  MonadA m => SugarM.Context m ->
  DataIRef.ExpressionM m (SugarInfer.Payload (Tag m) i (Stored m)) ->
  PrefixAction m -> CT m (Maybe (T m Guid))
mkCallWithArg sugarContext exprS prefixAction =
  guardReinferSuccess sugarContext "callWithArg" $ do
    prefixAction
    fmap DataIRef.exprGuid . DataOps.callWithArg $ resultStored exprS

mkActions ::
  MonadA m => SugarM.Context m ->
  DataIRef.ExpressionM m (SugarInfer.Payload (Tag m) i (Stored m)) -> Actions m
mkActions sugarContext exprS =
  Actions
  { _giveAsArg = giveAsArgPrefix
  , _callWithArg = mkCallWithArg sugarContext exprS
  , _callWithNextArg = pure (pure Nothing)
  , _setToHole = doReplace DataOps.setToHole
  , _replaceWithNewHole = doReplace DataOps.replaceWithHole
  , _cut = mkCutter (SugarM.scCodeAnchors sugarContext) (Property.value stored) $ doReplace DataOps.replaceWithHole
  , _giveAsArgToOperator = DataIRef.exprGuid <$> DataOps.giveAsArgToOperator stored
  }
  where
    giveAsArgPrefix prefix = DataIRef.exprGuid <$> (prefix *> DataOps.giveAsArg stored)
    stored = resultStored exprS
    doReplace f = DataIRef.exprGuid <$> f stored

mkGen :: Int -> Int -> Guid -> Random.StdGen
mkGen select count =
  Random.mkStdGen . (+select) . (*count) . BinaryUtils.decodeS . Guid.bs

resultGuid ::
  Expression.Expression def (SugarInfer.Payload t inferred stored) -> Guid
resultGuid = Lens.view (Expression.ePayload . SugarInfer.plGuid)

resultStored ::
  Expression.Expression def (SugarInfer.Payload t inferred stored) -> stored
resultStored = Lens.view (Expression.ePayload . SugarInfer.plStored)

resultInferred ::
  Expression.Expression def (SugarInfer.Payload t inferred stored) -> inferred
resultInferred = Lens.view (Expression.ePayload . SugarInfer.plInferred)

toPayloadMM :: SugarInfer.Payload (Tagged (m ())) NoInferred NoStored -> PayloadMM m
toPayloadMM =
  Lens.set SugarInfer.plInferred Nothing .
  Lens.set SugarInfer.plStored Nothing

mkExpression ::
  (Typeable1 m, MonadA m) => DataIRef.ExpressionM m (PayloadMM m) ->
  ExpressionBody m (Expression m) -> SugarM m (Expression m)
mkExpression exprI expr = do
  sugarContext <- SugarM.readContext
  inferredTypes <-
    zipWithM
    ( fmap (convertExpressionI . fmap toPayloadMM)
    . SugarInfer.resultFromPure
    ) seeds types
  return
    Expression
    { _rGuid = resultGuid exprI
    , _rExpressionBody = expr
    , _rPayload = Payload
      { _plInferredTypes = inferredTypes
      , _plActions =
        mkActions sugarContext <$>
        traverse (SugarInfer.ntraversePayload pure id) exprI
      , _plNextHole = Nothing
      }
    , _rHiddenGuids = []
    , _rPresugaredExpression =
      fmap (StorePoint . Property.value) .
      Lens.view SugarInfer.plStored <$> exprI
    }
  where
    seeds = RandomUtils.splits . mkGen 0 3 $ resultGuid exprI
    types = maybe [] iwcInferredTypes $ resultInferred exprI

replaceWith :: MonadA m => Stored m -> Stored m -> T m Guid
replaceWith parentP replacerP = do
  Property.set parentP replacerI
  return $ DataIRef.exprGuid replacerI
  where
    replacerI = Property.value replacerP

deleteParamRef ::
  MonadA m => Guid -> Expression.Expression def (Stored m) -> T m ()
deleteParamRef param =
  Lens.mapMOf_ refs
  (DataOps.setToHole . Lens.view Expression.ePayload)
  where
    refs =
      Lens.folding ExprUtil.subExpressions .
      Lens.filtered (Lens.anyOf (Expression.eBody . ExprUtil.bodyParameterRef) (== param))

mkFuncParamActions ::
  MonadA m => Stored m ->
  Expression.Lambda (Expression.Expression def (Stored m)) ->
  FuncParamActions m
mkFuncParamActions lambdaProp (Expression.Lambda _ param _paramType body) =
  FuncParamActions
  { _fpListItemActions =
    ListItemActions
    { _itemDelete = do
        deleteParamRef param body
        replaceWith lambdaProp $ body ^. Expression.ePayload
    , _itemAddNext = fmap fst . DataOps.lambdaWrap $ body ^. Expression.ePayload
    }
  , _fpGetExample =
      return
      Expression
      { _rGuid = Guid.augment "EXAMPLE" param
      , _rExpressionBody = ExpressionAtom "NotImplemented"
      , _rPayload = Payload [] Nothing Nothing
      , _rHiddenGuids = []
      , _rPresugaredExpression = Nothing <$ ExprUtil.pureHole
      }
  }

data IsDependent = Dependent | NonDependent
  deriving (Eq, Ord, Show)

convertFuncParam ::
  (Typeable1 m, MonadA m) => Expression.Lambda (DataIRef.ExpressionM m (PayloadMM m)) ->
  DataIRef.ExpressionM m (PayloadMM m) ->
  SugarM m (IsDependent, FuncParam m (Expression m))
convertFuncParam lam@(Expression.Lambda _ paramGuid paramType _) expr = do
  paramTypeS <- convertExpressionI paramType
  let
    fp = FuncParam
      { _fpGuid = paramGuid
      , _fpHiddenLambdaGuid = Just $ resultGuid expr
      , _fpType = removeSuccessfulType paramTypeS
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
  (Typeable1 m, MonadA m) => Expression.Lambda (DataIRef.ExpressionM m (PayloadMM m)) ->
  DataIRef.ExpressionM m (PayloadMM m) ->
  SugarM m ((IsDependent, FuncParam m (Expression m)), Expression m)
convertLambda lam expr = do
  param <- convertFuncParam lam expr
  result <- convertExpressionI (lam ^. Expression.lambdaResult)
  return (param & Lens._2 . fpType %~ setNextHole result, result)

fAllParams :: Func m expr -> [FuncParam m expr]
fAllParams (Func depParams params _) = depParams ++ params

convertFunc ::
  (MonadA m, Typeable1 m) => Expression.Lambda (DataIRef.ExpressionM m (PayloadMM m)) ->
  Convertor m
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

convertPi :: (MonadA m, Typeable1 m) => Expression.Lambda (DataIRef.ExpressionM m (PayloadMM m)) -> Convertor m
convertPi lambda exprI = do
  ((_, param), sBody) <- convertLambda lambda exprI
  mkExpression exprI $ ExpressionPi DontHaveParens
    Pi
    { pParam = Lens.over fpType addApplyChildParens param
    , pResultType = removeSuccessfulType sBody
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

isPolymorphicFunc :: DataIRef.ExpressionM m (PayloadMM m) -> Bool
isPolymorphicFunc funcI =
  maybe False
  (ExprUtil.isDependentPi . Infer.iType . iwcInferred) $
  resultInferred funcI

exprStoredGuid ::
  Lens.Fold
  (Expression.Expression def (SugarInfer.Payload t i (Maybe (Stored m)))) Guid
exprStoredGuid =
  Expression.ePayload .
  SugarInfer.plStored .
  traverse .
  Property.pVal .
  Lens.to DataIRef.exprGuid

subExpressionGuids ::
  Lens.Fold
  (Expression.Expression def (SugarInfer.Payload t i (Maybe (Stored m)))) Guid
subExpressionGuids = Lens.folding ExprUtil.subExpressions . exprStoredGuid

infixr 9 `orElse`
orElse :: Monad m => m (Maybe a) -> m a -> m a
orElse a b = maybe b return =<< a

convertApply ::
  (Typeable1 m, MonadA m) =>
  Expression.Apply (DataIRef.ExpressionM m (PayloadMM m)) ->
  Convertor m
convertApply app@(Expression.Apply _ argI) exprI = do
  -- if we're an apply of the form (nil T): Return an empty list
  specialFunctions <- SugarM.scSpecialFunctions <$> SugarM.readContext
  convertApplyEmptyList app specialFunctions exprI
    `orElse` do
      argS <- convertExpressionI argI
      convertApplyList app argS specialFunctions exprI
        `orElse`
        convertApplyNormal app argS exprI

convertApplyNormal ::
  (Typeable1 m, MonadA m) =>
  Expression.Apply (DataIRef.ExpressionM m (PayloadMM m)) ->
  Expression m -> Convertor m
convertApplyNormal (Expression.Apply funcI argI) argS exprI = do
  funcS <- convertExpressionI funcI
  let apply = Expression.Apply (funcS, funcI) (argS, argI)
  case funcS ^. rExpressionBody of
    ExpressionSection _ section ->
      applyOnSection section apply exprI
    _ -> convertApplyPrefix apply exprI

setListGuid :: Guid -> Expression m -> Expression m
setListGuid consistentGuid e = e
  & rGuid .~ consistentGuid
  & rHiddenGuids %~ (e ^. rGuid :)

mkListAddFirstItem ::
  MonadA m => Anchors.SpecialFunctions (Tag m) -> Stored m -> T m Guid
mkListAddFirstItem specialFunctions =
  fmap (DataIRef.exprGuid . snd) . DataOps.addListItem specialFunctions

convertApplyEmptyList ::
  (Typeable1 m, MonadA m) =>
  Expression.Apply (DataIRef.ExpressionM m (PayloadMM m)) ->
  Anchors.SpecialFunctions (Tag m) ->
  DataIRef.ExpressionM m (PayloadMM m) ->
  SugarM m (Maybe (Expression m))
convertApplyEmptyList app@(Expression.Apply funcI _) specialFunctions exprI
  | Lens.anyOf
    (Expression.eBody . ExprUtil.bodyDefinitionRef)
    (== Anchors.sfNil specialFunctions)
    funcI
  = Just . (rHiddenGuids <>~ (app ^.. Lens.traversed . subExpressionGuids)) .
    setListGuid consistentGuid <$>
    (mkExpression exprI . ExpressionList)
    (List [] (mkListActions <$> resultStored exprI))
  | otherwise = pure Nothing
  where
    consistentGuid = Guid.augment "list" (resultGuid exprI)
    mkListActions exprS =
      ListActions
      { addFirstItem = mkListAddFirstItem specialFunctions exprS
      , replaceNil = DataIRef.exprGuid <$> DataOps.setToHole exprS
      }

convertApplyList ::
  (Typeable1 m, MonadA m) =>
  Expression.Apply (DataIRef.ExpressionM m (PayloadMM m)) ->
  Expression m ->
  Anchors.SpecialFunctions (Tag m) ->
  DataIRef.ExpressionM m (PayloadMM m) ->
  SugarM m (Maybe (Expression m))
convertApplyList (Expression.Apply funcI argI) argS specialFunctions exprI =
  case (funcI ^? eApply, argS ^. rExpressionBody) of
  ( Just (Expression.Apply funcFuncI funcArgI)
    , ExpressionList (List innerValues innerListMActions)
    )
    -- exprI@(funcI@(funcFuncI funcArgI) argI)
    | Lens.anyOf
      (eApply . Expression.applyFunc . Expression.eBody . ExprUtil.bodyDefinitionRef)
      (== Anchors.sfCons specialFunctions)
      funcFuncI
      -- exprI@(funcI@(funcFuncI@(cons _) funcArgI) argI)
      -> Just <$> do
        listItemExpr <- convertExpressionI funcArgI
        let
          listItem =
            ListItem
            { liExpr =
              listItemExpr
              & setNextHole argS
              & rHiddenGuids <>~
                concat
                [ funcFuncI ^.. subExpressionGuids
                , funcI ^.. exprStoredGuid
                , argS ^. rHiddenGuids
                ]
            , liMActions = do
                addNext <- addFirstItem <$> innerListMActions
                exprProp <- resultStored exprI
                argProp <- resultStored argI
                return ListItemActions
                  { _itemAddNext = addNext
                  , _itemDelete = replaceWith exprProp argProp
                  }
            }
          mListActions = do
            exprS <- resultStored exprI
            innerListActions <- innerListMActions
            pure ListActions
              { addFirstItem = mkListAddFirstItem specialFunctions exprS
              , replaceNil = replaceNil innerListActions
              }
        setListGuid (argS ^. rGuid) <$>
          (mkExpression exprI . ExpressionList)
          (List (listItem : innerValues) mListActions)
  _ -> pure Nothing

eApply :: Traversal' (Expression.Expression def a) (Expression.Apply (Expression.Expression def a))
eApply = Expression.eBody . Expression._BodyApply

subExpressions :: Expression m -> [Expression m]
subExpressions x = x : x ^.. rExpressionBody . Lens.traversed . Lens.folding subExpressions

setNextHole :: MonadA m => Expression m -> Expression m -> Expression m
setNextHole dest =
  case dest ^? subHoles of
  Just hole ->
    -- The mplus ignores holes that are already set:
    Lens.mapped . plNextHole %~ (`mplus` Just hole)
  Nothing -> id
  where
    subHoles =
      Lens.folding subExpressions .
      Lens.filtered (Lens.notNullOf (rExpressionBody . _ExpressionHole))

applyOnSection ::
  (MonadA m, Typeable1 m) => Section (Expression m) ->
  Expression.Apply (Expression m, DataIRef.ExpressionM m (PayloadMM m)) ->
  Convertor m
applyOnSection (Section Nothing op Nothing) (Expression.Apply (_, funcI) arg@(argRef, _)) exprI
  | isPolymorphicFunc funcI = do
    newOpRef <-
      convertApplyPrefix (Expression.Apply (op, funcI) arg) exprI
    mkExpression exprI . ExpressionSection DontHaveParens $
      Section Nothing (removeSuccessfulType newOpRef) Nothing
  | otherwise =
    mkExpression exprI . ExpressionSection DontHaveParens $
    Section (Just (addApplyChildParens argRef)) op Nothing
applyOnSection (Section (Just left) op Nothing) (Expression.Apply _ (argRef, _)) exprI =
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
  (MonadA m, Typeable1 m) => Expression.Apply (Expression m, DataIRef.ExpressionM m (PayloadMM m)) -> Convertor m
convertApplyPrefix (Expression.Apply (funcRef, funcI) (argRef, _)) applyI = do
  sugarContext <- SugarM.readContext
  let
    newArgRef = addCallWithNextArg $ addParens argRef
    fromMaybeStored = traverse (SugarInfer.ntraversePayload pure id)
    onStored expr f = maybe id f $ fromMaybeStored expr
    addCallWithNextArg =
      onStored applyI $ \applyS ->
        rPayload . plActions . Lens.mapped . callWithNextArg .~
        mkCallWithArg sugarContext applyS
    newFuncRef =
      setNextHole newArgRef .
      addApplyChildParens .
      removeSuccessfulType $
      funcRef
    makeFullApply = makeApply newFuncRef
    makeApply f =
      mkExpression applyI . ExpressionApply DontHaveParens $
      Expression.Apply f newArgRef
  if isPolymorphicFunc funcI
    then
      case funcRef ^. rExpressionBody of
      ExpressionPolymorphic (Polymorphic g compact full) ->
        makePolymorphic g compact =<< makeApply full
      ExpressionGetVariable varRef ->
        makePolymorphic (resultGuid funcI) varRef =<< makeFullApply
      _ -> makeFullApply
    else
      makeFullApply
  where
    expandedGuid = Guid.combine (resultGuid applyI) $ Guid.fromString "polyExpanded"
    makePolymorphic g compact fullExpression =
      mkExpression applyI $ ExpressionPolymorphic Polymorphic
        { pFuncGuid = g
        , pCompact = compact
        , pFullExpression =
          Lens.set rGuid expandedGuid $ removeInferredTypes fullExpression
        }

convertGetVariable :: (MonadA m, Typeable1 m) => Expression.VariableRef (DefI (Tag m)) -> Convertor m
convertGetVariable varRef exprI = do
  isInfix <- SugarM.liftTransaction $ Infix.isInfixVar varRef
  getVarExpr <- removeParamType <$> mkExpression exprI (ExpressionGetVariable varRef)
  if isInfix
    then
      mkExpression exprI .
      ExpressionSection HaveParens $
      Section Nothing (removeInferredTypes getVarExpr) Nothing
    else return getVarExpr
  where
    removeParamType
      | Lens.notNullOf Expression._ParameterRef varRef = removeSuccessfulType
      | otherwise = id

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
      return $ DataIRef.exprGuid clip

inferOnTheSide ::
  MonadA m =>
  Infer.Context (DefI (Tag m)) ->
  Infer.Scope (DefI (Tag m)) ->
  Infer.Loaded (DefI (Tag m)) () ->
  Maybe (DataIRef.ExpressionM m ())
inferOnTheSide holeInferContext scope loaded =
  void . Infer.iType . Lens.view Expression.ePayload <$>
  SugarInfer.inferMaybe_ loaded sideInferContext node
  where
    (node, sideInferContext) =
      (`runState` holeInferContext) $ Infer.newNodeWithScope scope

mkHoleResult ::
  (MonadA m, Typeable1 m) => SugarM.Context m ->
  DataIRef.ExpressionM m (SugarInfer.Payload (Tag m) i (Stored m)) ->
  DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)), Maybe (StorePoint (Tag m))) ->
  HoleResult m
mkHoleResult sugarContext exprI res =
  HoleResult
  { _holeResultInferred = fst <$> res
  , _holeResultConvert = convertHoleResult
  , _holeResultPick = pick
  , _holeResultPickPrefix = void pick
  }
  where
    cp = SugarM.scCodeAnchors sugarContext
    convertHoleResult =
      SugarM.runPure cp . convertExpressionI .
      (Lens.mapped . SugarInfer.plInferred %~ Just) .
      (Lens.mapped . SugarInfer.plStored .~ Nothing) .
      SugarInfer.resultFromInferred gen $ fst <$> res
    gen =
      Random.mkStdGen $
      hashWithSalt 0 (show (void res), show guid)
    guid = resultGuid exprI
    pick = pickResult exprI $ ExprUtil.randomizeParamIds gen res

memoBy ::
  (Cache.Key k, Binary v, MonadA m) =>
  k -> m v -> StateT Cache m v
memoBy k act = Cache.memoS (const act) k

convertTypeCheckedHoleH ::
  (MonadA m, Typeable1 m) => SugarM.Context m -> Maybe (T m Guid) ->
  InferredWC (Tag m) -> Convertor m
convertTypeCheckedHoleH
  sugarContext mPaste iwc exprI =
    chooseHoleType (iwcInferredValues iwc) plainHole inferredHole
  where
    eGuid = resultGuid exprI
    inferState = SugarM.scHoleInferState sugarContext
    contextHash = SugarM.scMContextHash sugarContext
    inferred = iwcInferred iwc
    scope = Infer.nScope $ Infer.iPoint inferred
    inferResult expr = do
      loaded <- lift $ SugarInfer.load Nothing expr
      let point = Infer.iPoint inferred
      memoBy (loaded, token, point, 'r') . return $
        SugarInfer.inferMaybe loaded inferState point

    token = (eGuid, contextHash)
    inferExprType expr = do
      loaded <- lift $ SugarInfer.load Nothing expr
      memoBy (loaded, token, scope, 't') . return $
        inferOnTheSide inferState scope loaded
    onScopeElement (param, _typeExpr) = param
    hole =
      Hole $
      mkWritableHoleActions <$>
      traverse (SugarInfer.ntraversePayload pure id) exprI
    mkWritableHoleActions exprS =
      HoleActions
      { _holePaste = mPaste
      , _holeMDelete = Nothing
      , _holeScope = map onScopeElement . Map.toList $ Infer.iScope inferred
      , _holeInferExprType = inferExprType
      , _holeResult =
        (fmap . fmap) (mkHoleResult sugarContext exprS) .
        inferResult
      }
    inferredHole =
      mkExpression exprI .
      ExpressionInferred . (`Inferred` hole) <=<
      convertExpressionI . fmap toPayloadMM .
      SugarInfer.resultFromPure (mkGen 2 3 eGuid)
    plainHole =
      mkExpression exprI (ExpressionHole hole)

chooseHoleType ::
  [DataIRef.ExpressionM m f] -> hole -> (DataIRef.ExpressionM m f -> hole) -> hole
chooseHoleType inferredVals plain inferred =
  case inferredVals of
  [Expression.Expression { Expression._eBody = Expression.BodyLeaf Expression.Hole }] -> plain
  [inferredVal] -> inferred inferredVal
  _ -> plain

pickResult ::
  MonadA m =>
  DataIRef.ExpressionM m (SugarInfer.Payload (Tag m) i (Stored m)) ->
  DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)), Maybe (StorePoint (Tag m))) ->
  T m (Maybe Guid)
pickResult exprS =
  fmap
  ( fmap (DataIRef.exprGuid . Lens.view (Expression.ePayload . Lens._2))
  . listToMaybe . uninferredHoles . fmap swap
  ) .
  (DataIRef.writeExpressionWithStoredSubexpressions . Property.value . resultStored) exprS .
  fmap (Lens.over (Lens._1 . Lens.mapped) unStorePoint . swap)

-- Also skip param types, those can usually be inferred later, so less
-- useful to fill immediately
uninferredHoles ::
  Expression.Expression def (Infer.Inferred def, a) ->
  [Expression.Expression def (Infer.Inferred def, a)]
uninferredHoles e =
  case e ^. Expression.eBody of
  Expression.BodyLeaf Expression.Hole -> [e]
  Expression.BodyApply (Expression.Apply func _)
    | (ExprUtil.isDependentPi . Infer.iType . Lens.view (Expression.ePayload . Lens._1)) func ->
      uninferredHoles func
  Expression.BodyLam (Expression.Lambda _ _ paramType result) ->
    uninferredHoles result ++ uninferredHoles paramType
  body -> Foldable.concatMap uninferredHoles body

holeResultHasHoles :: HoleResult m -> Bool
holeResultHasHoles =
  not . null . uninferredHoles . fmap (flip (,) ()) . Lens.view holeResultInferred

convertHole :: (MonadA m, Typeable1 m) => Convertor m
convertHole exprI =
  maybe convertUntypedHole convertTypeCheckedHole $
  resultInferred exprI
  where
    convertTypeCheckedHole inferred = do
      ctx <- SugarM.readContext
      mPaste <- fmap join . traverse mkPaste $ resultStored exprI
      convertTypeCheckedHoleH ctx mPaste inferred exprI
    convertUntypedHole = mkExpression exprI . ExpressionHole $ Hole Nothing

convertLiteralInteger :: (MonadA m, Typeable1 m) => Integer -> Convertor m
convertLiteralInteger i exprI =
  mkExpression exprI . ExpressionLiteralInteger $
  LiteralInteger
  { liValue = i
  , liSetValue = setValue . Property.value <$> resultStored exprI
  }
  where
    setValue iref =
      DataIRef.writeExprBody iref . Lens.review ExprUtil.bodyLiteralInteger

convertTag :: (MonadA m, Typeable1 m) => Guid -> Convertor m
convertTag tag exprI =
  mkExpression exprI $ ExpressionTag tag

convertAtom :: (MonadA m, Typeable1 m) => String -> Convertor m
convertAtom name exprI =
  mkExpression exprI $ ExpressionAtom name

sideChannel ::
  Monad m =>
  Lens.Lens' s a ->
  Lens.LensLike m s (side, s) a (side, a)
sideChannel lens f s = (`runStateT` s) . Lens.zoom lens $ StateT f

writeRecordFields ::
  MonadA m =>
  DataIRef.ExpressionIM m -> result ->
  ( [(DataIRef.ExpressionIM m, DataIRef.ExpressionIM m)] ->
    T m
    ( result
    , [(DataIRef.ExpressionIM m, DataIRef.ExpressionIM m)]
    )
  ) ->
  T m result
writeRecordFields iref def f = do
  oldBody <- DataIRef.readExprBody iref
  case oldBody ^? Expression._BodyRecord of
    Nothing -> return def
    Just oldRecord -> do
      (res, newRecord) <- sideChannel Expression.recordFields f oldRecord
      DataIRef.writeExprBody iref $ Expression.BodyRecord newRecord
      return res

recordFieldActions ::
  MonadA m => Guid -> DataIRef.ExpressionIM m -> DataIRef.ExpressionIM m ->
  ListItemActions m
recordFieldActions defaultGuid exprIRef iref =
  ListItemActions
  { _itemDelete = action delete
  , _itemAddNext = action addNext
  }
  where
    action f = writeRecordFields iref defaultGuid $ splitFields f
    addNext (prevFields, field, nextFields) = do
      tagHole <- DataOps.newHole
      exprHole <- DataOps.newHole
      return
        ( DataIRef.exprGuid tagHole
        , prevFields ++ field : (tagHole, exprHole) : nextFields
        )
    delete (prevFields, _, nextFields) =
      return
      ( case nextFields ++ reverse prevFields of
        [] -> defaultGuid
        ((nextTagExpr, _) : _) -> DataIRef.exprGuid nextTagExpr
      , prevFields ++ nextFields
      )
    splitFields f oldFields =
      case break ((== exprIRef) . fst) oldFields of
      (prevFields, field : nextFields) -> f (prevFields, field, nextFields)
      _ -> return (defaultGuid, oldFields)

convertField ::
  (Typeable1 m, MonadA m) =>
  Kind -> Maybe (DataIRef.ExpressionIM m) -> Guid ->
  ( DataIRef.ExpressionM m (PayloadMM m)
  , DataIRef.ExpressionM m (PayloadMM m)
  ) ->
  SugarM m (RecordField m (Expression m))
convertField k mIRef defaultGuid (tagExpr, expr) = do
  tagExprS <- convertExpressionI tagExpr
  exprS <- convertExpressionI expr
  return RecordField
    { _rfMItemActions =
      recordFieldActions defaultGuid <$> resultMIRef tagExpr <*> mIRef
    , _rfTag = removeSuccessfulType tagExprS
    , _rfExpr =
        case k of
        Val -> exprS
        Type -> removeSuccessfulType exprS
    }

resultMIRef ::
  DataIRef.ExpressionM m (PayloadMM m) ->
  Maybe (DataIRef.ExpressionIM m)
resultMIRef = fmap Property.value . resultStored

convertRecord ::
  (Typeable1 m, MonadA m) =>
  Expression.Record (DataIRef.ExpressionM m (PayloadMM m)) ->
  Convertor m
convertRecord (Expression.Record k fields) exprI = do
  sFields <- mapM (convertField k (resultMIRef exprI) defaultGuid) fields
  fmap removeSuccessfulType .
    mkExpression exprI $ ExpressionRecord
    Record
    { rKind = k
    , rFields =
        FieldList
        { flItems = withNextHoles sFields
        , flMAddFirstItem = addField <$> resultMIRef exprI
        }
    }
  where
    defaultGuid = resultGuid exprI
    withNextHoles (field : rest@(nextField:_)) =
      (field
       & rfExpr %~ setNextHole (nextField ^. rfExpr))
      : withNextHoles rest
    withNextHoles xs = xs
    addField iref =
      writeRecordFields iref defaultGuid $ \recordFields -> do
        holeTagExpr <- DataOps.newHole
        holeExpr <- DataOps.newHole
        return
          ( DataIRef.exprGuid holeTagExpr
          , (holeTagExpr, holeExpr) : recordFields
          )

convertGetField ::
  (MonadA m, Typeable1 m) =>
  Expression.GetField (DataIRef.ExpressionM m (PayloadMM m)) ->
  DataIRef.ExpressionM m (PayloadMM m) ->
  SugarM m (Expression m)
convertGetField (Expression.GetField recExpr tagExpr) exprI = do
  recExprS <- convertExpressionI recExpr
  tagExprS <- convertExpressionI tagExpr
  mkExpression exprI $ ExpressionGetField
    GetField
    { _gfRecord = recExprS
    , _gfTag = removeSuccessfulType tagExprS
    }

convertExpressionI :: (Typeable1 m, MonadA m) => DataIRef.ExpressionM m (PayloadMM m) -> SugarM m (Expression m)
convertExpressionI ee =
  ($ ee) $
  case ee ^. Expression.eBody of
  Expression.BodyLam x@(Expression.Lambda Expression.Val _ _ _) -> convertFunc x
  Expression.BodyLam x@(Expression.Lambda Expression.Type _ _ _) -> convertPi x
  Expression.BodyApply x -> convertApply x
  Expression.BodyRecord x -> convertRecord x
  Expression.BodyGetField x -> convertGetField x
  Expression.BodyLeaf (Expression.GetVariable x) -> convertGetVariable x
  Expression.BodyLeaf (Expression.LiteralInteger x) -> convertLiteralInteger x
  Expression.BodyLeaf (Expression.Tag x) -> convertTag x
  Expression.BodyLeaf Expression.Hole -> convertHole
  Expression.BodyLeaf Expression.Set -> convertAtom "Set"
  Expression.BodyLeaf Expression.IntegerType -> convertAtom "Int"
  Expression.BodyLeaf Expression.TagType -> convertAtom "Tag"

-- Check no holes
isCompleteType :: Expression.Expression def () -> Bool
isCompleteType =
  Lens.nullOf
  ( Lens.folding ExprUtil.subExpressions
  . Expression.eBody . Expression._BodyLeaf . Expression._Hole
  )

convertExpressionPure ::
  (MonadA m, Typeable1 m, RandomGen g) =>
  Anchors.CodeProps m -> g ->
  DataIRef.ExpressionM m () -> T m (Expression m)
convertExpressionPure cp gen =
  SugarM.runPure cp . convertExpressionI . fmap toPayloadMM .
  SugarInfer.resultFromPure gen

convertDefinitionParams ::
  (MonadA m, Typeable1 m) =>
  DataIRef.ExpressionM m (PayloadMM m) ->
  SugarM m
  ( [FuncParam m (Expression m)]
  , [FuncParam m (Expression m)]
  , DataIRef.ExpressionM m (PayloadMM m)
  )
convertDefinitionParams expr =
  case expr ^. Expression.eBody of
  Expression.BodyLam lambda@(Expression.Lambda Expression.Val _ _ body) -> do
    (isDependent, fp) <- convertFuncParam lambda expr
    (depParams, params, deepBody) <- convertDefinitionParams body
    return $
      case isDependent of
      Dependent -> (fp : depParams, params, deepBody)
      NonDependent -> ([], fp : depParams ++ params, deepBody)
  _ -> return ([], [], expr)

convertWhereItems ::
  (MonadA m, Typeable1 m) => DataIRef.ExpressionM m (PayloadMM m) ->
  SugarM m ([WhereItem m], DataIRef.ExpressionM m (PayloadMM m))
convertWhereItems
  topLevel@Expression.Expression
  { Expression._eBody = Expression.BodyApply apply@Expression.Apply
  { Expression._applyFunc = Expression.Expression
  { Expression._eBody = Expression.BodyLam lambda@Expression.Lambda
  { Expression._lambdaKind = Expression.Val
  , Expression._lambdaParamId = param
  , Expression._lambdaParamType = Expression.Expression
  { Expression._eBody = Expression.BodyLeaf Expression.Hole
  }
  , Expression._lambdaResult = body
  }}}} = do
    value <- convertDefinitionContent $ apply ^. Expression.applyArg
    let
      mkWIActions topLevelProp bodyStored =
        ListItemActions
        { _itemDelete = do
             deleteParamRef param bodyStored
             replaceWith topLevelProp $ bodyStored ^. Expression.ePayload
        , _itemAddNext = fmap fst $ DataOps.redexWrap topLevelProp
        }
      item = WhereItem
        { wiValue = value
        , wiGuid = param
        , wiHiddenGuids =
            map resultGuid [topLevel, lambda ^. Expression.lambdaParamType]
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
  Expression.Expression def (SugarInfer.Payload t inferred (Maybe (Stored m))) ->
  T m Guid
addStoredParam
  Expression.Expression
  { Expression._ePayload =
    SugarInfer.Payload { SugarInfer._plStored = Just prop } } =
  fmap fst $ DataOps.lambdaWrap prop
addStoredParam
  Expression.Expression
  { Expression._eBody =
    Expression.BodyLam
    Expression.Lambda
    { Expression._lambdaKind = Expression.Val
    , Expression._lambdaResult = body
    }
  } = addStoredParam body
addStoredParam _ =
  error $
  "Non-stored can only be lambda added by implicit " ++
  "type-variables which must contain a stored in its body"

assertedGetProp ::
  String ->
  Expression.Expression def (SugarInfer.Payload t inferred (Maybe stored)) -> stored
assertedGetProp _
  Expression.Expression
  { Expression._ePayload =
    SugarInfer.Payload { SugarInfer._plStored = Just prop } } = prop
assertedGetProp msg _ = error msg

convertDefinitionContent ::
  (MonadA m, Typeable1 m) =>
  DataIRef.ExpressionM m (PayloadMM m) ->
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
  (MonadA m, Typeable1 m) =>
  Anchors.CodeProps m -> DefI (Tag m) ->
  CT m (Definition m)
loadConvertDefI cp defI =
  convertDefI =<< lift (Load.loadDefinitionClosure defI)
  where
    convertDefBody (Definition.BodyBuiltin builtin) =
      fmap return . convertDefIBuiltin builtin
    convertDefBody (Definition.BodyExpression exprLoaded) =
      convertDefIExpression cp exprLoaded
    convertDefI (Definition.Definition defBody typeLoaded) = do
      body <- convertDefBody defBody defI typeLoaded
      typeS <-
        lift .
        convertExpressionPure cp (mkGen 2 3 (IRef.guid defI)) $
        void typeLoaded
      return Definition
        { drGuid = IRef.guid defI
        , drBody = body
        , drType = typeS
        }

convertDefIBuiltin ::
  MonadA m =>
  Definition.Builtin -> DefI (Tag m) ->
  Load.LoadedClosure (Tag m) -> DefinitionBody m
convertDefIBuiltin (Definition.Builtin name) defI typeIRef =
  DefinitionBodyBuiltin DefinitionBuiltin
    { biName = name
    , biMSetName = Just setName
    }
  where
    typeI = typeIRef ^. Expression.ePayload
    setName =
      Transaction.writeIRef defI .
      (`Definition.Definition` Load.irefOfClosure typeI) .
      Definition.BodyBuiltin . Definition.Builtin

convertDefIExpression ::
  (MonadA m, Typeable1 m) => Anchors.CodeProps m ->
  Load.LoadedClosure (Tag m) -> DefI (Tag m) -> Load.LoadedClosure (Tag m) ->
  CT m (DefinitionBody m)
convertDefIExpression cp exprLoaded defI typeI = do
  inferredLoadedResult <-
    SugarInfer.inferLoadedExpression
    inferLoadedGen (Just defI) exprLoaded initialInferState
  let
    inferredTypeP =
      void . Infer.iType . iwcInferred . resultInferred $
      inferredLoadedResult ^. SugarInfer.ilrExpr
    typesMatch =
      on (==) ExprUtil.canonizeParamIds (void typeI) inferredTypeP
    mkNewType = do
      inferredTypeS <-
        convertExpressionPure cp iTypeGen
        inferredTypeP
      return DefinitionNewType
        { dntNewType = inferredTypeS
        , dntAcceptNewType =
          (Property.set . Load.propertyOfClosure) (typeI ^. Expression.ePayload) =<<
          DataIRef.newExpression inferredTypeP
        }
  context <- lift $ SugarM.mkContext cp (Just defI) (Just reinferRoot) inferredLoadedResult
  lift . SugarM.run context $ do
    content <-
      convertDefinitionContent .
      (fmap . Lens.over SugarInfer.plInferred) Just $
      inferredLoadedResult ^. SugarInfer.ilrExpr
    mNewType <-
      if inferredLoadedResult ^. SugarInfer.ilrSuccess && not typesMatch && isCompleteType inferredTypeP
      then fmap Just $ SugarM.liftTransaction mkNewType
      else return Nothing
    return $ DefinitionBodyExpression DefinitionExpression
      { deContent = content
      , deMNewType = mNewType
      , deIsTypeRedundant = inferredLoadedResult ^. SugarInfer.ilrSuccess && typesMatch
      }
  where
    initialInferState = Infer.initial (Just defI)
    reinferRoot key = do
      loaded <-
        lift $ do
          reloadedRoot <-
            DataIRef.readExpression . Load.irefOfClosure $
            exprLoaded ^. Expression.ePayload
          SugarInfer.load (Just defI) (void reloadedRoot)
      memoBy (key, loaded, initialInferState, "reinfer root" :: String) .
        return . isJust $ uncurry (SugarInfer.inferMaybe_ loaded)
        initialInferState

    iTypeGen = mkGen 0 3 $ IRef.guid defI
    inferLoadedGen = mkGen 1 3 $ IRef.guid defI

--------------

removeSuccessfulType :: Expression m -> Expression m
removeSuccessfulType =
  rPayload . plInferredTypes %~ removeIfNoErrors
  where
    removeIfNoErrors [_] = []
    removeIfNoErrors xs = xs

removeInferredTypes :: Expression m -> Expression m
removeInferredTypes = rPayload . plInferredTypes .~ []

removeTypes :: MonadA m => Expression m -> Expression m
removeTypes = Lens.mapped . plInferredTypes .~ []
