{-# LANGUAGE FlexibleContexts, OverloadedStrings, ConstraintKinds, TypeFamilies, Rank2Types #-}
module Lamdu.CodeEdit.Sugar
  ( Definition(..), drGuid, drType, drBody
  , DefinitionBody(..)
  , ListItemActions(..), itemAddNext, itemDelete
  , FuncParamActions(..), fpListItemActions, fpGetExample
  , DefinitionExpression(..), deContent, deIsTypeRedundant, deMNewType
  , DefinitionContent(..), DefinitionNewType(..)
  , DefinitionBuiltin(..)
  , Actions(..)
    , giveAsArg, callWithArg, callWithNextArg
    , setToHole, replaceWithNewHole, cut, giveAsArgToOperator
  , ExpressionBody(..)
  , Payload(..), plInferredTypes, plActions, plNextHole
  , ExpressionP(..)
    , rGuid, rExpressionBody, rPayload, rHiddenGuids, rPresugaredExpression
  , NameSource(..), Name(..), NoName
  , DefinitionN
  , Expression, ExpressionN
  , ExpressionBodyN
  , WhereItem(..)
  , ListItem(..), ListActions(..), List(..)
  , RecordField(..), rfMItemActions, rfTag, rfExpr
  , Kind(..), Record(..), FieldList(..), GetField(..)
  , GetVar(..), VarType(..)
  , Func(..)
  , FuncParam(..), fpGuid, fpHiddenLambdaGuid, fpType, fpMActions
  , Pi(..)
  , Section(..)
  , Hole(..), holeScope, holeMActions
  , ScopeItem(..), siParamGuid
  , HoleActions(..)
    , holePaste, holeMDelete, holeResult, holeInferExprType
  , StorePoint
  , HoleResult(..)
    , holeResultInferred
    , holeResultConverted
    , holeResultPick, holeResultPickPrefix
  , holeResultHasHoles
  , LiteralInteger(..)
  , TagG(..), tagName, tagGuid
  , Inferred(..), iValue, iHole
  , Collapsed(..), pFuncGuid, pCompact, pFullExpression
  , HasParens(..)
  , loadConvertDefI
  , removeTypes
  , PrefixAction, emptyPrefixAction
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens (Traversal')
import Control.Lens.Operators
import Control.Monad ((<=<), guard, join, mplus, void, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), runState, mapStateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Function (on)
import Data.Hashable (hashWithSalt)
import Data.Maybe (fromMaybe, listToMaybe, isJust)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag, Tagged)
import Data.Traversable (traverse)
import Data.Tuple (swap)
import Data.Typeable (Typeable1)
import Lamdu.CodeEdit.Sugar.Infer (InferredWC, NoInferred(..), Stored, NoStored)
import Lamdu.CodeEdit.Sugar.Monad (SugarM)
import Lamdu.CodeEdit.Sugar.Types
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
import qualified Lamdu.CodeEdit.Sugar.AddNames as AddNames
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
  SugarM m (ExpressionU m)

mkCutter :: MonadA m => Anchors.CodeProps m -> DataIRef.ExpressionI (Tag m) -> T m Guid -> T m Guid
mkCutter cp expr replaceWithHole = do
  _ <- DataOps.newClipboard cp expr
  replaceWithHole

checkReinferSuccess :: MonadA m => SugarM.Context m -> String -> T m a -> CT m Bool
checkReinferSuccess sugarContext key act =
  case sugarContext ^. SugarM.scMReinferRoot of
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
  , _cut = mkCutter (sugarContext ^. SugarM.scCodeAnchors) (Property.value stored) $ doReplace DataOps.replaceWithHole
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
  ExpressionBodyU m -> SugarM m (ExpressionU m)
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
  FuncParamActions NoName m
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
  SugarM m (IsDependent, FuncParam NoName m (ExpressionU m))
convertFuncParam lam@(Expression.Lambda _ paramGuid paramType _) expr = do
  paramTypeS <- convertExpressionI paramType
  let
    fp = FuncParam
      { _fpName = ()
      , _fpGuid = paramGuid
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
  SugarM m ((IsDependent, FuncParam NoName m (ExpressionU m)), ExpressionU m)
convertLambda lam expr = do
  param <- convertFuncParam lam expr
  result <- convertExpressionI (lam ^. Expression.lambdaResult)
  return (param & Lens._2 . fpType %~ setNextHole result, result)

fAllParams :: Func NoName m expr -> [FuncParam NoName m expr]
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
    maybeEta = do
      (_, Expression.Apply func arg) <- sBody ^? rExpressionBody . _ExpressionApply
      argVar <- arg ^? rExpressionBody . _ExpressionGetVar
      guard $ gvIdentifier argVar == param ^. fpGuid
      funcVar <- func ^? rExpressionBody . _ExpressionGetVar
      pure (func ^. rGuid, funcVar)
  fullExpr <- mkExpression exprI $ ExpressionFunc DontHaveParens newFunc
  case maybeEta of
    Nothing -> pure fullExpr
    Just (funcGuid, funcVar) ->
      makeCollapsed exprI funcGuid funcVar fullExpr
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

addParens :: ExpressionP name m pl -> ExpressionP name m pl
addParens =
  Lens.over rExpressionBody addParensBody
  where
    addParensBody (ExpressionInferred (Inferred val hole)) =
      ExpressionInferred $ Inferred (addParens val) hole
    addParensBody (ExpressionCollapsed (Collapsed g compact full)) =
      ExpressionCollapsed . Collapsed g compact $
      addParens full
    addParensBody x = Lens.set eHasParens HaveParens x

addApplyChildParens :: ExpressionP name m pl -> ExpressionP name m pl
addApplyChildParens x =
  case x ^. rExpressionBody of
  ExpressionApply{} -> x
  ExpressionCollapsed{} -> x
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
convertApply app@(Expression.Apply funcI argI) exprI = do
  -- if we're an apply of the form (nil T): Return an empty list
  specialFunctions <- (^. SugarM.scSpecialFunctions) <$> SugarM.readContext
  convertApplyEmptyList app specialFunctions exprI
    `orElse` do
      argS <- convertExpressionI argI
      convertApplyList app argS specialFunctions exprI
        `orElse`
        convertApplyNormal funcI argS exprI

convertApplyNormal ::
  (Typeable1 m, MonadA m) =>
  DataIRef.ExpressionM m (PayloadMM m) -> ExpressionU m -> Convertor m
convertApplyNormal funcI argS exprI = do
  funcS <- convertExpressionI funcI
  case funcS ^. rExpressionBody of
    ExpressionSection _ section ->
      applyOnSection section funcS funcI argS exprI
    _ -> convertApplyPrefix funcS funcI sugaredArg exprI
  where
    sugaredArg =
      fromMaybe argS $ do
        [field] <- argS ^? rExpressionBody . _ExpressionRecord . rFields . flItems
        pure $ field ^. rfExpr

setListGuid :: Guid -> ExpressionU m -> ExpressionU m
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
  SugarM m (Maybe (ExpressionU m))
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
  ExpressionU m ->
  Anchors.SpecialFunctions (Tag m) ->
  DataIRef.ExpressionM m (PayloadMM m) ->
  SugarM m (Maybe (ExpressionU m))
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

subExpressions :: ExpressionU m -> [ExpressionU m]
subExpressions x = x : x ^.. rExpressionBody . Lens.traversed . Lens.folding subExpressions

setNextHole :: MonadA m => ExpressionU m -> ExpressionU m -> ExpressionU m
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
  (MonadA m, Typeable1 m) => Section (ExpressionU m) ->
  ExpressionU m -> DataIRef.ExpressionM m (PayloadMM m) -> ExpressionU m ->
  Convertor m
applyOnSection (Section Nothing op Nothing) _ funcI argRef exprI
  | isPolymorphicFunc funcI = do
    newOpRef <-
      convertApplyPrefix op funcI argRef exprI
    mkExpression exprI . ExpressionSection DontHaveParens $
      Section Nothing (removeSuccessfulType newOpRef) Nothing
  | otherwise =
    mkExpression exprI . ExpressionSection DontHaveParens $
    Section (Just (addApplyChildParens argRef)) op Nothing
applyOnSection (Section (Just left) op Nothing) _ _ argRef exprI =
  mkExpression exprI . ExpressionSection DontHaveParens $
  on (Section . Just) (setNextHole right) left op (Just right)
  where
    -- TODO: Handle left/right-associativity
    isSameOp (ExpressionCollapsed p0) (ExpressionCollapsed p1) =
      on isSameVar (^. pCompact) p0 p1
    isSameOp (ExpressionGetVar v0) (ExpressionGetVar v1) =
      isSameVar v0 v1
    isSameOp _ _ = False
    isSameVar = on (==) gvIdentifier
    right =
      case argRef ^. rExpressionBody of
      ExpressionSection _ (Section (Just _) rightOp (Just _))
        | on isSameOp (Lens.view rExpressionBody) op rightOp -> argRef
      _ -> addApplyChildParens argRef
applyOnSection _ funcRef funcI argRef exprI = convertApplyPrefix funcRef funcI argRef exprI

convertApplyPrefix ::
  (MonadA m, Typeable1 m) =>
  ExpressionU m -> DataIRef.ExpressionM m (PayloadMM m) -> ExpressionU m ->
  Convertor m
convertApplyPrefix funcRef funcI argRef applyI = do
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
      ExpressionCollapsed (Collapsed g compact full) ->
        makeCollapsed applyI g compact =<< makeApply full
      ExpressionGetVar var ->
        makeCollapsed applyI (resultGuid funcI) var =<< makeFullApply
      _ -> makeFullApply
    else
      makeFullApply

makeCollapsed ::
  (MonadA m, Typeable1 m) =>
  DataIRef.ExpressionM m (PayloadMM m) ->
  Guid -> GetVar NoName m -> ExpressionU m -> SugarM m (ExpressionU m)
makeCollapsed exprI g compact fullExpression =
  mkExpression exprI $ ExpressionCollapsed Collapsed
    { _pFuncGuid = g
    , _pCompact = compact
    , _pFullExpression =
      Lens.set rGuid expandedGuid $ removeInferredTypes fullExpression
    }
  where
    expandedGuid = Guid.combine (resultGuid exprI) $ Guid.fromString "polyExpanded"

convertGetVariable :: (MonadA m, Typeable1 m) => Expression.VariableRef (DefI (Tag m)) -> Convertor m
convertGetVariable varRef exprI = do
  isInfix <- SugarM.liftTransaction $ Infix.isInfixVar varRef
  sugarContext <- SugarM.readContext
  let
    getVar =
      GetVar
      { gvName = ()
      , gvIdentifier = DataIRef.variableRefGuid varRef
      , gvJumpTo =
          case varRef of
          Expression.ParameterRef guid -> pure guid
          Expression.DefinitionRef defI ->
            IRef.guid defI <$ DataOps.newPane (sugarContext ^. SugarM.scCodeAnchors) defI
      , gvVarType =
          case varRef of
          Expression.ParameterRef _ -> GetParameter
          Expression.DefinitionRef _ -> GetDefinition
      }
  getVarExpr <- removeParamType <$> mkExpression exprI (ExpressionGetVar getVar)
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

makeHoleResult ::
  (Typeable1 m, MonadA m) => SugarM.Context m ->
  Infer.Inferred (DefI (Tag m)) ->
  Expression.Expression (DefI (Tag m))
  (SugarInfer.Payload (Tag m) inferred (Stored m)) ->
  T m (DataIRef.ExpressionM m (Maybe (StorePoint (Tag m)))) ->
  CT m (Maybe (HoleResult NoName m))
makeHoleResult sugarContext inferred exprI makeExpr =
  lift . traverse mkHoleResult =<<
  mapStateT Transaction.forkScratch makeInferredExpr
  where
    gen expr =
      Random.mkStdGen $
      hashWithSalt 0 (show (void expr), show guid)
    makeInferredExpr = inferResult =<< lift makeExpr
    convertHoleResult res =
      SugarM.runPure (sugarContext ^. SugarM.scCodeAnchors) (sugarContext ^. SugarM.scRecordParams) .
      convertExpressionI .
      (Lens.mapped . SugarInfer.plInferred %~ Just) .
      (Lens.mapped . SugarInfer.plStored .~ Nothing) .
      SugarInfer.resultFromInferred (gen res) $ fst <$> res
    inferResult expr = do
      loaded <- lift $ SugarInfer.load Nothing expr
      let point = Infer.iPoint inferred
      memoBy (loaded, token, point, 'r') . return $
        SugarInfer.inferMaybe loaded (sugarContext ^. SugarM.scHoleInferState) point
    pick = do
      mResReal <- Cache.unmemoS makeInferredExpr
      case mResReal of
        Nothing ->
          fail $
          "Rerun of hole result maker and infer on its " ++
          "result failed after first run succeeded. Fishy transaction given!"
        Just resReal ->
          pickResult exprI $ ExprUtil.randomizeParamIds (gen resReal) resReal
    guid = resultGuid exprI
    token = (guid, sugarContext ^. SugarM.scMContextHash)
    mkHoleResult resFake = do
      converted <- convertHoleResult resFake
      return HoleResult
        { _holeResultInferred = fst <$> resFake
          -- TODO: Is it ok to use the fake result (resFake) not in the
          -- forked scratch space?
        , _holeResultConverted = converted
        , _holeResultPick = pick
        , _holeResultPickPrefix = void pick
        }

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
    inferState  = sugarContext ^. SugarM.scHoleInferState
    contextHash = sugarContext ^. SugarM.scMContextHash
    inferred = iwcInferred iwc
    scope = Infer.nScope $ Infer.iPoint inferred
    token = (eGuid, contextHash)
    inferExprType expr = do
      loaded <- lift $ SugarInfer.load Nothing expr
      memoBy (loaded, token, scope, 't') . return $
        inferOnTheSide inferState scope loaded
    onScopeElement (param, typeExpr) =
      ScopeItem
      { _siParamGuid = param
      , _siFields =
          typeExpr ^..
          Expression.eBody . Expression._BodyRecord .
          Expression.recordFields . traverse . Lens._1 .
          Expression.eBody . Expression._BodyLeaf .
          Expression._Tag
      }
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
      , _holeResult = makeHoleResult sugarContext inferred exprS
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
  Expression.BodyLam (Expression.Lambda lamKind _ paramType result) ->
    uninferredHoles result ++ do
      guard $ lamKind == Expression.Type
      uninferredHoles paramType
  body -> Foldable.concatMap uninferredHoles body

holeResultHasHoles :: HoleResult name m -> Bool
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
  mkExpression exprI . ExpressionTag $ TagG tag ()

convertAtom :: (MonadA m, Typeable1 m) => String -> Convertor m
convertAtom str exprI =
  mkExpression exprI $ ExpressionAtom str

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
  SugarM m (RecordField m (ExpressionU m))
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
    { _rKind = k
    , _rFields =
        FieldList
        { _flItems = withNextHoles sFields
        , _flMAddFirstItem = addField <$> resultMIRef exprI
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
  SugarM m (ExpressionU m)
convertGetField (Expression.GetField recExpr tagExpr) exprI = do
  recordParams <- (^. SugarM.scRecordParams) <$> SugarM.readContext
  let
    mVar = do
      tag <- tagExpr ^? Expression.eBody . Expression._BodyLeaf . Expression._Tag
      paramInfo <- Map.lookup tag recordParams
      param <- recExpr ^? Expression.eBody . ExprUtil.bodyParameterRef
      guard $ param == SugarM.piFromParameters paramInfo
      return
        GetVar
        { gvName = ()
        , gvIdentifier = tag
        , gvJumpTo = pure $ SugarM.piJumpTo paramInfo
        , gvVarType = GetParameter
        }
  case mVar of
    Just var ->
      fmap removeSuccessfulType .
      mkExpression exprI $ ExpressionGetVar var
    Nothing -> do
      recExprS <- convertExpressionI recExpr
      tagExprS <- convertExpressionI tagExpr
      mkExpression exprI $ ExpressionGetField
        GetField
        { _gfRecord = recExprS
        , _gfTag = removeSuccessfulType tagExprS
        }

convertExpressionI ::
  (Typeable1 m, MonadA m) =>
  DataIRef.ExpressionM m (PayloadMM m) -> SugarM m (ExpressionU m)
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
  DataIRef.ExpressionM m () -> T m (ExpressionU m)
convertExpressionPure cp gen =
  SugarM.runPure cp Map.empty . convertExpressionI . fmap toPayloadMM .
  SugarInfer.resultFromPure gen

convertDefinitionParams ::
  (MonadA m, Typeable1 m) =>
  [Guid] ->
  DataIRef.ExpressionM m (PayloadMM m) ->
  SugarM m
  ( [FuncParam NoName m (ExpressionU m)]
  , Maybe (FuncParam NoName m (ExpressionU m))
  , DataIRef.ExpressionM m (PayloadMM m)
  )
convertDefinitionParams usedTags expr =
  case expr ^. Expression.eBody of
  Expression.BodyLam lambda@(Expression.Lambda Expression.Val _ _ body) -> do
    (isDependent, fp) <- convertFuncParam lambda expr
    case (isDependent, fp ^. fpType . rExpressionBody) of
      (Dependent, _) -> do
        (depParams, fieldParams, deepBody) <- convertDefinitionParams usedTags body
        return (fp : depParams, fieldParams, deepBody)
      (NonDependent, ExpressionRecord (Record k fields))
        | k == Type
        && (not . null) (fields ^. flItems)
        && Lens.allOf fieldTags (`notElem` usedTags) fields ->
          return ([], Just fp, body)
      _ -> return ([], Nothing, expr)
  _ -> return ([], Nothing, expr)
  where
    fieldTags = flItems . traverse . rfTag . rExpressionBody . _ExpressionTag . tagGuid

mExtractWhere ::
  Expression.Expression def a ->
  Maybe (Expression.Apply (Expression.Expression def a), Expression.Lambda (Expression.Expression def a))
mExtractWhere expr = do
  apply <- expr ^? Expression.eBody . Expression._BodyApply
  lambda <- apply ^? Expression.applyFunc . Expression.eBody . Expression._BodyLam
  guard $ (lambda ^. Expression.lambdaKind) == Expression.Val
  -- paramType has to be Hole for this to be sugarred to Where
  lambda ^? Expression.lambdaParamType . Expression.eBody . Expression._BodyLeaf . Expression._Hole
  return (apply, lambda)

convertWhereItems ::
  (MonadA m, Typeable1 m) =>
  [Guid] ->
  DataIRef.ExpressionM m (PayloadMM m) ->
  SugarM m ([WhereItem NoName m], DataIRef.ExpressionM m (PayloadMM m))
convertWhereItems usedTags expr =
  case mExtractWhere expr of
  Nothing -> return ([], expr)
  Just (apply, lambda) -> do
    value <- convertDefinitionContent usedTags $ apply ^. Expression.applyArg
    let
      mkWIActions topLevelProp bodyStored =
        ListItemActions
        { _itemDelete = do
             deleteParamRef (lambda ^. Expression.lambdaParamId) bodyStored
             replaceWith topLevelProp $ bodyStored ^. Expression.ePayload
        , _itemAddNext = fmap fst $ DataOps.redexWrap topLevelProp
        }
      item = WhereItem
        { wiValue = value
        , wiGuid = lambda ^. Expression.lambdaParamId
        , wiHiddenGuids =
            map resultGuid [expr, lambda ^. Expression.lambdaParamType]
        , wiActions =
          mkWIActions <$>
          resultStored expr <*>
          traverse (Lens.view SugarInfer.plStored) (lambda ^. Expression.lambdaResult)
        }
    (nextItems, whereBody) <- convertWhereItems usedTags $ lambda ^. Expression.lambdaResult
    return (item : nextItems, whereBody)

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
  [Guid] ->
  DataIRef.ExpressionM m (PayloadMM m) ->
  SugarM m (DefinitionContent NoName m)
convertDefinitionContent usedTags expr = do
  (depParams, params, funcBody) <- convertDefinitionParams usedTags expr
  let
    fieldTagsLens =
      Lens._Just . fpType .
      rExpressionBody . _ExpressionRecord . rFields . flItems . traverse . rfTag
    tagExprs = params ^.. fieldTagsLens
    defTags = tagExprs >>= (^.. rExpressionBody . _ExpressionTag . tagGuid)
    paramInfos = Map.fromList $ do
      arg <- params ^.. Lens._Just . fpGuid
      tagExpr <- tagExprs
      tag <- tagExpr ^.. rExpressionBody . _ExpressionTag . tagGuid
      return (tag, SugarM.ParamInfo arg (tagExpr ^. rGuid))
  SugarM.local (SugarM.scRecordParams <>~ paramInfos) $ do
    (whereItems, whereBody) <- convertWhereItems (usedTags ++ defTags) funcBody
    bodyS <- convertExpressionI whereBody
    return DefinitionContent
      { dDepParams = depParams
      , dParams = params
      , dBody = bodyS
      , dWhereItems = whereItems
      , dAddFirstParam = addStoredParam expr
      , dAddInnermostWhereItem =
        fmap fst . DataOps.redexWrap $
        assertedGetProp "Where must be stored" whereBody
      }

loadConvertDefI ::
  (MonadA m, Typeable1 m) =>
  Anchors.CodeProps m -> DefI (Tag m) ->
  CT m (DefinitionN m)
loadConvertDefI cp defI =
  lift . AddNames.toDef =<< convertDefI =<< lift (Load.loadDefinitionClosure defI)
  where
    convertDefBody (Definition.BodyBuiltin builtin) =
      fmap return . convertDefIBuiltin builtin
    convertDefBody (Definition.BodyExpression exprLoaded) =
      convertDefIExpression cp exprLoaded
    convertDefI (Definition.Definition defBody typeLoaded) = do
      bodyS <- convertDefBody defBody defI $ Load.propertyOfClosure <$> typeLoaded
      typeS <-
        lift .
        convertExpressionPure cp (mkGen 2 3 (IRef.guid defI)) $
        void typeLoaded
      return Definition
        { _drGuid = IRef.guid defI
        , _drBody = bodyS
        , _drType = typeS
        }

convertDefIBuiltin ::
  MonadA m => Definition.Builtin -> DefI (Tag m) ->
  DataIRef.ExpressionM m (Stored m) -> DefinitionBody NoName m
convertDefIBuiltin (Definition.Builtin name) defI typeI =
  DefinitionBodyBuiltin DefinitionBuiltin
    { biName = name
    , biMSetName = Just setName
    }
  where
    typeIRef = Property.value $ typeI ^. Expression.ePayload
    setName =
      Transaction.writeIRef defI .
      (`Definition.Definition` typeIRef) .
      Definition.BodyBuiltin . Definition.Builtin

makeNewTypeForDefinition ::
  (Typeable1 m, MonadA m, RandomGen gen) =>
  Anchors.CodeProps m -> Stored m -> DataIRef.ExpressionM m () -> Bool -> Bool ->
  gen -> T m (Maybe (DefinitionNewType NoName m))
makeNewTypeForDefinition cp typeIRef inferredTypeP typesMatch success iTypeGen
  | success && not typesMatch && isCompleteType inferredTypeP =
    Just <$> mkNewType
  | otherwise = return Nothing
  where
    mkNewType = do
      inferredTypeS <-
        convertExpressionPure cp iTypeGen inferredTypeP
      return DefinitionNewType
        { dntNewType = inferredTypeS
        , dntAcceptNewType =
          Property.set typeIRef =<<
          DataIRef.newExpression inferredTypeP
        }

convertDefIExpression ::
  (MonadA m, Typeable1 m) => Anchors.CodeProps m ->
  Load.LoadedClosure (Tag m) -> DefI (Tag m) ->
  DataIRef.ExpressionM m (Stored m) ->
  CT m (DefinitionBody NoName m)
convertDefIExpression cp exprLoaded defI typeI = do
  inferredLoadedResult@SugarInfer.InferLoadedResult
    { SugarInfer._ilrExpr = ilrExpr
    , SugarInfer._ilrSuccess = success
    } <-
    SugarInfer.inferLoadedExpression
    inferLoadedGen (Just defI) exprLoaded initialInferState
  let
    inferredTypeP =
      void . Infer.iType . iwcInferred $ resultInferred ilrExpr
    typesMatch =
      on (==) ExprUtil.canonizeParamIds (void typeI) inferredTypeP
  mNewType <-
    lift $ makeNewTypeForDefinition cp (typeI ^. Expression.ePayload)
    inferredTypeP typesMatch success iTypeGen
  context <- lift $ SugarM.mkContext cp (Just defI) (Just reinferRoot) inferredLoadedResult
  lift . SugarM.run context $ do
    content <-
      convertDefinitionContent [] $
      ilrExpr & Lens.mapped . SugarInfer.plInferred %~ Just
    return $ DefinitionBodyExpression DefinitionExpression
      { _deContent = content
      , _deMNewType = mNewType
      , _deIsTypeRedundant = success && typesMatch
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

removeSuccessfulType :: Expression name m -> Expression name m
removeSuccessfulType =
  rPayload . plInferredTypes %~ removeIfNoErrors
  where
    removeIfNoErrors [_] = []
    removeIfNoErrors xs = xs

removeInferredTypes :: Expression name m -> Expression name m
removeInferredTypes = rPayload . plInferredTypes .~ []

removeTypes :: MonadA m => Expression name m -> Expression name m
removeTypes = Lens.mapped . plInferredTypes .~ []
