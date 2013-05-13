{-# LANGUAGE FlexibleContexts, OverloadedStrings, ConstraintKinds, TypeFamilies, Rank2Types, PatternGuards #-}
module Lamdu.CodeEdit.Sugar
  ( Definition(..), drName, drGuid, drType, drBody
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
  , NameSource(..), Name(..), NameHint
  , DefinitionN
  , Expression, ExpressionN
  , ExpressionBodyN
  , WhereItem(..)
  , ListItem(..), ListActions(..), List(..)
  , RecordField(..), rfMItemActions, rfTag, rfExpr
  , Kind(..), Record(..), FieldList(..), GetField(..)
  , GetVar(..), gvIdentifier, gvName, gvJumpTo, gvVarType
  , VarType(..)
  , Func(..)
  , FuncParam(..), fpName, fpGuid, fpId, fpHiddenLambdaGuid, fpType, fpMActions
  , Pi(..)
  , Section(..)
  , ScopeItem(..), _ScopeVar, _ScopeTag
  , Hole(..), holeScope, holeMActions
  , HoleResultSeed(..)
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
import Control.Lens (LensLike, Lens')
import Control.Lens.Operators
import Control.Monad (guard, join, mplus, void, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), runState, mapStateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Function (on)
import Data.Hashable (hashWithSalt)
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe, isJust)
import Data.Monoid (mconcat)
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

type PayloadM m = SugarInfer.Payload (Tag m)
type PayloadMM m =
  PayloadM m (Maybe (InferredWC (Tag m))) (Maybe (Stored m))
type ExprMM m = DataIRef.ExpressionM m (PayloadMM m)
type Convertor m = ExprMM m -> SugarM m (ExpressionU m)

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
  DataIRef.ExpressionM m (PayloadM m i (Stored m)) ->
  PrefixAction m -> CT m (Maybe (T m Guid))
mkCallWithArg sugarContext exprS prefixAction =
  guardReinferSuccess sugarContext "callWithArg" $ do
    prefixAction
    fmap DataIRef.exprGuid . DataOps.callWithArg $ resultStored exprS

mkActions ::
  MonadA m => SugarM.Context m ->
  DataIRef.ExpressionM m (PayloadM m i (Stored m)) -> Actions m
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
resultStored = Lens.view plStored

resultInferred ::
  Expression.Expression def (SugarInfer.Payload t inferred stored) -> inferred
resultInferred = Lens.view (Expression.ePayload . SugarInfer.plInferred)

toPayloadMM :: SugarInfer.Payload (Tagged (m ())) NoInferred NoStored -> PayloadMM m
toPayloadMM =
  Lens.set SugarInfer.plInferred Nothing .
  Lens.set SugarInfer.plStored Nothing

mkExpression ::
  (Typeable1 m, MonadA m) => ExprMM m ->
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

toHoles ::
  MonadA m =>
  (Expression.Expression def (DataIRef.ExpressionProperty m) -> Bool) ->
  Expression.Expression def (DataIRef.ExpressionProperty m) ->
  T m ()
toHoles predicate =
  Lens.mapMOf_ (Lens.folding ExprUtil.subExpressions . Lens.filtered predicate)
  (DataOps.setToHole . (^. Expression.ePayload))      

deleteParamRef ::
  MonadA m => Guid -> Expression.Expression def (Stored m) -> T m ()
deleteParamRef param =
  toHoles $ Lens.anyOf (Expression.eBody . ExprUtil.bodyParameterRef) (== param)

deleteFieldParamRef ::
  MonadA m => Guid -> Guid -> Expression.Expression def (Stored m) -> T m ()
deleteFieldParamRef param tagG = do
  toHoles (p . (^? Expression.eBody . Expression._BodyGetField))
  where
    p Nothing = False
    p (Just (Expression.GetField record tag)) =
      Lens.anyOf ExprUtil.exprBodyTag (== tagG) tag &&
      Lens.anyOf (Expression.eBody . ExprUtil.bodyParameterRef) (== param) record

mkFuncParamActions ::
  MonadA m => Guid -> Stored m -> Expression.Expression def (Stored m) ->
  FuncParamActions NameHint m
mkFuncParamActions param lambdaProp body =
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

convertFuncParam ::
  (Typeable1 m, MonadA m) => Expression.Lambda (ExprMM m) ->
  ExprMM m ->
  SugarM m (FuncParam NameHint m (ExpressionU m))
convertFuncParam (Expression.Lambda _k paramGuid paramType body) expr = do
  paramTypeS <- convertExpressionI paramType
  let
    fp = FuncParam
      { _fpName = Nothing
      , _fpGuid = paramGuid
      , _fpId = paramGuid -- should be unique
      , _fpHiddenLambdaGuid = Just $ resultGuid expr
      , _fpType = removeSuccessfulType paramTypeS
      , _fpMActions =
        mkFuncParamActions paramGuid
        <$> expr ^. plStored
        <*> traverse (^. SugarInfer.plStored) body
      }
  return fp

convertLambda ::
  (Typeable1 m, MonadA m) => Expression.Lambda (ExprMM m) ->
  ExprMM m ->
  SugarM m (FuncParam NameHint m (ExpressionU m), ExpressionU m)
convertLambda lam expr = do
  param <- convertFuncParam lam expr
  result <- convertExpressionI (lam ^. Expression.lambdaResult)
  return (param & fpType %~ setNextHole result, result)

fAllParams :: Func NameHint m expr -> [FuncParam NameHint m expr]
fAllParams (Func depParams params _) = depParams ++ params

convertFunc ::
  (MonadA m, Typeable1 m) => Expression.Lambda (ExprMM m) ->
  Convertor m
convertFunc lambda exprI = do
  (param, sBody) <- convertLambda lambda exprI
  let
    innerFunc =
      case sBody ^. rExpressionBody of
      ExpressionFunc _ func -> func
      _ -> Func [] [] sBody
    mNextParam = listToMaybe $ fAllParams innerFunc
    newParam = maybe id deleteToNextParam mNextParam param
    newFunc
      | isPolymorphicFunc exprI = innerFunc & fDepParams %~ (newParam :)
      | otherwise = Func [] (newParam : fAllParams innerFunc) $ innerFunc ^. fBody
    maybeEta = do
      (_, Expression.Apply func arg) <- sBody ^? rExpressionBody . _ExpressionApply
      argVar <- arg ^? rExpressionBody . _ExpressionGetVar
      guard $ argVar ^. gvIdentifier == param ^. fpGuid
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
      nextParam ^. fpId

convertPi :: (MonadA m, Typeable1 m) => Expression.Lambda (ExprMM m) -> Convertor m
convertPi lambda exprI = do
  (param, sBody) <- convertLambda lambda exprI
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

isPolymorphicFunc :: ExprMM m -> Bool
isPolymorphicFunc funcI =
  maybe False
  (ExprUtil.isDependentPi . Infer.iType . iwcInferred) $
  resultInferred funcI

plStored ::
  Lens'
  (Expression.Expression def (SugarInfer.Payload t i stored))
  stored
plStored = Expression.ePayload . SugarInfer.plStored

plIRef ::
  Lens.Traversal'
  (Expression.Expression def (SugarInfer.Payload t i (Maybe (Stored m))))
  (DataIRef.ExpressionI (Tag m))
plIRef = plStored . traverse . Property.pVal

exprStoredGuid ::
  Lens.Fold
  (Expression.Expression def (SugarInfer.Payload t i (Maybe (Stored m)))) Guid
exprStoredGuid = plIRef . Lens.to DataIRef.exprGuid

subExpressionGuids ::
  Lens.Fold
  (Expression.Expression def (SugarInfer.Payload t i (Maybe (Stored m)))) Guid
subExpressionGuids = Lens.folding ExprUtil.subExpressions . exprStoredGuid

infixr 9 `orElse`
orElse :: Monad m => m (Maybe a) -> m a -> m a
orElse a b = maybe b return =<< a

convertApply ::
  (Typeable1 m, MonadA m) =>
  Expression.Apply (ExprMM m) ->
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
  ExprMM m -> ExpressionU m -> Convertor m
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
  Expression.Apply (ExprMM m) ->
  Anchors.SpecialFunctions (Tag m) ->
  ExprMM m ->
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
  Expression.Apply (ExprMM m) ->
  ExpressionU m ->
  Anchors.SpecialFunctions (Tag m) ->
  ExprMM m ->
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

eApply :: Lens.Traversal' (Expression.Expression def a) (Expression.Apply (Expression.Expression def a))
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
  ExpressionU m -> ExprMM m -> ExpressionU m ->
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
    isSameVar = on (==) (^. gvIdentifier)
    right =
      case argRef ^. rExpressionBody of
      ExpressionSection _ (Section (Just _) rightOp (Just _))
        | on isSameOp (Lens.view rExpressionBody) op rightOp -> argRef
      _ -> addApplyChildParens argRef
applyOnSection _ funcRef funcI argRef exprI = convertApplyPrefix funcRef funcI argRef exprI

convertApplyPrefix ::
  (MonadA m, Typeable1 m) =>
  ExpressionU m -> ExprMM m -> ExpressionU m ->
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
  ExprMM m ->
  Guid -> GetVar NameHint m -> ExpressionU m -> SugarM m (ExpressionU m)
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
      { _gvName = Nothing
      , _gvIdentifier = DataIRef.variableRefGuid varRef
      , _gvJumpTo =
          case varRef of
          Expression.ParameterRef guid -> pure guid
          Expression.DefinitionRef defI ->
            IRef.guid defI <$ DataOps.newPane (sugarContext ^. SugarM.scCodeAnchors) defI
      , _gvVarType =
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

seedExpression ::
  HoleResultSeed m -> DataIRef.ExpressionM m (Maybe (StorePoint (Tag m)))
seedExpression (ResultSeedExpression expr) = expr
seedExpression (ResultSeedNewTag _) =
  Nothing <$ ExprUtil._PureTagExpr # Guid.fromString "FakeNewTag"

convertHoleResult ::
  (MonadA m, Typeable1 m) => SugarM.Context m -> Random.StdGen ->
  DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m))) -> T m (ExpressionU m)
convertHoleResult sugarContext gen res =
  SugarM.runPure
  (sugarContext ^. SugarM.scCodeAnchors)
  (sugarContext ^. SugarM.scRecordParams) .
  convertExpressionI .
  (traverse . SugarInfer.plInferred %~ Just) .
  (traverse . SugarInfer.plStored .~ Nothing) $
  SugarInfer.resultFromInferred gen res

makeHoleResult ::
  (Typeable1 m, MonadA m) => SugarM.Context m ->
  Infer.Inferred (DefI (Tag m)) ->
  Expression.Expression (DefI (Tag m))
  (PayloadM m inferred (Stored m)) ->
  HoleResultSeed m -> CT m (Maybe (HoleResult NameHint m))
makeHoleResult sugarContext inferred exprI seed =
  fmap (mkHoleResult <$>) . lift .
  traverse addConverted =<< inferResult (seedExpression seed)
  where
    addConverted inferredResult = do
      converted <-
        convertHoleResult sugarContext (gen inferredResult) $
        fst <$> inferredResult
      pure (insertFakeName converted, inferredResult)
    -- TODO: Cleanup:
    insertFakeName =
      case seed of
      ResultSeedExpression _ -> id
      ResultSeedNewTag name -> rExpressionBody . _ExpressionTag . tagName .~ Just name
    inferResult expr = do
      loaded <- lift $ SugarInfer.load Nothing expr
      let point = Infer.iPoint inferred
      memoBy (loaded, token, point, 'r') . return $
        SugarInfer.inferMaybe loaded (sugarContext ^. SugarM.scHoleInferState) point
    gen res =
      Random.mkStdGen $
      hashWithSalt 0 (show (void res), show guid)
    guid = resultGuid exprI
    token = (guid, sugarContext ^. SugarM.scMContextHash)
    mkHoleResult (converted, inferredExpr) =
      HoleResult
      { _holeResultInferred = fst <$> inferredExpr
      , _holeResultConverted = converted
      , _holeResultPick = pick
      , _holeResultPickPrefix = void pick
      }
      where
        pick = do
          finalExpr <-
            case seed of
            ResultSeedExpression _ -> pure inferredExpr
            ResultSeedNewTag name -> do
              newTagGuid <- DataOps.makeNewTag $ sugarContext ^. SugarM.scCodeAnchors
              mInferredExpr <- Cache.unmemoS . inferResult . (Nothing <$) $ ExprUtil._PureTagExpr # newTagGuid
              case mInferredExpr of
                Nothing ->
                  fail $ "Arbitrary fake tag successfully inferred as hole result, " ++
                  "but real new tag failed!"
                Just finalExpr ->
                  finalExpr <$ Transaction.setP (Anchors.assocNameRef newTagGuid) name
          pickResult exprI $
            ExprUtil.randomizeParamIds (gen finalExpr)
            finalExpr

memoBy ::
  (Cache.Key k, Binary v, MonadA m) =>
  k -> m v -> StateT Cache m v
memoBy k act = Cache.memoS (const act) k

getGlobal :: DefI (Tag m) -> (ScopeItem NameHint m, DataIRef.ExpressionM m ())
getGlobal defI =
  ( ScopeVar GetVar
    { _gvIdentifier = IRef.guid defI
    , _gvName = Nothing
    , _gvJumpTo = errorJumpTo
    , _gvVarType = GetDefinition
    }
  , ExprUtil.pureExpression $ ExprUtil.bodyDefinitionRef # defI
  )
  where
    errorJumpTo = error "Jump to on scope item??"

getField :: Guid -> (ScopeItem NameHint m, DataIRef.ExpressionM m ())
getField guid =
  ( ScopeTag TagG
    { _tagGuid = guid
    , _tagName = Nothing
    }
  , ExprUtil.pureExpression . Expression.BodyLeaf $
    Expression.Tag guid
  )

onScopeElement ::
  Monad m => (Guid, Expression.Expression def a) ->
  [(ScopeItem NameHint m, Expression.Expression def ())]
onScopeElement (param, typeExpr) =
  ( ScopeVar GetVar
    { _gvIdentifier = param
    , _gvName = Nothing
    , _gvJumpTo = errorJumpTo
    , _gvVarType = GetParameter
    }
  , getParam
  ) :
  map onScopeField
  (typeExpr ^..
   Expression.eBody . Expression._BodyRecord .
   Expression.recordFields . traverse . Lens._1 . ExprUtil.exprBodyTag)
  where
    errorJumpTo = error "Jump to on scope item??"
    exprTag = ExprUtil.pureExpression . Expression.BodyLeaf . Expression.Tag
    getParam = ExprUtil.pureExpression $ ExprUtil.bodyParameterRef # param
    onScopeField tGuid =
      ( ScopeVar GetVar
        { _gvIdentifier = tGuid
        , _gvName = Nothing
        , _gvJumpTo = errorJumpTo
        , _gvVarType = GetParameter
        }
      , ExprUtil.pureExpression . Expression.BodyGetField $
        Expression.GetField getParam (exprTag tGuid)
      )

convertTypeCheckedHoleH ::
  (MonadA m, Typeable1 m) => SugarM.Context m -> Maybe (T m Guid) ->
  InferredWC (Tag m) -> Convertor m
convertTypeCheckedHoleH sugarContext mPaste iwc exprI =
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
    mkHole =
      fmap Hole . traverse mkWritableHoleActions $
      traverse (SugarInfer.ntraversePayload pure id) exprI
    mkWritableHoleActions exprS = do
      globals <-
        SugarM.liftTransaction . Transaction.getP . Anchors.globals $
        sugarContext ^. SugarM.scCodeAnchors
      fields <-
        SugarM.liftTransaction . Transaction.getP . Anchors.fields $
        sugarContext ^. SugarM.scCodeAnchors
      pure HoleActions
        { _holePaste = mPaste
        , _holeMDelete = Nothing
        , _holeScope = pure $ concat
          [ (concatMap onScopeElement . Map.toList . Infer.iScope) inferred
          , map getGlobal globals
          , map getField fields
          ]
        , _holeInferExprType = inferExprType
        , _holeResult = makeHoleResult sugarContext inferred exprS
        }
    inferredHole x = do
      hole <- mkHole
      mkExpression exprI .
        ExpressionInferred . (`Inferred` hole) =<<
        (convertExpressionI . fmap toPayloadMM .
         SugarInfer.resultFromPure (mkGen 2 3 eGuid)) x
    plainHole =
      mkExpression exprI . ExpressionHole =<< mkHole

chooseHoleType ::
  [DataIRef.ExpressionM m f] -> hole -> (DataIRef.ExpressionM m f -> hole) -> hole
chooseHoleType inferredVals plain inferred =
  case inferredVals of
  [Expression.Expression { Expression._eBody = Expression.BodyLeaf Expression.Hole }] -> plain
  [inferredVal] -> inferred inferredVal
  _ -> plain

pickResult ::
  MonadA m =>
  DataIRef.ExpressionM m (PayloadM m i (Stored m)) ->
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
      guard $ lamKind == Type
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
  mkExpression exprI . ExpressionTag $ TagG tag Nothing

convertAtom :: (MonadA m, Typeable1 m) => String -> Convertor m
convertAtom str exprI =
  mkExpression exprI $ ExpressionAtom str

sideChannel ::
  Monad m =>
  Lens' s a ->
  LensLike m s (side, s) a (side, a)
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
  ( ExprMM m
  , ExprMM m
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
  ExprMM m ->
  Maybe (DataIRef.ExpressionIM m)
resultMIRef = fmap Property.value . resultStored

convertRecord ::
  (Typeable1 m, MonadA m) =>
  Expression.Record (ExprMM m) ->
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
  Expression.GetField (ExprMM m) ->
  ExprMM m ->
  SugarM m (ExpressionU m)
convertGetField (Expression.GetField recExpr tagExpr) exprI = do
  recordParams <- (^. SugarM.scRecordParams) <$> SugarM.readContext
  let
    mVar = do
      tag <- tagExpr ^? ExprUtil.exprBodyTag
      paramInfo <- Map.lookup tag recordParams
      param <- recExpr ^? Expression.eBody . ExprUtil.bodyParameterRef
      guard $ param == SugarM.piFromParameters paramInfo
      return
        GetVar
        { _gvName = Nothing
        , _gvIdentifier = tag
        , _gvJumpTo = pure $ SugarM.piJumpTo paramInfo
        , _gvVarType = GetParameter
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
  ExprMM m -> SugarM m (ExpressionU m)
convertExpressionI ee =
  ($ ee) $
  case ee ^. Expression.eBody of
  Expression.BodyLam x@(Expression.Lambda Val _ _ _) -> convertFunc x
  Expression.BodyLam x@(Expression.Lambda Type _ _ _) -> convertPi x
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

data RecordParams m = RecordParams
  { rpTags :: [Guid]
  , rpParamInfos :: Map Guid SugarM.ParamInfo
  , rpParams :: [FuncParam NameHint m (ExpressionU m)]
  }

data FieldParam m = FieldParam
  { fpTagGuid :: Guid
  , fpTagExpr :: ExprMM m
  , fpFieldType :: ExprMM m
  }

emptyRecordParams :: RecordParams m
emptyRecordParams = RecordParams
  { rpTags = []
  , rpParamInfos = Map.empty
  , rpParams = []
  }

mkRecordParams ::
  (MonadA m, Typeable1 m) => Guid -> [FieldParam m] ->
  Maybe (Stored m) -> Maybe (DataIRef.ExpressionIM m) ->
  Maybe (DataIRef.ExpressionM m (Stored m)) ->
  SugarM m (RecordParams m)
mkRecordParams paramGuid fieldParams mLambdaP mParamTypeI mBodyStored = do
  params <- traverse mkParam fieldParams
  pure RecordParams
    { rpTags = fpTagGuid <$> fieldParams
    , rpParamInfos = mconcat $ mkParamInfo <$> fieldParams
    , rpParams = params
    }
  where
    mkParamInfo fp =
      Map.singleton (fpTagGuid fp) . SugarM.ParamInfo paramGuid $
      fpTagExpr fp ^. plGuid
    mkParam fp = do
      typeS <- convertExpressionI $ fpFieldType fp
      let tagExprGuid = fpTagExpr fp ^. plGuid
      pure FuncParam
        { _fpGuid = fpTagGuid fp
        , _fpId = tagExprGuid
        , _fpName = Nothing
        , _fpHiddenLambdaGuid = Nothing --TODO: First param to take lambda's guid?
        , _fpType = removeSuccessfulType typeS
        , _fpMActions =
          fpActions tagExprGuid
          <$> mLambdaP <*> mParamTypeI <*> mBodyStored
        }
    fpActions tagExprGuid lambdaP paramTypeI bodyStored =
      FuncParamActions
      { _fpListItemActions = ListItemActions
        { _itemAddNext = addFieldParamAfter tagExprGuid paramTypeI
        , _itemDelete =
          delFieldParam tagExprGuid paramTypeI paramGuid lambdaP bodyStored
        }
      , _fpGetExample = fail "TODO: Examples"
      }

plGuid :: Lens' (Expression.Expression def (SugarInfer.Payload t i s)) Guid
plGuid = Expression.ePayload . SugarInfer.plGuid

type ExprField m = (DataIRef.ExpressionIM m, DataIRef.ExpressionIM m)
rereadFieldParamTypes ::
  MonadA m =>
  Guid -> DataIRef.ExpressionIM m ->
  ([ExprField m] -> ExprField m -> [ExprField m] -> T m Guid) ->
  T m Guid
rereadFieldParamTypes tagExprGuid paramTypeI f = do
  paramType <- DataIRef.readExprBody paramTypeI
  case paramType of
    Expression.BodyRecord (Expression.Record Type fields)
      | (prevFields, theField : nextFields) <-
        break ((tagExprGuid ==) . DataIRef.exprGuid . fst) fields ->
          f prevFields theField nextFields
    _ -> return tagExprGuid

rewriteFieldParamTypes ::
  MonadA m => DataIRef.ExpressionIM m -> [ExprField m] -> T m ()
rewriteFieldParamTypes paramTypeI fields =
  DataIRef.writeExprBody paramTypeI . Expression.BodyRecord $
  Expression.Record Type fields

addFieldParamAfter :: MonadA m => Guid -> DataIRef.ExpressionIM m -> T m Guid
addFieldParamAfter tagExprGuid paramTypeI =
  rereadFieldParamTypes tagExprGuid paramTypeI $
  \prevFields theField nextFields -> do
    fieldGuid <- Transaction.newKey
    tagExprI <- DataIRef.newExprBody $ ExprUtil.bodyTag # fieldGuid
    holeTypeI <- DataOps.newHole
    rewriteFieldParamTypes paramTypeI $
      prevFields ++ theField : (tagExprI, holeTypeI) : nextFields
    pure $ DataIRef.exprGuid tagExprI

delFieldParam ::
  MonadA m => Guid -> DataIRef.ExpressionIM m -> Guid ->
  Stored m -> DataIRef.ExpressionM m (Stored m) -> T m Guid
delFieldParam tagExprGuid paramTypeI paramGuid lambdaP bodyStored =
  rereadFieldParamTypes tagExprGuid paramTypeI $
  \prevFields (tagExprI, _) nextFields -> do
    tagExpr <- DataIRef.readExprBody tagExprI
    case tagExpr ^? ExprUtil.bodyTag of
      Just tagG -> deleteFieldParamRef paramGuid tagG bodyStored
      Nothing -> return ()
    case prevFields ++ nextFields of
      [] -> delLambda
      newFields ->
        dest prevFields nextFields <$
        rewriteFieldParamTypes paramTypeI newFields
  where
    bodyI = bodyStored ^. Expression.ePayload . Property.pVal
    dest prevFields nextFields =
      DataIRef.exprGuid . head $
      map fst (nextFields ++ reverse prevFields) ++
      [bodyI]
    delLambda =
      DataIRef.exprGuid bodyI <$
      Property.set lambdaP bodyI

convertDefinitionParams ::
  (MonadA m, Typeable1 m) =>
  [Guid] ->
  ExprMM m ->
  SugarM m
  ( [FuncParam NameHint m (ExpressionU m)]
  , RecordParams m
  , ExprMM m
  )
convertDefinitionParams usedTags expr =
  case expr ^. Expression.eBody of
  Expression.BodyLam lambda@(Expression.Lambda Val paramGuid paramType body)
    | isPolymorphicFunc expr -> do
      -- Dependent:
      fp <- convertFuncParam lambda expr
      (depParams, recordParams, deepBody) <- convertDefinitionParams usedTags body
      return (fp : depParams, recordParams, deepBody)
    | otherwise ->
      -- Independent:
      case paramType ^. Expression.eBody of
      Expression.BodyRecord (Expression.Record Type fields)
        | (not . null) fields
        , Just fieldParams <- traverse makeFieldParam fields
        , all ((`notElem` usedTags) . fpTagGuid) fieldParams -> do
          recordParams <-
            mkRecordParams paramGuid fieldParams
            (expr ^. plStored) (paramType ^? plIRef)
            (traverse (^. SugarInfer.plStored) body)
          return ([], recordParams, body)
      _ -> notConventionalParams
  _ -> notConventionalParams
  where
    makeFieldParam (tagExpr, typeExpr) = do
      fieldTagGuid <- tagExpr ^? ExprUtil.exprBodyTag
      pure FieldParam
        { fpTagGuid = fieldTagGuid
        , fpTagExpr = tagExpr
        , fpFieldType = typeExpr
        }
    notConventionalParams = return ([], emptyRecordParams, expr)

mExtractWhere ::
  Expression.Expression def a ->
  Maybe (Expression.Apply (Expression.Expression def a), Expression.Lambda (Expression.Expression def a))
mExtractWhere expr = do
  apply <- expr ^? Expression.eBody . Expression._BodyApply
  lambda <- apply ^? Expression.applyFunc . Expression.eBody . Expression._BodyLam
  guard $ (lambda ^. Expression.lambdaKind) == Val
  -- paramType has to be Hole for this to be sugarred to Where
  lambda ^? Expression.lambdaParamType . Expression.eBody . ExprUtil.bodyHole
  return (apply, lambda)

convertWhereItems ::
  (MonadA m, Typeable1 m) =>
  [Guid] ->
  ExprMM m ->
  SugarM m ([WhereItem NameHint m], ExprMM m)
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
        , wiName = Nothing
        }
    (nextItems, whereBody) <- convertWhereItems usedTags $ lambda ^. Expression.lambdaResult
    return (item : nextItems, whereBody)

newField ::
  MonadA m => T m (DataIRef.ExpressionIM m, DataIRef.ExpressionIM m)
newField = do
  tag <- Transaction.newKey
  newTagI <- DataIRef.newExprBody (ExprUtil.bodyTag # tag)
  holeI <- DataOps.newHole
  return (newTagI, holeI)

makeFirstRecordParam :: MonadA m => Stored m -> T m Guid
makeFirstRecordParam prop = do
  field <- newField
  recordTypeI <-
    DataIRef.newExprBody . Expression.BodyRecord $
    Expression.Record Type [field]
  (_, newLambdaI) <-
    DataIRef.newLambda recordTypeI $ Property.value prop
  Property.set prop newLambdaI
  return . DataIRef.exprGuid $ fst field

addFirstFieldParam :: MonadA m => DataIRef.ExpressionIM m -> T m Guid
addFirstFieldParam recordI = do
  recordBody <- DataIRef.readExprBody recordI
  DataIRef.exprGuid <$>
    case recordBody of
    Expression.BodyRecord Expression.Record
      { Expression._recordKind = Type
      , Expression._recordFields = fields
      } -> do
        field <- newField
        DataIRef.writeExprBody recordI $
          Expression.BodyRecord Expression.Record
          { Expression._recordKind = Type
          , Expression._recordFields = field : fields
          }
        pure $ fst field
    _ -> pure recordI

addStoredParam :: MonadA m => ExprMM m -> T m Guid
addStoredParam
  Expression.Expression
  { Expression._eBody = body
  , Expression._ePayload = SugarInfer.Payload { SugarInfer._plStored = Just prop }
  } =
    case body of
    Expression.BodyLam Expression.Lambda
      { Expression._lambdaKind = Val
      , Expression._lambdaParamType =
        Expression.Expression
        { Expression._eBody =
          Expression.BodyRecord Expression.Record
          { Expression._recordKind = Type
          , Expression._recordFields = fields
          }
        , Expression._ePayload =
          SugarInfer.Payload { SugarInfer._plStored = Just recordProp }
        }
      } | (not . null) fields ->
        addFirstFieldParam $ Property.value recordProp
    _ -> makeFirstRecordParam prop
addStoredParam
  (Expression.Expression
   (Expression.BodyLam
    (Expression.Lambda Val _ _ body)) _) = addStoredParam body
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
  ExprMM m ->
  SugarM m (DefinitionContent NameHint m)
convertDefinitionContent usedTags expr = do
  (depParams, recordParams, funcBody) <- convertDefinitionParams usedTags expr
  SugarM.local (SugarM.scRecordParams <>~ rpParamInfos recordParams) $ do
    (whereItems, whereBody) <-
      convertWhereItems (usedTags ++ rpTags recordParams) funcBody
    bodyS <- convertExpressionI whereBody
    return DefinitionContent
      { dDepParams = depParams
      , dParams = rpParams recordParams
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
  lift . AddNames.addToDef =<< convertDefI =<< lift (Load.loadDefinitionClosure defI)
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
        , _drName = Nothing
        , _drBody = bodyS
        , _drType = typeS
        }

convertDefIBuiltin ::
  MonadA m => Definition.Builtin -> DefI (Tag m) ->
  DataIRef.ExpressionM m (Stored m) -> DefinitionBody NameHint m
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
  gen -> T m (Maybe (DefinitionNewType NameHint m))
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
  CT m (DefinitionBody NameHint m)
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
      ilrExpr & traverse . SugarInfer.plInferred %~ Just
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
