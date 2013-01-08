{-# LANGUAGE OverloadedStrings, ConstraintKinds, TypeFamilies #-}
module Lamdu.CodeEdit.Sugar
  ( Definition(..), DefinitionBody(..)
  , ListItemActions(..), itemAddNext, itemDelete
  , FuncParamActions(..), fpListItemActions, fpGetExample
  , DefinitionExpression(..), DefinitionContent(..), DefinitionNewType(..)
  , DefinitionBuiltin(..)
  , Actions(..), giveAsArg, callWithArg, callWithNextArg, replace, cut, giveAsArgToOperator
  , ExpressionBody(..)
  , Payload(..), plInferredTypes, plActions, plNextHole
  , ExpressionP(..), rGuid, rExpressionBody, rPayload, rHiddenGuids
  , Expression
  , WhereItem(..)
  , ListItem(..), ListActions(..), List(..)
  , Func(..)
  , FuncParam(..), fpGuid, fpHiddenLambdaGuid, fpType, fpMActions
  , Pi(..)
  , Section(..)
  , Hole(..), holeScope, holeMActions, holeInferResults
  , HoleActions(..), holePaste, holeMDelete
  , HoleResult(..)
    , holeResultInferred
    , holeResultConvert, holeResultMPickAndCallWithArg
    , holeResultPick, holeResultPickAndGiveAsArg
    , holeResultPickAndGiveAsArgToOperator
    , holeResultMPickAndCallWithNextArg
  , holeResultHasHoles
  , LiteralInteger(..)
  , Inferred(..)
  , Polymorphic(..)
  , HasParens(..)
  , loadConvertDefI
  , removeTypes
  ) where

import Control.Applicative ((<$>), Applicative(..), liftA2)
import Control.Lens (SimpleTraversal, (.~), (^.), (&), (%~), (.~), (^?), (^..), (<>~), (+~))
import Control.Monad ((<=<), join, mplus, void, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (runState)
import Control.Monad.Trans.Writer (runWriter)
import Control.MonadA (MonadA)
import Data.Function (on)
import Data.List.Utils (sortOn)
import Data.Maybe (listToMaybe, maybeToList, isJust)
import Data.Monoid (Any(..))
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
import qualified Control.Monad.Trans.Writer as Writer
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
import qualified Lamdu.Config as Config
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

guardReinferSuccess :: MonadA m => SugarM.Context m -> T m a -> T m (Maybe (T m a))
guardReinferSuccess sugarContext act =
  case SugarM.scMReinferRoot sugarContext of
  Nothing -> pure Nothing
  Just reinferRoot ->
    Transaction.forkScratch $ do
    _ <- act
    success <- reinferRoot
    pure $
      if success
      then Just act
      else Nothing

mkCallWithArg ::
  MonadA m => SugarM.Context m ->
  DataIRef.ExpressionM m (SugarInfer.Payload (Tag m) i (Stored m)) ->
  T m (Maybe (T m Guid))
mkCallWithArg sugarContext exprS =
  guardReinferSuccess sugarContext mkCall
  where
    mkCall = fmap DataIRef.exprGuid . DataOps.callWithArg $ resultStored exprS

mkActions ::
  MonadA m => SugarM.Context m ->
  DataIRef.ExpressionM m (SugarInfer.Payload (Tag m) i (Stored m)) -> Actions m
mkActions sugarContext exprS =
  Actions
  { _giveAsArg = DataIRef.exprGuid <$> DataOps.giveAsArg stored
  , _callWithArg = mkCallWithArg sugarContext exprS
  , _callWithNextArg = pure Nothing
  , _replace = doReplace DataOps.setToHole
  , _cut = mkCutter (SugarM.scCodeAnchors sugarContext) (Property.value stored) $ doReplace DataOps.replaceWithHole
  , _giveAsArgToOperator =
      fmap DataIRef.exprGuid $ DataOps.giveAsArgToOperator stored
  }
  where
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
  inferredTypesRefs <-
    zipWithM
    ( fmap (convertExpressionI . fmap toPayloadMM)
    . SugarInfer.resultFromPure
    ) seeds types
  return
    Expression
    { _rGuid = resultGuid exprI
    , _rExpressionBody = expr
    , _rPayload = Payload
      { _plInferredTypes = inferredTypesRefs
      , _plActions =
        mkActions sugarContext <$>
        traverse (SugarInfer.ntraversePayload pure id pure) exprI
      , _plNextHole = Nothing
      }
    , _rHiddenGuids = []
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
      Lens.filtered (Lens.anyOf paramRef (== param))
    paramRef = getVar . Expression.parameterRef

getVar :: SimpleTraversal (Expression.Expression def a) (Expression.VariableRef def)
getVar = Expression.eBody . Expression.bodyLeaf . Expression.getVariable

mkFuncParamActions ::
  MonadA m => Stored m ->
  Expression.Lambda (Expression.Expression def (Stored m)) ->
  FuncParamActions m
mkFuncParamActions lambdaProp (Expression.Lambda param _paramType body) =
  FuncParamActions
  { _fpListItemActions =
    ListItemActions
    { _itemDelete = do
        deleteParamRef param body
        replaceWith lambdaProp $ body ^. Expression.ePayload
    , _itemAddNext = fmap fst . DataOps.lambdaWrap $ body ^. Expression.ePayload
    }
  , _fpGetExample =
      return $
      Expression (Guid.augment "EXAMPLE" param)
      (ExpressionAtom "NotImplemented")
      (Payload [] Nothing Nothing)
      []
  }

data IsDependent = Dependent | NonDependent
  deriving (Eq, Ord, Show)

convertFuncParam ::
  (Typeable1 m, MonadA m) => Expression.Lambda (DataIRef.ExpressionM m (PayloadMM m)) ->
  DataIRef.ExpressionM m (PayloadMM m) ->
  SugarM m (IsDependent, FuncParam m (Expression m))
convertFuncParam lam@(Expression.Lambda paramGuid paramType _) expr = do
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
  (Typeable1 m, MonadA m) => Expression.Lambda (DataIRef.ExpressionM m (PayloadMM m)) ->
  DataIRef.ExpressionM m (PayloadMM m) ->
  SugarM m ((IsDependent, FuncParam m (Expression m)), Expression m)
convertLambda lam expr =
  liftA2 (,)
  (convertFuncParam lam expr) $
  convertExpressionI (lam ^. Expression.lambdaBody)

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
  | Lens.anyOf getDefinition (== Anchors.sfNil specialFunctions) funcI
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
    | Lens.anyOf (eApply . Expression.applyFunc . getDefinition)
      (== Anchors.sfCons specialFunctions) funcFuncI
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

eApply :: SimpleTraversal (Expression.Expression def a) (Expression.Apply (Expression.Expression def a))
eApply = Expression.eBody . Expression.bodyApply

getDefinition :: SimpleTraversal (Expression.Expression def a) def
getDefinition = getVar . Expression.definitionRef

removeInferredTypes :: Expression m -> Expression m
removeInferredTypes = Lens.set (rPayload . plInferredTypes) []

removeRedundantTypes :: Expression m -> Expression m
removeRedundantTypes =
  Lens.over (rPayload . plInferredTypes) removeIfNoErrors
  where
    removeIfNoErrors [_] = []
    removeIfNoErrors xs = xs

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
      Lens.filtered (Lens.notNullOf (rExpressionBody . expressionHole))

applyOnSection ::
  (MonadA m, Typeable1 m) => Section (Expression m) ->
  Expression.Apply (Expression m, DataIRef.ExpressionM m (PayloadMM m)) ->
  Convertor m
applyOnSection (Section Nothing op Nothing) (Expression.Apply (_, funcI) arg@(argRef, _)) exprI
  | isPolymorphicFunc funcI = do
    newOpRef <-
      convertApplyPrefix (Expression.Apply (op, funcI) arg) exprI
    mkExpression exprI . ExpressionSection DontHaveParens $
      Section Nothing (removeRedundantTypes newOpRef) Nothing
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
convertApplyPrefix (Expression.Apply (funcRef, funcI) (argRef, argI)) applyI = do
  sugarContext <- SugarM.readContext
  let
    newArgRef = addCallWithNextArg $ addParens argRef
    fromMaybeStored = traverse (SugarInfer.ntraversePayload pure id pure)
    onStored expr f = maybe id f $ fromMaybeStored expr
    addCallWithNextArg =
      onStored applyI $ \applyS ->
      ( rPayload . plActions . Lens.mapped . callWithNextArg .~
        mkCallWithArg sugarContext applyS
      ) .
      onStored argI (addPickAndCallWithNextArg sugarContext applyS)
    newFuncRef =
      setNextHole newArgRef .
      addApplyChildParens .
      removeRedundantTypes $
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
  getVarExpr <-
    mkExpression exprI $ ExpressionGetVariable varRef
  if isInfix
    then
      mkExpression exprI .
      ExpressionSection HaveParens $
      Section Nothing (removeInferredTypes getVarExpr) Nothing
    else return getVarExpr

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

countArrows :: Expression.Expression def () -> Int
countArrows Expression.Expression
  { Expression._eBody =
    Expression.BodyPi (Expression.Lambda _ _ resultType)
  } = 1 + countArrows resultType
countArrows _ = 0

-- TODO: Return a record, not a tuple
countPis :: Expression.Expression def () -> (Int, Int)
countPis e@Expression.Expression
  { Expression._eBody =
    Expression.BodyPi (Expression.Lambda _ _ resultType)
  }
  | ExprUtil.isDependentPi e = Lens._1 +~ 1 $ countPis resultType
  | otherwise = (0, 1 + countArrows resultType)
countPis _ = (0, 0)

applyForms :: Expression.Expression def () -> Expression.Expression def () -> [Expression.Expression def ()]
applyForms _ e@Expression.Expression{ Expression._eBody = Expression.BodyLambda {} } =
  [e]
applyForms exprType expr =
  reverse . take (1 + arrows) $ iterate addApply withDepPisApplied
  where
    withDepPisApplied = iterate addApply expr !! depPis
    (depPis, arrows) = countPis exprType
    addApply = ExprUtil.pureExpression . (`ExprUtil.makeApply` ExprUtil.pureHole)

-- Fill partial holes in an expression. Parital holes are those whose
-- inferred (filler) value itself is not complete, so will not be a
-- useful auto-inferred value. By auto-filling those, we allow the
-- user a chance to access all the partiality that needs filling more
-- easily.
fillPartialHolesInExpression ::
  MonadA m =>
  (Expression.Expression def () ->
   m (Maybe (Expression.Expression def (Infer.Inferred def)))) ->
  Expression.Expression def (Infer.Inferred def) ->
  m [Expression.Expression def (Infer.Inferred def)]
fillPartialHolesInExpression check oldExpr =
  fmap ((++ [oldExpr]) . maybeToList) .
  recheck . runWriter $ fillHoleExpr oldExpr
  where
    recheck (newExpr, Any True) = check newExpr
    recheck (_, Any False) = return Nothing
    fillHoleExpr expr@(Expression.Expression (Expression.BodyLeaf Expression.Hole) inferred) =
      let inferredVal = void $ Infer.iValue inferred
      in
        case inferredVal ^. Expression.eBody of
        Expression.BodyLeaf Expression.Hole -> return $ void expr
        _ | isCompleteType inferredVal -> return $ void expr
          | otherwise -> do
            -- Hole inferred value has holes to fill, no use leaving it as
            -- auto-inferred, just fill it:
            Writer.tell $ Any True
            return inferredVal
    fillHoleExpr (Expression.Expression body _) =
      fmap ExprUtil.pureExpression $ traverse fillHoleExpr body

resultComplexityScore :: DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m))) -> Int
resultComplexityScore =
  sum . map (subtract 2 . length . Foldable.toList . Infer.iType) .
  Foldable.toList

inferApplyForms ::
  MonadA m =>
  (DataIRef.ExpressionM m () -> T m [DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)))]) -> DataIRef.ExpressionM m () ->
  (Infer.InferNode (DefI (Tag m)), Infer.Context (DefI (Tag m))) ->
  T m [DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)))]
inferApplyForms processRes expr (node, inferContext) =
  fmap (sortOn resultComplexityScore) . makeApplyForms =<<
  SugarInfer.inferMaybe_ Nothing expr inferContext node
  where
    makeApplyForms Nothing = return []
    makeApplyForms (Just i) =
      fmap concat . traverse processRes $
      (applyForms . void) (Infer.iType (i ^. Expression.ePayload)) expr

mkHoleResult ::
  (MonadA m, Typeable1 m) => SugarM.Context m ->
  DataIRef.ExpressionM m (SugarInfer.Payload (Tag m) i (Stored m)) ->
  DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m))) -> HoleResult m
mkHoleResult sugarContext exprS res =
  HoleResult
  { _holeResultInferred = res
  , _holeResultConvert = convertHoleResult res
  , _holeResultPick = pick
  , _holeResultPickAndGiveAsArg =
      pick >> DataIRef.exprGuid <$> DataOps.giveAsArg stored
  , _holeResultPickAndGiveAsArgToOperator = pickAndGiveAsArgToOp
  , _holeResultMPickAndCallWithArg = guardReinferSuccess sugarContext pickAndCallWithArg
  , _holeResultMPickAndCallWithNextArg = pure Nothing
  }
  where
    cp = SugarM.scCodeAnchors sugarContext
    convertHoleResult =
      SugarM.runPure cp . convertExpressionI .
      (Lens.mapped . SugarInfer.plInferred %~ Just) .
      (Lens.mapped . SugarInfer.plStored .~ Nothing) .
      SugarInfer.resultFromInferred
    pickAndCallWithArg =
      pick >> DataIRef.exprGuid <$> DataOps.callWithArg stored
    pickAndGiveAsArgToOp =
      pick >> DataIRef.exprGuid <$> DataOps.giveAsArgToOperator stored
    pick = pickResult (resultGuid exprS) exprS res
    stored = resultStored exprS

-- This is called to add the pick-and-call-with-next-arg action on the
-- *ARG*(argS) which may be a hole (inside the applyS).
addPickAndCallWithNextArg ::
  MonadA m => SugarM.Context m ->
  DataIRef.ExpressionM m (SugarInfer.Payload (Tag m) i (Stored m)) ->
  DataIRef.ExpressionM m (SugarInfer.Payload (Tag m) i (Stored m)) ->
  Expression m -> Expression m
addPickAndCallWithNextArg sugarContext applyS argS =
  rExpressionBody . expressionHole . holeMActions . Lens.mapped .
  holeInferResults . Lens.mapped . Lens.mapped . Lens.mapped %~ onHoleResult
  where
    onHoleResult holeResult =
      holeResult
      & holeResultMPickAndCallWithNextArg .~
        newPickAndCallWithArg (holeResult ^. holeResultInferred)

    -- In this context we return the new actions of an apply's arg
    -- (which is a hole):
    newPickAndCallWithArg res =
      guardReinferSuccess sugarContext $ pickAndCallWithArg res
    pickAndCallWithArg res = do
      _ <- pickResult eGuid argS res
      DataIRef.exprGuid <$> DataOps.callWithArg (resultStored applyS)
    eGuid = resultGuid applyS

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
    check expr = SugarInfer.inferMaybe_ Nothing expr inferState $ Infer.iPoint inferred

    memoBy expr act = Cache.memoS (const act) (expr, eGuid, contextHash)

    inferResults processRes expr =
      memoBy expr . inferApplyForms processRes expr .
      (`runState` inferState) $ Infer.newNodeWithScope scope
    onScopeElement (param, _typeExpr) = param
    mkHole processRes =
      Hole $
      mkWritableHoleActions processRes <$>
      traverse (SugarInfer.ntraversePayload pure id pure) exprI
    mkWritableHoleActions processRes exprS =
      HoleActions
      { _holePaste = mPaste
      , _holeMDelete = Nothing
      , _holeScope = map onScopeElement . Map.toList $ Infer.iScope inferred
      , _holeInferResults =
        (fmap . map) (mkHoleResult sugarContext exprS) .
        inferResults processRes
      }
    filledHole =
      mkHole $ maybe (return []) (fillPartialHolesInExpression check) <=< check
    inferredHole =
      mkExpression exprI .
      ExpressionInferred . (`Inferred` filledHole) <=<
      convertExpressionI . fmap toPayloadMM .
      SugarInfer.resultFromPure (mkGen 2 3 eGuid)
    plainHole =
      wrapOperatorHole exprI <=<
      mkExpression exprI . ExpressionHole $ mkHole (fmap maybeToList . check)

wrapOperatorHole :: (MonadA m, Typeable1 m) => DataIRef.ExpressionM m (PayloadMM m) -> Expression m -> SugarM m (Expression m)
wrapOperatorHole exprI holeExpr = do
  -- TODO: It is ugly to even know about assocSearchTermRef in
  -- Sugar. Instead, some other metadata should be set to indicate it
  -- is a hole
  searchTerm <-
    SugarM.liftTransaction . Transaction.getP . Anchors.assocSearchTermRef $
    resultGuid exprI
  if isOperatorName searchTerm
    then
      -- TODO: Ok to mkExpression with same exprI here?
      mkExpression exprI . ExpressionSection DontHaveParens $
      Section Nothing (removeInferredTypes holeExpr) Nothing
    else return holeExpr

isOperatorName :: String -> Bool
isOperatorName name =
  not (null name) && all (`elem` Config.operatorChars) name

chooseHoleType ::
  [DataIRef.ExpressionM m f] -> hole -> (DataIRef.ExpressionM m f -> hole) -> hole
chooseHoleType inferredVals plain inferred =
  case inferredVals of
  [Expression.Expression { Expression._eBody = Expression.BodyLeaf Expression.Hole }] -> plain
  [inferredVal] -> inferred inferredVal
  _ -> plain

pickResult ::
  MonadA m => Guid ->
  DataIRef.ExpressionM m (SugarInfer.Payload (Tag m) i (Stored m)) ->
  DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m))) ->
  T m Guid
pickResult defaultDest exprS =
  fmap
  ( maybe defaultDest
    (DataIRef.exprGuid . Lens.view (Expression.ePayload . Lens._2))
  . listToMaybe . uninferredHoles . fmap swap
  )
  . (DataIRef.writeExpression . Property.value . resultStored) exprS

-- Also skip param types, those can usually be inferred later, so less
-- useful to fill immediately
uninferredHoles ::
  Expression.Expression def (Infer.Inferred def, a) ->
  [Expression.Expression def (Infer.Inferred def, a)]
uninferredHoles
  Expression.Expression { Expression._eBody = Expression.BodyApply (Expression.Apply func arg) }
  | (ExprUtil.isDependentPi . Infer.iType . Lens.view (Expression.ePayload . Lens._1)) func =
    uninferredHoles func
  | otherwise = uninferredHoles func ++ uninferredHoles arg
uninferredHoles e@Expression.Expression { Expression._eBody = Expression.BodyLeaf Expression.Hole } = [e]
uninferredHoles Expression.Expression
  { Expression._eBody = Expression.BodyPi (Expression.Lambda _ paramType resultType) } =
    uninferredHoles resultType ++ uninferredHoles paramType
uninferredHoles Expression.Expression
  { Expression._eBody = Expression.BodyLambda (Expression.Lambda _ paramType result) } =
    uninferredHoles result ++ uninferredHoles paramType
uninferredHoles Expression.Expression { Expression._eBody = body } =
  Foldable.concatMap uninferredHoles body

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
  , liSetValue = writeIRef <$> resultStored exprI
  }
  where
    writeIRef prop =
      DataIRef.writeExprBody (Property.value prop) .
      ExprUtil.makeLiteralInteger

convertAtom :: (MonadA m, Typeable1 m) => String -> Convertor m
convertAtom name exprI =
  mkExpression exprI $ ExpressionAtom name

convertExpressionI :: (Typeable1 m, MonadA m) => DataIRef.ExpressionM m (PayloadMM m) -> SugarM m (Expression m)
convertExpressionI ee =
  ($ ee) $
  case ee ^. Expression.eBody of
  Expression.BodyLambda x -> convertFunc x
  Expression.BodyPi x -> convertPi x
  Expression.BodyApply x -> convertApply x
  Expression.BodyLeaf (Expression.GetVariable x) -> convertGetVariable x
  Expression.BodyLeaf (Expression.LiteralInteger x) -> convertLiteralInteger x
  Expression.BodyLeaf Expression.Hole -> convertHole
  Expression.BodyLeaf Expression.Set -> convertAtom "Set"
  Expression.BodyLeaf Expression.IntegerType -> convertAtom "Int"

-- Check no holes
isCompleteType :: Expression.Expression def () -> Bool
isCompleteType =
  Lens.nullOf
  ( Lens.folding ExprUtil.subExpressions
  . Expression.eBody . Expression.bodyLeaf . Expression.hole
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
  Expression.BodyLambda lambda -> do
    (isDependent, fp) <- convertFuncParam lambda expr
    (depParams, params, deepBody) <-
      convertDefinitionParams $ lambda ^. Expression.lambdaBody
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
  { Expression._eBody = Expression.BodyLambda lambda@Expression.Lambda
  { Expression._lambdaParamId = param
  , Expression._lambdaParamType = Expression.Expression
  { Expression._eBody = Expression.BodyLeaf Expression.Hole
  }
  , Expression._lambdaBody = body
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
    Expression.BodyLambda
    Expression.Lambda { Expression._lambdaBody = body } } =
  addStoredParam body
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
    reinferRoot = do
      reloadedRoot <-
        DataIRef.readExpression . Load.irefOfClosure $
        exprLoaded ^. Expression.ePayload
      isJust <$> uncurry (SugarInfer.inferMaybe_ (Just defI) (void reloadedRoot))
        initialInferState

    iTypeGen = mkGen 0 3 $ IRef.guid defI
    inferLoadedGen = mkGen 1 3 $ IRef.guid defI

--------------

removeTypes :: MonadA m => Expression m -> Expression m
removeTypes = Lens.set (Lens.mapped . plInferredTypes) []
