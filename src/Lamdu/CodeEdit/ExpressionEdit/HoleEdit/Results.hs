{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Results
  ( makeAll, pick, HaveHiddenResults(..)
  , Result(..), ResultsList(..)
  , prefixId, MakeWidgets(..)
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens.Operators
import Control.Monad ((<=<), void, join)
import Control.Monad.ListT (ListT)
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Cache (Cache)
import Data.Function (on)
import Data.List (isInfixOf, isPrefixOf)
import Data.List.Utils (sortOn, nonEmptyAll)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import Lamdu.Data.Expression (Expression(..))
import Lamdu.Data.Expression.IRef (DefI)
import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List.Class as List
import qualified Data.Store.Guid as Guid
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Info as HoleInfo
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.WidgetIds as WidgetIds

type T = Transaction
type CT m = StateT Cache (T m)

data Group def = Group
  { groupNames :: [String]
  , groupBaseExpr :: Expression def ()
  }
type GroupM m = Group (DefI (Tag m))

mkGroup :: [String] -> Expression.BodyExpr def () -> Group def
mkGroup names body = Group
  { groupNames = names
  , groupBaseExpr = ExprUtil.pureExpression body
  }

pick ::
  MonadA m =>
  HoleInfo m -> Sugar.HoleResult m -> T m (Maybe Guid)
pick holeInfo holeResult = do
  Property.set (hiState holeInfo) HoleInfo.emptyState
  holeResult ^. Sugar.holeResultPick

data Result m = Result
  { rHoleResult :: Sugar.HoleResult m
  , rMkWidget :: ExprGuiM m (WidgetT m)
  , rId :: Widget.Id
  }

data ResultsList m = ResultsList
  { rlExtraResultsPrefixId :: Widget.Id
  , rlMain :: Result m
  , rlExtra :: [Result m]
  }

makeVariableGroup ::
  MonadA m => Expression.VariableRef (DefI (Tag m)) ->
  ExprGuiM m (GroupM m)
makeVariableGroup varRef =
  ExprGuiM.withNameFromVarRef varRef $ \(_, varName) ->
  return . mkGroup [varName] . Expression.BodyLeaf $ Expression.GetVariable varRef

makeTagGroup ::
  MonadA m => Guid ->
  ExprGuiM m (GroupM m)
makeTagGroup tag = do
  (_, name) <- ExprGuiM.transaction $ ExprGuiM.getGuidName tag
  return . mkGroup [name] . Expression.BodyLeaf $ Expression.Tag tag

makeLiteralGroup :: String -> [Group def]
makeLiteralGroup searchTerm =
  [ makeLiteralIntResult (read searchTerm)
  | nonEmptyAll Char.isDigit searchTerm
  ]
  where
    makeLiteralIntResult integer =
      mkGroup [show integer] $ ExprUtil.bodyLiteralInteger # integer

resultComplexityScore :: DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m))) -> Int
resultComplexityScore =
  sum . map (subtract 2 . length . Foldable.toList . Infer.iType) .
  Foldable.toList

prefixId :: HoleInfo m -> Widget.Id
prefixId holeInfo = mconcat [hiHoleId holeInfo, Widget.Id ["results"]]

type WidgetMaker m = Widget.Id -> Sugar.HoleResult m -> ExprGuiM m (WidgetT m)
data MakeWidgets m = MakeWidgets
  { mkResultWidget :: WidgetMaker m
  , mkNewTagResultWidget :: WidgetMaker m
  }

toMResultsList ::
  MonadA m => HoleInfo m -> WidgetMaker m ->
  Widget.Id ->
  [T m (DataIRef.ExpressionM m (Maybe (Sugar.StorePoint (Tag m))))] ->
  CT m (Maybe (ResultsList m))
toMResultsList holeInfo makeWidget baseId options = do
  results <-
    sortOn (resultComplexityScore . (^. Sugar.holeResultInferred)) .
    catMaybes <$>
    traverse (hiHoleActions holeInfo ^. Sugar.holeResult) options
  case results of
    [] -> return Nothing
    x : xs -> Just <$> do
      return ResultsList
        { rlExtraResultsPrefixId = extraResultsPrefixId
        , rlMain = mkResult (mconcat [prefixId holeInfo, baseId]) x
        , rlExtra = map (\res -> mkResult (extraResultId res) res) xs
        }
  where
    extraResultId =
      mappend extraResultsPrefixId . WidgetIds.hash . void .
      (^. Sugar.holeResultInferred)
    extraResultsPrefixId = mconcat [prefixId holeInfo, Widget.Id ["extra results"], baseId]
    mkResult resultId holeResult =
      Result
      { rHoleResult = holeResult
      , rMkWidget = makeWidget resultId holeResult
      , rId = resultId
      }

baseExprToResultsList ::
  MonadA m => HoleInfo m -> WidgetMaker m -> DataIRef.ExpressionM m () ->
  CT m (Maybe (ResultsList m))
baseExprToResultsList holeInfo makeWidget baseExpr =
  fmap join . traverse conclude =<<
  (hiHoleActions holeInfo ^. Sugar.holeInferExprType) baseExpr
  where
    conclude baseExprType =
      toMResultsList holeInfo makeWidget baseId .
      map (return . (Nothing <$)) $
      ExprUtil.applyForms baseExprType baseExpr
    baseId = WidgetIds.hash baseExpr

applyOperatorResultsList ::
  MonadA m => HoleInfo m -> WidgetMaker m ->
  DataIRef.ExpressionM m (Maybe (Sugar.StorePoint (Tag m))) ->
  DataIRef.ExpressionM m () ->
  CT m (Maybe (ResultsList m))
applyOperatorResultsList holeInfo makeWidget argument baseExpr =
  toMResultsList holeInfo makeWidget baseId . map return =<<
  case (Nothing <$ baseExpr) ^. Expression.eBody of
  Expression.BodyLam (Expression.Lambda k paramGuid paramType result) ->
    pure $ map genExpr
    [ Expression.BodyLam (Expression.Lambda k paramGuid unwrappedArg result)
    , Expression.BodyLam (Expression.Lambda k paramGuid paramType unwrappedArg)
    ]
  Expression.BodyGetField (Expression.GetField recExpr fieldTag) ->
    pure $ map genExpr
    [ Expression.BodyGetField $ Expression.GetField unwrappedArg fieldTag
    , Expression.BodyGetField $ Expression.GetField recExpr unwrappedArg
    ]
  _ -> do
    mBaseExprType <- hiHoleActions holeInfo ^. Sugar.holeInferExprType $ baseExpr
    pure $
      case mBaseExprType of
      Nothing -> []
      Just baseExprType ->
        let
          base = Nothing <$ ExprUtil.applyDependentPis baseExprType baseExpr
          applyBase = genExpr . ExprUtil.makeApply base
          fullyApplied x = genExpr . ExprUtil.makeApply (applyBase x)
        in
          [ fullyApplied unwrappedArg hole
          , fullyApplied hole unwrappedArg
          , applyBase unwrappedArg
          ]
  where
    genExpr = (`Expression` Nothing)
    hole = genExpr $ Expression.BodyLeaf Expression.Hole
    unwrappedArg = fromMaybe argument $ removeHoleWrap argument
    baseId = WidgetIds.hash baseExpr

removeHoleWrap :: Expression def a -> Maybe (Expression def a)
removeHoleWrap expr = do
  apply <- expr ^? Expression.eBody . Expression._BodyApply
  void $
    apply ^?
    Expression.applyFunc . Expression.eBody .
    Expression._BodyLeaf . Expression._Hole
  pure $ apply ^. Expression.applyArg

data ResultType = GoodResult | BadResult

makeResultsList ::
  MonadA m => HoleInfo m -> WidgetMaker m -> GroupM m ->
  CT m (Maybe (ResultType, ResultsList m))
makeResultsList holeInfo makeWidget group =
  case Property.value (hiState holeInfo) ^. HoleInfo.hsArgument of
  Just arg ->
    fmap ((,) GoodResult) <$> applyOperatorResultsList holeInfo makeWidget arg baseExpr
  Nothing -> do
    -- We always want the first (main), and we want to know if there's
    -- more (extra), so take 2:
    mRes <- toResList baseExpr
    case mRes of
      Just res -> return . Just $ (GoodResult, res)
      Nothing ->
        (fmap . fmap) ((,) BadResult) . toResList $
        holeApply baseExpr
  where
    toResList = baseExprToResultsList holeInfo makeWidget
    baseExpr = groupBaseExpr group
    holeApply =
      ExprUtil.pureExpression .
      (ExprUtil.makeApply . ExprUtil.pureExpression . Expression.BodyLeaf) Expression.Hole

makeNewTagResultList ::
  MonadA m => HoleInfo m -> WidgetMaker m ->
  Anchors.CodeProps m ->
  ListT (CT m) (Maybe (ResultType, ResultsList m))
makeNewTagResultList holeInfo makeNewTagResultWidget cp
  | null searchTerm = mempty
  | otherwise = do
      List.joinM $ List.fromList
        [fmap ((,) GoodResult) <$>
         toMResultsList holeInfo makeNewTagResultWidget (Widget.Id ["NewTag"]) [makeNewTag]]
  where
    searchTerm = (Property.value . hiState) holeInfo ^. HoleInfo.hsSearchTerm
    makeNewTag = do
      tag <- DataOps.makeNewTag cp
      Transaction.setP (Anchors.assocNameRef tag) searchTerm
      return . (`Expression.Expression` Nothing) . Expression.BodyLeaf $
        Expression.Tag tag

data HaveHiddenResults = HaveHiddenResults | NoHiddenResults

collectResults :: MonadA m => ListT m (ResultType, a) -> m ([a], HaveHiddenResults)
collectResults =
  conclude <=<
  List.splitWhenM (return . (>= Config.holeResultCount) . length . fst) .
  List.scanl step ([], [])
  where
    haveHiddenResults [] = NoHiddenResults
    haveHiddenResults _ = HaveHiddenResults
    conclude (notEnoughResults, enoughResultsM) =
      ( (Lens._2 %~ haveHiddenResults) . splitAt Config.holeResultCount
      . uncurry (on (++) reverse) . last . mappend notEnoughResults
      ) <$>
      List.toList (List.take 2 enoughResultsM)
    step results (tag, x) =
      results
      & case tag of
        GoodResult -> Lens._1
        BadResult -> Lens._2
        %~ (x :)

makeAll ::
  MonadA m => HoleInfo m -> MakeWidgets m ->
  ExprGuiM m ([ResultsList m], HaveHiddenResults)
makeAll holeInfo makeWidget = do
  cp <- ExprGuiM.readCodeAnchors
  resultList <-
    List.catMaybes .
    (`mappend` makeNewTagResultList holeInfo (mkNewTagResultWidget makeWidget) cp) .
    List.mapL (makeResultsList holeInfo (mkResultWidget makeWidget)) .
    List.fromList <$>
    makeAllGroups holeInfo
  ExprGuiM.liftMemoT $ collectResults resultList

makeAllGroups :: MonadA m => HoleInfo m -> ExprGuiM m [GroupM m]
makeAllGroups holeInfo = do
  paramGroups <-
    traverse (makeVariableGroup . Expression.ParameterRef) $
    hiHoleActions holeInfo ^. Sugar.holeScope
  globalGroups <-
    traverse (makeVariableGroup . Expression.DefinitionRef) =<<
    ExprGuiM.getCodeAnchor Anchors.globals
  tagGroups <- traverse makeTagGroup =<< ExprGuiM.getCodeAnchor Anchors.fields
  let
    literalGroups = makeLiteralGroup searchTerm
    getVarGroups = paramGroups ++ globalGroups
    relevantGroups = primitiveGroups ++ literalGroups ++ getVarGroups ++ tagGroups
  return $ holeMatches groupNames searchTerm relevantGroups
  where
    state = Property.value $ hiState holeInfo
    searchTerm = state ^. HoleInfo.hsSearchTerm
    primitiveGroups =
      [ mkGroup ["Set", "Type"] $ Expression.BodyLeaf Expression.Set
      , mkGroup ["Integer", "ℤ", "Z"] $ Expression.BodyLeaf Expression.IntegerType
      , mkGroup ["->", "Pi", "→", "→", "Π", "π"] $
        ExprUtil.makePi (Guid.fromString "NewPi") holeExpr holeExpr
      , mkGroup ["\\", "Lambda", "Λ", "λ"] $
        ExprUtil.makeLambda (Guid.fromString "NewLambda") holeExpr holeExpr
      , mkGroup ["Record Type", "{"] . Expression.BodyRecord $
        Expression.Record Expression.Type mempty
      , mkGroup ["Record Value", "{"] . Expression.BodyRecord $
        Expression.Record Expression.Val mempty
      , mkGroup [".", "Get Field"] . Expression.BodyGetField $
        Expression.GetField ExprUtil.pureHole ExprUtil.pureHole
      ]
    holeExpr = ExprUtil.pureExpression $ Expression.BodyLeaf Expression.Hole

groupOrdering :: String -> [String] -> [Bool]
groupOrdering searchTerm names =
  map not
  [ match (==)
  , match isPrefixOf
  , match insensitivePrefixOf
  , match isInfixOf
  ]
  where
    insensitivePrefixOf = isPrefixOf `on` map Char.toLower
    match f = any (f searchTerm) names

holeMatches :: (a -> [String]) -> String -> [a] -> [a]
holeMatches getNames searchTerm =
  sortOn (groupOrdering searchTerm . getNames) .
  filter nameMatch
  where
    nameMatch = any (insensitiveInfixOf searchTerm) . getNames
    insensitiveInfixOf = isInfixOf `on` map Char.toLower
