{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Results
  ( makeAll, HaveHiddenResults(..)
  , Result(..)
  , ResultsList(..), rlExtraResultsPrefixId, rlMain, rlExtra
  , prefixId, MakeWidgets(..)
  , SugarExprPl
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens.Operators
import Control.Lens.Utils (contextSetter, contextVal)
import Control.Monad (void)
import Control.Monad.ListT (ListT)
import Control.Monad.Trans.Either.Utils (leftToJust, justToLeft)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Cache (Cache)
import Data.Function (on)
import Data.List (isInfixOf, isPrefixOf, partition)
import Data.List.Utils (sortOn, nonEmptyAll)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Maybe.Utils (maybeToMPlus)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Lamdu.Config (Config)
import Lamdu.Data.Expression (Expression(..))
import Lamdu.Data.Expression.IRef (DefIM)
import Lamdu.Data.Expression.Utils (ApplyFormAnnotation(..), pureHole)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), hiSearchTerm, hiMArgument)
import Lamdu.Sugar.Types (Scope(..))
import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List.Class as List
import qualified Data.Store.Guid as Guid
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction
type CT m = StateT Cache (T m)

data Group def = Group
  { _groupNames :: [String]
  , _groupBaseExpr :: Expression def ()
  }
type GroupM m = Group (DefIM m)

Lens.makeLenses ''Group

mkGroup :: [String] -> Expr.BodyExpr def () -> Group def
mkGroup names body = Group
  { _groupNames = names
  , _groupBaseExpr = ExprUtil.pureExpression body
  }

data ResultType = GoodResult | BadResult
  deriving (Eq, Ord)

type SugarExprPl = (ExprGuiM.StoredGuids, ExprGuiM.Injected)

data Result m = Result
  { rType :: ResultType
  , rHoleResult :: Sugar.HoleResult Sugar.Name m SugarExprPl
  , rMkWidget :: ExprGuiM m (WidgetT m)
  , rId :: Widget.Id
  }

data IsPreferred = Preferred | NotPreferred
  deriving (Eq, Ord)

data ResultsList m = ResultsList
  { _rlPreferred :: IsPreferred -- Move to top of result list
  , _rlExtraResultsPrefixId :: Widget.Id
  , _rlMain :: Result m
  , _rlExtra :: [Result m]
  }
Lens.makeLenses ''ResultsList

getVarsToGroup :: (Sugar.GetVar Sugar.Name m, Expression def ()) -> Group def
getVarsToGroup (getVar, expr) = sugarNameToGroup (getVar ^. Sugar.gvName) expr

tagsToGroup :: (Sugar.TagG Sugar.Name, Expression def ()) -> Group def
tagsToGroup (tagG, expr) = sugarNameToGroup (tagG ^. Sugar.tagName) expr

getParamsToGroup :: (Sugar.GetParams Sugar.Name m, Expression def ()) -> Group def
getParamsToGroup (getParams, expr) =
  sugarNameToGroup (getParams ^. Sugar.gpDefName) expr
  & groupNames <>~ ["params"]

sugarNameToGroup :: Sugar.Name -> Expression def () -> Group def
sugarNameToGroup (Sugar.Name _ collision varName) expr = Group
  { _groupNames = varName : collisionStrs
  , _groupBaseExpr = expr
  }
  where
    collisionStrs =
      case collision of
      Sugar.NoCollision -> []
      Sugar.Collision suffix -> [show suffix]

makeLiteralGroups :: String -> [Group def]
makeLiteralGroups searchTerm =
  [ makeLiteralIntResult (read searchTerm)
  | nonEmptyAll Char.isDigit searchTerm
  ]
  where
    makeLiteralIntResult integer =
      mkGroup [show integer] $ ExprLens.bodyLiteralInteger # integer

resultComplexityScore :: ExprIRef.ExpressionM m (Infer.Inferred (DefIM m)) -> Int
resultComplexityScore = length . Foldable.toList . Infer.iType . (^. Expr.ePayload)

prefixId :: HoleInfo m -> Widget.Id
prefixId holeInfo = mconcat [hiId holeInfo, Widget.Id ["results"]]

type
  WidgetMaker m =
    Widget.Id -> Sugar.HoleResult Sugar.Name m SugarExprPl ->
    ExprGuiM m (WidgetT m)
data MakeWidgets m = MakeWidgets
  { mkResultWidget :: WidgetMaker m
  , mkNewTagResultWidget :: WidgetMaker m
  }

typeCheckHoleResult ::
  MonadA m => HoleInfo m ->
  Sugar.HoleResultSeed m (Sugar.MStorePoint m SugarExprPl) ->
  CT m (Maybe (ResultType, Sugar.HoleResult Sugar.Name m SugarExprPl))
typeCheckHoleResult holeInfo seed = do
  mGood <- mkHoleResult seed
  case (mGood, seed) of
    (Just good, _) -> pure $ Just (GoodResult, good)
    (Nothing, Sugar.ResultSeedExpression expr) ->
      fmap ((,) BadResult) <$>
      (mkHoleResult . Sugar.ResultSeedExpression . storePointHoleWrap) expr
    _ -> pure Nothing
  where
    mkHoleResult = Sugar.holeResult (hiActions holeInfo)

typeCheckResults ::
  MonadA m => HoleInfo m ->
  [Sugar.HoleResultSeed m (Sugar.MStorePoint m SugarExprPl)] ->
  CT m [(ResultType, Sugar.HoleResult Sugar.Name m SugarExprPl)]
typeCheckResults holeInfo options = do
  rs <- catMaybes <$> traverse (typeCheckHoleResult holeInfo) options
  let (goodResults, badResults) = partition ((== GoodResult) . fst) rs
  return $ sortOn (score . snd) goodResults ++ badResults
  where
    score = resultComplexityScore . (^. Sugar.holeResultInferred)

mResultsListOf ::
  HoleInfo m -> WidgetMaker m -> Widget.Id ->
  [(ResultType, Sugar.HoleResult Sugar.Name m SugarExprPl)] ->
  Maybe (ResultsList m)
mResultsListOf _ _ _ [] = Nothing
mResultsListOf holeInfo makeWidget baseId (x:xs) = Just
  ResultsList
  { _rlPreferred = NotPreferred
  , _rlExtraResultsPrefixId = extraResultsPrefixId
  , _rlMain = mkResult (mconcat [prefixId holeInfo, baseId]) x
  , _rlExtra = map (\res -> mkResult (extraResultId (snd res)) res) xs
  }
  where
    extraResultId =
      mappend extraResultsPrefixId . WidgetIds.hash . void .
      (^. Sugar.holeResultInferred)
    extraResultsPrefixId = mconcat [prefixId holeInfo, Widget.Id ["extra results"], baseId]
    mkResult resultId (typ, holeResult) =
      Result
      { rType = typ
      , rHoleResult = holeResult
      , rMkWidget = makeWidget resultId holeResult
      , rId = resultId
      }

typeCheckToResultsList ::
  MonadA m => HoleInfo m -> WidgetMaker m ->
  Widget.Id -> [Sugar.HoleResultSeed m (Sugar.MStorePoint m SugarExprPl)] ->
  CT m (Maybe (ResultsList m))
typeCheckToResultsList holeInfo makeWidget baseId options =
  mResultsListOf holeInfo makeWidget baseId <$>
  typeCheckResults holeInfo options

baseExprWithApplyForms ::
  MonadA m => HoleInfo m -> ExprIRef.ExpressionM m () ->
  CT m [ExprIRef.ExpressionM m ApplyFormAnnotation]
baseExprWithApplyForms holeInfo baseExpr =
  maybe [] applyForms <$>
  (hiActions holeInfo ^. Sugar.holeInferExprType) baseExpr
  where
    applyForms baseExprType =
      ExprUtil.applyForms baseExprType baseExpr

storePointExpr ::
  Monoid a =>
  Expr.BodyExpr (DefIM m) (Sugar.MStorePoint m a) ->
  Sugar.ExprStorePoint m a
storePointExpr = (`Expression` (Nothing, mempty))

storePointHole :: Monoid a => Sugar.ExprStorePoint m a
storePointHole = storePointExpr $ ExprLens.bodyHole # ()

storePointHoleWrap :: Monoid a => Sugar.ExprStorePoint m a -> Sugar.ExprStorePoint m a
storePointHoleWrap expr =
  storePointExpr $ ExprUtil.makeApply storePointHole expr

removeWrappers :: Expression def a -> Maybe (Expression def a)
removeWrappers expr
  | Lens.has wrappers expr =
    expr
    & wrappers %~ (^?! ExprLens.exprApply . Expr.applyArg)
    & Just
  | otherwise = Nothing
  where
    wrappers :: Lens.Traversal' (Expression def a) (Expression def a)
    wrappers =
      (ExprLens.subTreesThat . Lens.has)
      (ExprLens.exprApply . Expr.applyFunc . ExprLens.exprHole)

injectIntoHoles ::
  MonadA m => HoleInfo m ->
  Sugar.ExprStorePoint m a ->
  ExprIRef.ExpressionM m (ApplyFormAnnotation, a) ->
  CT m [Sugar.ExprStorePoint m a]
injectIntoHoles holeInfo arg =
  fmap catMaybes . mapM injectArg . injectArgPositions .
  ExprUtil.addExpressionContexts (Lens._1 .~ Nothing) .
  Lens.Context id
  where
    typeCheckOnSide expr =
      (expr <$) <$> (hiActions holeInfo ^. Sugar.holeInferExprType) (void expr)
    toOrd IndependentParamAdded = 'a'
    toOrd DependentParamAdded = 'b'
    toOrd Untouched = 'c'
    condition subExpr =
      Lens.has ExprLens.exprHole subExpr &&
      DependentParamAdded /= (subExpr ^. Expr.ePayload . contextVal . Lens._1)
    injectArg setter =
      runMaybeT . leftToJust .
      mapM_ (justToLeft . MaybeT . typeCheckOnSide . setter) $
      maybeToMPlus (removeWrappers arg) ++ [ arg ]
    injectArgPositions =
      map (^. contextSetter) .
      sortOn (^. contextVal . Lens._1 . Lens.to toOrd) .
      map (^. Expr.ePayload) . filter condition .
      ExprUtil.subExpressions

maybeInjectArgumentExpr ::
  MonadA m => HoleInfo m ->
  [ExprIRef.ExpressionM m ApplyFormAnnotation] ->
  CT m [Sugar.ExprStorePoint m SugarExprPl]
maybeInjectArgumentExpr holeInfo =
  case hiMArgument holeInfo of
  Nothing -> return . map ((Nothing, mempty) <$)
  Just holeArg ->
    fmap concat .
    traverse (injectIntoHoles holeInfo arg . fmap (flip (,) (pl False)))
    where
      pl isInjected = (ExprGuiM.StoredGuids [], ExprGuiM.Injected [isInjected])
      arg =
        holeArg ^. Sugar.haExprPresugared
        <&> Lens._2 .~ pl False
        & Expr.ePayload . Lens._2 .~ pl True

maybeInjectArgumentNewTag ::
  HoleInfo m -> [Sugar.HoleResultSeed m (Sugar.MStorePoint m SugarExprPl)]
maybeInjectArgumentNewTag holeInfo =
  case hiMArgument holeInfo of
  Nothing -> [makeNewTag]
  Just _ -> []
  where
    makeNewTag = Sugar.ResultSeedNewTag $ hiSearchTerm holeInfo

makeResultsList ::
  MonadA m => HoleInfo m -> WidgetMaker m -> GroupM m ->
  CT m (Maybe (ResultsList m))
makeResultsList holeInfo makeWidget group =
  (Lens.mapped . Lens.mapped %~ rlPreferred .~ toPreferred) .
  typeCheckToResultsList holeInfo makeWidget baseId .
  map Sugar.ResultSeedExpression . filter (not . isHoleWrap) =<<
  maybeInjectArgumentExpr holeInfo =<<
  baseExprWithApplyForms holeInfo baseExpr
  where
    isHoleWrap = Lens.has (ExprLens.exprApply . Expr.applyFunc . ExprLens.exprHole)
    toPreferred
      | Lens.anyOf (groupNames . traverse) (== searchTerm) group = Preferred
      | otherwise = NotPreferred
    searchTerm = hiSearchTerm holeInfo
    baseExpr = group ^. groupBaseExpr
    baseId = WidgetIds.hash baseExpr

makeNewTagResultList ::
  MonadA m => HoleInfo m -> WidgetMaker m ->
  CT m (Maybe (ResultsList m))
makeNewTagResultList holeInfo makeNewTagResultWidget
  | null (hiSearchTerm holeInfo) = pure Nothing
  | otherwise =
      typeCheckToResultsList holeInfo makeNewTagResultWidget (Widget.Id ["NewTag"]) $
      maybeInjectArgumentNewTag holeInfo

data HaveHiddenResults = HaveHiddenResults | NoHiddenResults

collectResults :: MonadA m => Config -> ListT m (ResultsList f) -> m ([ResultsList f], HaveHiddenResults)
collectResults config resultsM = do
  (collectedResults, remainingResultsM) <-
    List.splitWhenM (return . (>= Config.holeResultCount config) . length . fst) $
    List.scanl step ([], []) resultsM
  remainingResults <- List.toList $ List.take 2 remainingResultsM
  let
    results =
      last (collectedResults ++ remainingResults)
      & Lens.both %~ reverse
  results
    & Lens._1 %~ sortOn resultsListScore
    & uncurry (++)
    & splitAt (Config.holeResultCount config)
    & Lens._2 %~ haveHiddenResults
    & return
  where
    haveHiddenResults [] = NoHiddenResults
    haveHiddenResults _ = HaveHiddenResults
    resultsListScore x = (x ^. rlPreferred, rType (x ^. rlMain))
    step results x =
      results
      & case resultsListScore x of
        (NotPreferred, BadResult) -> Lens._2
        _ -> Lens._1
        %~ (x :)

makeAll ::
  MonadA m => Config -> HoleInfo m -> MakeWidgets m ->
  ExprGuiM m ([ResultsList m], HaveHiddenResults)
makeAll config holeInfo makeWidget = do
  allGroups <- ExprGuiM.transaction $ makeAllGroups holeInfo
  let
    allGroupsList =
      List.mapL (makeResultsList holeInfo (mkResultWidget makeWidget)) $
      List.fromList allGroups
    newTagList
      | isTagType =
        List.joinM . return . makeNewTagResultList holeInfo $
        mkNewTagResultWidget makeWidget
      | otherwise = mempty
    resultList = List.catMaybes $ mappend allGroupsList newTagList
  ExprGuiM.liftMemoT $ collectResults config resultList
  where
    isTagType =
      Lens.has (Sugar.holeInferredType . ExprLens.exprTagType) $
      hiActions holeInfo

makeAllGroups :: MonadA m => HoleInfo m -> T m [GroupM m]
makeAllGroups holeInfo = do
  Scope
    { _scopeLocals = locals
    , _scopeGlobals = globals
    , _scopeTags = tags
    , _scopeGetParams = getParams
    } <- hiActions holeInfo ^. Sugar.holeScope
  let
    allGroups = concat
      [ primitiveGroups holeInfo, literalGroups
      , localsGroups, globalsGroups, tagsGroups, getParamsGroups
      ]
    sortedGroups f  = sortOn (^. groupNames) . map f
    localsGroups    = sortedGroups getVarsToGroup locals
    globalsGroups   = sortedGroups getVarsToGroup globals
    tagsGroups      = sortedGroups tagsToGroup tags
    getParamsGroups = sortedGroups getParamsToGroup getParams
  pure $ holeMatches (^. groupNames) (hiSearchTerm holeInfo) allGroups
  where
    literalGroups = makeLiteralGroups (hiSearchTerm holeInfo)

primitiveGroups :: HoleInfo m -> [GroupM m]
primitiveGroups holeInfo =
  [ mkGroup ["Type"] $ Expr.BodyLeaf Expr.Type
  , mkGroup ["Integer", "ℤ", "Z"] $ Expr.BodyLeaf Expr.IntegerType
  , mkGroup ["Apply", "Give argument"] . Expr.BodyApply $
    Expr.Apply pureHole pureHole
  , mkGroup ["->", "Pi", "→", "→", "Π", "π"] $
    ExprUtil.makePi (Guid.fromString "NewPi") pureHole pureHole
  , mkGroup ["\\", "Lambda", "Λ", "λ"] $
    ExprUtil.makeLambda (Guid.fromString "NewLambda") pureHole pureHole
  , Group ["Record Value", "{"] .
    fromMaybe (record Expr.KVal) . ExprUtil.recordValForm $
    hiActions holeInfo ^. Sugar.holeInferredType
  , Group ["Record Type", "{"] $ record Expr.KType
  , mkGroup [".", "Get Field"] . Expr.BodyGetField $
    Expr.GetField pureHole pureHole
  ]
  where
    record k =
      ExprUtil.pureExpression . Expr.BodyRecord . Expr.Record k $
      case hiMArgument holeInfo of
      Nothing -> []
      Just _ -> [(pureHole, pureHole)]

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
