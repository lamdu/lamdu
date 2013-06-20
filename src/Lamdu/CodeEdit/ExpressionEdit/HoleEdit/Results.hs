{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Results
  ( makeAll, pick, HaveHiddenResults(..)
  , Result(..)
  , ResultsList(..), rlExtraResultsPrefixId, rlMain, rlExtra
  , prefixId, MakeWidgets(..)
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens.Operators
import Control.Lens.Utils (contextSetter, contextVal)
import Control.Monad ((<=<), void)
import Control.Monad.ListT (ListT)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Either.Utils (leftToJust, justToLeft)
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Cache (Cache)
import Data.Function (on)
import Data.List (isInfixOf, isPrefixOf, partition)
import Data.List.Utils (sortOn, nonEmptyAll)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Info (HoleInfo(..), hiSearchTerm, hiMArgument)
import Lamdu.CodeEdit.Sugar (Scope(..))
import Lamdu.Config (Config)
import Lamdu.Data.Expression (Expression(..))
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Utils (ApplyFormAnnotation(..), pureHole)
import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List.Class as List
import qualified Data.Store.Guid as Guid
import qualified Data.Store.Property as Property
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Info as HoleInfo
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.WidgetIds as WidgetIds

type T = Transaction
type CT m = StateT Cache (T m)

data Group def = Group
  { _groupNames :: [String]
  , _groupBaseExpr :: Expression def ()
  }
type GroupM m = Group (DefI (Tag m))

Lens.makeLenses ''Group

mkGroup :: [String] -> Expr.BodyExpr def () -> Group def
mkGroup names body = Group
  { _groupNames = names
  , _groupBaseExpr = ExprUtil.pureExpression body
  }

pick ::
  MonadA m =>
  HoleInfo m -> Sugar.HoleResult Sugar.Name m -> T m (Maybe Guid)
pick holeInfo holeResult = do
  Property.set (hiState holeInfo) HoleInfo.emptyState
  holeResult ^. Sugar.holeResultPick

data ResultType = GoodResult | BadResult
  deriving (Eq, Ord)

data Result m = Result
  { rType :: ResultType
  , rHoleResult :: Sugar.HoleResult Sugar.Name m
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

resultComplexityScore :: ExprIRef.ExpressionM m (Infer.Inferred (DefI (Tag m))) -> Int
resultComplexityScore = length . Foldable.toList . Infer.iType . (^. Expr.ePayload)

prefixId :: HoleInfo m -> Widget.Id
prefixId holeInfo = mconcat [hiId holeInfo, Widget.Id ["results"]]

type WidgetMaker m = Widget.Id -> Sugar.HoleResult Sugar.Name m -> ExprGuiM m (WidgetT m)
data MakeWidgets m = MakeWidgets
  { mkResultWidget :: WidgetMaker m
  , mkNewTagResultWidget :: WidgetMaker m
  }

typeCheckHoleResult ::
  MonadA m => HoleInfo m -> Sugar.HoleResultSeed m ->
  CT m (Maybe (ResultType, Sugar.HoleResult Sugar.Name m))
typeCheckHoleResult holeInfo seed = do
  mGood <- hiActions holeInfo ^. Sugar.holeResult $ seed
  case (mGood, seed) of
    (Just good, _) -> pure $ Just (GoodResult, good)
    (Nothing, Sugar.ResultSeedExpression expr) ->
      fmap ((,) BadResult) <$>
      ( (hiActions holeInfo ^. Sugar.holeResult)
      . Sugar.ResultSeedExpression . storePointHoleWrap
      ) expr
    _ -> pure Nothing

typeCheckResults ::
  MonadA m => HoleInfo m -> [Sugar.HoleResultSeed m] ->
  CT m [(ResultType, Sugar.HoleResult Sugar.Name m)]
typeCheckResults holeInfo options = do
  rs <- catMaybes <$> traverse (typeCheckHoleResult holeInfo) options
  let (goodResults, badResults) = partition ((== GoodResult) . fst) rs
  return $ sortOn (score . snd) goodResults ++ badResults
  where
    score = resultComplexityScore . (^. Sugar.holeResultInferred)

mResultsListOf ::
  HoleInfo m -> WidgetMaker m -> Widget.Id ->
  [(ResultType, Sugar.HoleResult Sugar.Name m)] ->
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
  Widget.Id -> [Sugar.HoleResultSeed m] ->
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

storePointExpr :: Expr.BodyExpr (DefI (Tag m)) (Maybe (Sugar.StorePoint (Tag m))) -> Sugar.ExprStorePoint m
storePointExpr = (`Expression` Nothing)

storePointHole :: Sugar.ExprStorePoint m
storePointHole = storePointExpr $ ExprLens.bodyHole # ()

storePointHoleWrap :: Sugar.ExprStorePoint m -> Sugar.ExprStorePoint m
storePointHoleWrap expr =
  storePointExpr $ ExprUtil.makeApply storePointHole expr

wrappers :: Lens.Traversal' (Expression def a) (Expression def a)
wrappers =
  (ExprLens.subTreesThat . Lens.has)
  (ExprLens.exprApply . Expr.applyFunc . ExprLens.exprHole)

injectIntoHoles ::
  MonadA m => HoleInfo m ->
  Sugar.ExprStorePoint m ->
  ExprIRef.ExpressionM m ApplyFormAnnotation ->
  CT m [Sugar.ExprStorePoint m]
injectIntoHoles holeInfo arg =
  fmap catMaybes . mapM injectArg . injectArgPositions .
  ExprUtil.addExpressionContexts (const Nothing) .
  Lens.Context id
  where
    typeCheckOnSide expr =
      (expr <$) <$> (hiActions holeInfo ^. Sugar.holeInferExprType) (void expr)
    toOrd IndependentParamAdded = 'a'
    toOrd DependentParamAdded = 'b'
    toOrd Untouched = 'c'
    condition subExpr =
      Lens.has ExprLens.exprHole subExpr &&
      DependentParamAdded /= (subExpr ^. Expr.ePayload . contextVal)
    argWithoutWrappers =
      arg & wrappers %~ (^?! ExprLens.exprApply . Expr.applyArg)
    injectArg setter =
      runMaybeT . leftToJust .
      mapM_ (justToLeft . MaybeT . typeCheckOnSide . setter) $
      [ argWithoutWrappers | Lens.has wrappers arg ] ++
      [ arg ]
    injectArgPositions =
      map (^. contextSetter) .
      sortOn (^. contextVal . Lens.to toOrd) .
      map (^. Expr.ePayload) . filter condition .
      ExprUtil.subExpressions

maybeInjectArgumentExpr ::
  MonadA m => HoleInfo m ->
  [ExprIRef.ExpressionM m ApplyFormAnnotation] ->
  CT m [Sugar.ExprStorePoint m]
maybeInjectArgumentExpr holeInfo =
  case hiMArgument holeInfo of
  Nothing -> return . map (Nothing <$)
  Just holeArg ->
    fmap concat .
    traverse (injectIntoHoles holeInfo (holeArg ^. Sugar.haExprPresugared))

maybeInjectArgumentNewTag :: HoleInfo m -> [Sugar.HoleResultSeed m]
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
collectResults config =
  conclude <=<
  List.splitWhenM (return . (>= Config.holeResultCount config) . length . fst) .
  List.scanl step ([], [])
  where
    haveHiddenResults [] = NoHiddenResults
    haveHiddenResults _ = HaveHiddenResults
    conclude (collectedResults, remainingResultsM) =
      ( (Lens._2 %~ haveHiddenResults) . splitAt (Config.holeResultCount config)
      . uncurry (++)
      . (Lens._1 %~ sortOn resultsListScore)
      . (Lens.both %~ reverse)
      . last . mappend collectedResults
      ) <$>
      List.toList (List.take 2 remainingResultsM)
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
  [ mkGroup ["Set", "Type"] $ Expr.BodyLeaf Expr.Set
  , mkGroup ["Integer", "ℤ", "Z"] $ Expr.BodyLeaf Expr.IntegerType
  , mkGroup ["Apply", "Give argument"] . Expr.BodyApply $
    Expr.Apply pureHole pureHole
  , mkGroup ["->", "Pi", "→", "→", "Π", "π"] $
    ExprUtil.makePi (Guid.fromString "NewPi") pureHole pureHole
  , mkGroup ["\\", "Lambda", "Λ", "λ"] $
    ExprUtil.makeLambda (Guid.fromString "NewLambda") pureHole pureHole
  , Group ["Record Value", "{"] .
    fromMaybe (record Expr.Val) . ExprUtil.recordValForm $
    hiActions holeInfo ^. Sugar.holeInferredType
  , Group ["Record Type", "{"] $ record Expr.Type
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
