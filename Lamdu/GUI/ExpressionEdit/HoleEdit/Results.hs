{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Results
  ( makeAll, HaveHiddenResults(..)
  , Result(..), ResultInfo(..)
  , ResultsList(..), rlExtraResultsPrefixId, rlMain, rlExtra
  , prefixId
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
import Data.Derive.Monoid (makeMonoid)
import Data.DeriveTH (derive)
import Data.Function (on)
import Data.List (isInfixOf, isPrefixOf, partition)
import Data.List.Utils (sortOn, nonEmptyAll)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Maybe.Utils (maybeToMPlus)
import Data.Monoid (Monoid(..), Any(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Lamdu.Config (Config)
import Lamdu.Data.Expr (Expression(..))
import Lamdu.Data.Expression.IRef (DefIM)
import Lamdu.Data.Expression.Utils (ApplyFormAnnotation(..), pureHole)
import Lamdu.Data.Infer.Deref (DerefedTV)
import Lamdu.Data.Infer.Load (ldDef)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), hiSearchTerm, hiMArgument, hiActiveId)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Sugar.Types (Scope(..))
import System.Random.Utils (genFromHashable)
import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.Class as ListClass
import qualified Data.Store.Guid as Guid
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Expr as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Infer.Deref as InferDeref
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar
import qualified System.Random as Random

type T = Transaction
type CT m = StateT Cache (T m)

data SearchTerms = SearchTerms
  { _searchTerms :: [String]
  , __searchTermsHighPrecedence :: Any
  }

data Group def = Group
  { _groupId :: String
  , _groupSearchTerms :: SearchTerms
  , _groupBaseExpr :: Expression def ()
  }
type GroupM m = Group (DefIM m)

derive makeMonoid ''SearchTerms
Lens.makeLenses ''SearchTerms
Lens.makeLenses ''Group

data ResultType = GoodResult | BadResult
  deriving (Eq, Ord)

data ResultInfo = ResultInfoNewTag | ResultInfoNormal

type SugarExprPl = (ExprGuiM.StoredGuids, ExprGuiM.Injected)

data Result m = Result
  { rType :: ResultType
  , rHoleResult :: Sugar.HoleResult Sugar.Name m SugarExprPl
  , rInfo :: ResultInfo
  , rId :: WidgetId.Id
  }

data IsPreferred = Preferred | NotPreferred
  deriving (Eq, Ord)

data ResultsList m = ResultsList
  { _rlPreferred :: IsPreferred -- Move to top of result list
  , _rlExtraResultsPrefixId :: WidgetId.Id
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
  & groupSearchTerms <>~ SearchTerms ["params"] (Any True)

sugarNameToGroup :: Sugar.Name -> Expression def () -> Group def
sugarNameToGroup (Sugar.Name _ collision varName) expr = Group
  { _groupId = "Var" ++ varName ++ ":" ++ concat collisionStrs
  , _groupSearchTerms = SearchTerms (varName : collisionStrs) (Any True)
  , _groupBaseExpr = expr
  }
  where
    collisionStrs =
      case collision of
      Sugar.NoCollision -> []
      Sugar.Collision suffix -> [show suffix]

resultComplexityScore :: Sugar.LoadedExpr m (DerefedTV (DefIM m)) -> [Int]
resultComplexityScore expr =
  [ length . Foldable.toList $ expr ^. Expr.ePayload . InferDeref.dType
  , length $ Foldable.toList expr
  ]

prefixId :: HoleInfo m -> WidgetId.Id
prefixId holeInfo = mconcat [hiActiveId holeInfo, WidgetId.Id ["results"]]

typeCheckHoleResult ::
  MonadA m => HoleInfo m ->
  (Guid -> Random.StdGen) ->
  Sugar.HoleResultSeed m (Sugar.MStorePoint m SugarExprPl) ->
  CT m (Maybe (ResultType, Sugar.HoleResult Sugar.Name m SugarExprPl))
typeCheckHoleResult holeInfo mkGen seed = do
  mGood <- mkHoleResult seed
  case (mGood, seed) of
    (Just good, _) -> pure $ Just (GoodResult, good)
    (Nothing, Sugar.ResultSeedExpression expr) ->
      fmap ((,) BadResult) <$>
      (mkHoleResult . Sugar.ResultSeedExpression . storePointHoleWrap) expr
    _ -> pure Nothing
  where
    mkHoleResult = Sugar.holeResult (hiActions holeInfo) mkGen

typeCheckResults ::
  MonadA m => HoleInfo m ->
  (Int -> Guid -> Random.StdGen) ->
  [Sugar.HoleResultSeed m (Sugar.MStorePoint m SugarExprPl)] ->
  CT m [(ResultType, Sugar.HoleResult Sugar.Name m SugarExprPl)]
typeCheckResults holeInfo mkGen options = do
  rs <-
    options
    & Lens.traversed %%@~ typeCheckHoleResult holeInfo . mkGen
    <&> catMaybes
  let (goodResults, badResults) = partition ((== GoodResult) . fst) rs
  return $ sorted goodResults ++ sorted badResults
  where
    sorted = sortOn (score . snd)
    score = resultComplexityScore . (^. Sugar.holeResultInferred)

mResultsListOf ::
  HoleInfo m -> ResultInfo -> WidgetId.Id ->
  [(ResultType, Sugar.HoleResult Sugar.Name m SugarExprPl)] ->
  Maybe (ResultsList m)
mResultsListOf _ _ _ [] = Nothing
mResultsListOf holeInfo resultInfo baseId (x:xs) = Just
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
    extraResultsPrefixId = mconcat [prefixId holeInfo, WidgetId.Id ["extra results"], baseId]
    mkResult resultId (typ, holeResult) =
      Result
      { rType = typ
      , rHoleResult = holeResult
      , rInfo = resultInfo
      , rId = resultId
      }

typeCheckToResultsList ::
  MonadA m => HoleInfo m -> (Int -> Guid -> Random.StdGen) -> ResultInfo ->
  WidgetId.Id -> [Sugar.HoleResultSeed m (Sugar.MStorePoint m SugarExprPl)] ->
  CT m (Maybe (ResultsList m))
typeCheckToResultsList holeInfo mkGen resultInfo baseId options =
  mResultsListOf holeInfo resultInfo baseId <$>
  typeCheckResults holeInfo mkGen options

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
  Expr.BodyExpr def (Sugar.MStorePoint m a) ->
  Expr.Expression def (Sugar.MStorePoint m a)
storePointExpr = (`Expression` (Nothing, mempty))

storePointHole :: Monoid a => Expr.Expression def (Sugar.MStorePoint m a)
storePointHole = storePointExpr $ ExprLens.bodyHole # ()

storePointHoleWrap ::
  Monoid a =>
  Expr.Expression def (Sugar.MStorePoint m a) ->
  Expr.Expression def (Sugar.MStorePoint m a)
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
  MonadA m =>
  HoleInfo m ->
  ExprIRef.ExpressionM m (Sugar.MStorePoint m a) ->
  ExprIRef.ExpressionM m (ApplyFormAnnotation, a) ->
  CT m [ExprIRef.ExpressionM m (Sugar.MStorePoint m a)]
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
  CT m [ExprIRef.ExpressionM m (Sugar.MStorePoint m SugarExprPl)]
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
        -- TODO: Don't wastefully lose the loaded...
        & ExprLens.exprDef %~ (^. ldDef)

maybeInjectArgumentNewTag ::
  HoleInfo m -> [Sugar.HoleResultSeed m (Sugar.MStorePoint m SugarExprPl)]
maybeInjectArgumentNewTag holeInfo =
  case hiMArgument holeInfo of
  Nothing -> [makeNewTag]
  Just _ -> []
  where
    makeNewTag = Sugar.ResultSeedNewTag $ hiSearchTerm holeInfo

makeResultsList ::
  MonadA m => HoleInfo m -> ResultInfo -> GroupM m ->
  CT m (Maybe (ResultsList m))
makeResultsList holeInfo resultInfo group =
  (Lens.mapped . Lens.mapped %~ rlPreferred .~ toPreferred) .
  typeCheckToResultsList holeInfo mkGen resultInfo baseId .
  map Sugar.ResultSeedExpression . filter (not . isHoleWrap) =<<
  maybeInjectArgumentExpr holeInfo =<<
  baseExprWithApplyForms holeInfo baseExpr
  where
    mkGen i guid = genFromHashable (guid, group ^. groupId, i)
    isHoleWrap = Lens.has (ExprLens.exprApply . Expr.applyFunc . ExprLens.exprHole)
    toPreferred
      | Lens.anyOf groupSearchTerms (preferFor searchTerm) group = Preferred
      | otherwise = NotPreferred
    searchTerm = hiSearchTerm holeInfo
    baseExpr = group ^. groupBaseExpr
    baseId = WidgetIds.hash baseExpr

makeNewTagResultList ::
  MonadA m => HoleInfo m ->
  CT m (Maybe (ResultsList m))
makeNewTagResultList holeInfo
  | null (hiSearchTerm holeInfo) = pure Nothing
  | otherwise =
      typeCheckToResultsList holeInfo mkGen ResultInfoNewTag (WidgetId.Id ["NewTag"]) $
      maybeInjectArgumentNewTag holeInfo
  where
    mkGen i guid = genFromHashable (guid, "New tag" :: String, i)

data HaveHiddenResults = HaveHiddenResults | NoHiddenResults

collectResults :: MonadA m => Config -> ListT m (ResultsList f) -> m ([ResultsList f], HaveHiddenResults)
collectResults config resultsM = do
  (collectedResults, remainingResultsM) <-
    ListClass.splitWhenM (return . (>= Config.holeResultCount config) . length . fst) $
    ListClass.scanl step ([], []) resultsM
  remainingResults <- ListClass.toList $ ListClass.take 2 remainingResultsM
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
  MonadA m => Config -> HoleInfo m ->
  ExprGuiM m ([ResultsList m], HaveHiddenResults)
makeAll config holeInfo = do
  allGroups <- ExprGuiM.transaction $ makeAllGroups holeInfo
  let
    allGroupsList =
      ListClass.mapL (makeResultsList holeInfo ResultInfoNormal) $
      ListClass.fromList allGroups
    newTagList
      | isTagType =
        ListClass.joinM . return $ makeNewTagResultList holeInfo
      | otherwise = mempty
    resultList = ListClass.catMaybes $ mappend allGroupsList newTagList
  ExprGuiM.liftMemoT $ collectResults config resultList
  where
    isTagType =
      Lens.has ExprLens.exprTagType $
      hiInferred holeInfo ^. Sugar.hiType

makeAllGroups :: MonadA m => HoleInfo m -> T m [GroupM m]
makeAllGroups holeInfo = do
  Scope
    { _scopeLocals = locals
    , _scopeGlobals = globals
    , _scopeTags = tags
    , _scopeGetParams = getParams
    } <- hiActions holeInfo ^. Sugar.holeScope
  let
    allGroups =
      addInferredGroups $
      concat
      [ primitiveGroups holeInfo
      , localsGroups
      , globalsGroups
      , tagsGroups
      , getParamsGroups
      ]
    sortedGroups f  = sortOn (^. groupSearchTerms . searchTerms) . map f
    localsGroups    = sortedGroups getVarsToGroup locals
    globalsGroups   = sortedGroups getVarsToGroup globals
    tagsGroups      = sortedGroups tagsToGroup tags
    getParamsGroups = sortedGroups getParamsToGroup getParams
  pure $ holeMatches (^. groupSearchTerms) (hiSearchTerm holeInfo) allGroups
  where
    inferredGroups =
      [ Group
        { _groupId = "inferred"
        , _groupSearchTerms = SearchTerms ["inferred"] (Any True)
        , _groupBaseExpr = iVal
        }
      | Lens.nullOf ExprLens.exprHole iVal
      ]
    addInferredGroups groups =
      let
        (dupsOfInferred, others) =
          List.partition (ExprUtil.alphaEq iVal . (^. groupBaseExpr)) groups
        dupsGroupNames = dupsOfInferred ^. Lens.traverse . groupSearchTerms
      in
        ( inferredGroups & Lens.traverse . groupSearchTerms <>~ dupsGroupNames
        ) ++ others
    iVal =
      hiInferred holeInfo ^. Sugar.hiBaseValue
      & ExprLens.exprDef %~ (^. ldDef)

primitiveGroups :: HoleInfo m -> [GroupM m]
primitiveGroups holeInfo =
  [ mkGroupBody True "LiteralInt" [searchTerm] $ ExprLens.bodyLiteralInteger # read searchTerm
  | nonEmptyAll Char.isDigit searchTerm
  ] ++
  [ mkGroupBody False "Pi" ["->", "Pi", "→", "→", "Π", "π"] $
    ExprUtil.makePi (Guid.fromString "NewPi") pureHole pureHole
  , mkGroupBody False "Lambda" ["\\", "Lambda", "Λ", "λ"] $
    ExprUtil.makeLambda (Guid.fromString "NewLambda") pureHole pureHole
  , mkGroupBody False "GetField" [".", "Get Field"] . Expr.BodyGetField $
    Expr.GetField pureHole pureHole
  , mkGroupBody True "Type" ["Type"] $ Expr.BodyLeaf Expr.Type
  , mkGroupBody True "Integer" ["Integer", "ℤ", "Z"] $ Expr.BodyLeaf Expr.IntegerType
  , Group "RecValue" (SearchTerms ["Record Value", "{"] (Any False)) .
    fromMaybe (record Expr.KVal) . ExprUtil.recordValForm .
    void $ hiInferred holeInfo ^. Sugar.hiType
  , Group "RecType" (SearchTerms ["Record Type", "{"] (Any False)) $
    record Expr.KType
  ]
  where
    searchTerm = hiSearchTerm holeInfo
    record k =
      ExprUtil.pureExpression . Expr.BodyRecord . Expr.Record k $
      case hiMArgument holeInfo of
      Nothing -> []
      Just _ -> [(pureHole, pureHole)]
    mkGroupBody highPrecedence gId terms body = Group
      { _groupId = gId
      , _groupSearchTerms = SearchTerms terms (Any highPrecedence)
      , _groupBaseExpr = ExprUtil.pureExpression body
      }

preferFor :: String -> SearchTerms -> Bool
preferFor searchTerm (SearchTerms terms (Any True)) = searchTerm `elem` terms
preferFor _ _ = False

groupOrdering :: String -> SearchTerms -> [Bool]
groupOrdering searchTerm (SearchTerms terms (Any highPrecedence)) =
  map not
  [ highPrecedence
  , match (==)
  , match isPrefixOf
  , match insensitivePrefixOf
  , match isInfixOf
  ]
  where
    insensitivePrefixOf = isPrefixOf `on` map Char.toLower
    match f = any (f searchTerm) terms

holeMatches :: (a -> SearchTerms) -> String -> [a] -> [a]
holeMatches getSearchTerms searchTerm =
  sortOn (groupOrdering searchTerm . getSearchTerms) .
  filter (nameMatch . getSearchTerms)
  where
    nameMatch (SearchTerms _ (Any False))
      | null searchTerm = False
    nameMatch (SearchTerms terms _) =
      any (insensitiveInfixOf searchTerm) terms
    insensitiveInfixOf = isInfixOf `on` map Char.toLower
