{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
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
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (isInfixOf, isPrefixOf)
import Data.List.Utils (sortOn, nonEmptyAll)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Info (HoleInfo(..))
import Lamdu.CodeEdit.Sugar (Scope(..))
import Lamdu.Data.Expression (Expression(..))
import Lamdu.Data.Expression.IRef (DefI)
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

data Result m = Result
  { rType :: ResultType
  , rHoleResult :: Sugar.HoleResult Sugar.Name m
  , rMkWidget :: ExprGuiM m (WidgetT m)
  , rId :: Widget.Id
  }

data ResultsList m = ResultsList
  { rlExtraResultsPrefixId :: Widget.Id
  , rlMain :: Result m
  , rlExtra :: [Result m]
  }

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
resultComplexityScore =
  sum . map (subtract 2 . length . Foldable.toList . Infer.iType) .
  Foldable.toList

prefixId :: HoleInfo m -> Widget.Id
prefixId holeInfo = mconcat [hiHoleId holeInfo, Widget.Id ["results"]]

type WidgetMaker m = Widget.Id -> Sugar.HoleResult Sugar.Name m -> ExprGuiM m (WidgetT m)
data MakeWidgets m = MakeWidgets
  { mkResultWidget :: WidgetMaker m
  , mkNewTagResultWidget :: WidgetMaker m
  }

toMResultsList ::
  MonadA m => HoleInfo m -> WidgetMaker m ->
  Widget.Id -> [Sugar.HoleResultSeed m] ->
  CT m (Maybe (ResultsList m))
toMResultsList holeInfo makeWidget baseId options = do
  rs <- catMaybes <$> traverse processOption options
  let
    (badResults, goodResults) = partitionEithers rs
    results =
      map ((,) GoodResult)
      (sortOn (resultComplexityScore . (^. Sugar.holeResultInferred))
       goodResults) ++
      map ((,) BadResult) badResults
  return $ case results of
    [] -> Nothing
    x : xs ->
      Just
      ResultsList
      { rlExtraResultsPrefixId = extraResultsPrefixId
      , rlMain = mkResult (mconcat [prefixId holeInfo, baseId]) x
      , rlExtra = map (\res -> mkResult (extraResultId (snd res)) res) xs
      }
  where
    processOption seed = do
      mGood <- hiHoleActions holeInfo ^. Sugar.holeResult $ seed
      case (mGood, seed) of
        (Just good, _) -> pure . Just $ Right good
        (Nothing, Sugar.ResultSeedExpression expr) ->
          fmap Left <$>
          ( (hiHoleActions holeInfo ^. Sugar.holeResult)
          . Sugar.ResultSeedExpression . holeApply
          ) expr
        _ -> pure Nothing
    holeApply expr =
      storePointExpr $ ExprUtil.makeApply storePointHole expr
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

baseExprToResultsList ::
  MonadA m => HoleInfo m -> WidgetMaker m -> ExprIRef.ExpressionM m () ->
  CT m (Maybe (ResultsList m))
baseExprToResultsList holeInfo makeWidget baseExpr =
  fmap join . traverse conclude =<<
  (hiHoleActions holeInfo ^. Sugar.holeInferExprType) baseExpr
  where
    conclude baseExprType =
      toMResultsList holeInfo makeWidget baseId .
      map (Sugar.ResultSeedExpression . (Nothing <$)) $
      applyForms baseExprType
    applyForms baseExprType =
      ExprUtil.applyForms baseExprType baseExpr
    baseId = WidgetIds.hash baseExpr

injectLenses :: [Lens.ALens s t a b] -> b -> s -> [t]
injectLenses lenses b s = ($ s) . (#~ b) <$> lenses

injectLam :: expr -> Expr.Lambda expr -> [Expr.Lambda expr]
injectLam = injectLenses [Expr.lambdaParamType, Expr.lambdaResult]

injectGetField :: expr -> Expr.GetField expr -> [Expr.GetField expr]
injectGetField = injectLenses [Expr.getFieldRecord, Expr.getFieldTag]

storePointExpr :: Expr.BodyExpr (DefI (Tag m)) (Maybe (Sugar.StorePoint (Tag m))) -> Sugar.ExprStorePoint m
storePointExpr = (`Expression` Nothing)

storePointHole :: Sugar.ExprStorePoint m
storePointHole = storePointExpr $ ExprLens.bodyHole # ()

injectApply ::
  Sugar.ExprStorePoint m ->
  ExprIRef.ExpressionM m () ->
  ExprIRef.ExpressionM m () ->
  [Sugar.ExprStorePoint m]
injectApply arg baseExpr baseExprType = do
  [storePointExpr (ExprUtil.makeApply (Nothing <$ base) arg)]
  where
    base = ExprUtil.applyDependentPis baseExprType baseExpr

injectArg ::
  MonadA m => HoleInfo m -> WidgetMaker m ->
  Sugar.ExprStorePoint m -> ExprIRef.ExpressionM m () ->
  CT m (Maybe (ResultsList m))
injectArg holeInfo makeWidget rawArg baseExpr =
  toMResultsList holeInfo makeWidget baseId .
  map Sugar.ResultSeedExpression =<<
  case (Nothing <$ baseExpr) ^. Expr.eBody of
  Expr.BodyLam lam ->
    pure $ storePointExpr . Expr.BodyLam <$> injectLam arg lam
  Expr.BodyGetField getField ->
    pure $ storePointExpr . Expr.BodyGetField <$> injectGetField arg getField
  _ ->
    maybe [] (injectApply arg baseExpr) <$>
    (hiHoleActions holeInfo ^. Sugar.holeInferExprType) baseExpr
  where
    arg = fromMaybe rawArg $ removeHoleWrap rawArg
    baseId = WidgetIds.hash baseExpr

removeHoleWrap :: Expression def a -> Maybe (Expression def a)
removeHoleWrap expr = do
  apply <- expr ^? ExprLens.exprApply
  void $
    apply ^?
    Expr.applyFunc . Expr.eBody .
    Expr._BodyLeaf . Expr._Hole
  pure $ apply ^. Expr.applyArg

data ResultType = GoodResult | BadResult

makeResultsList ::
  MonadA m => HoleInfo m -> WidgetMaker m -> GroupM m ->
  CT m (Maybe (ResultsList m))
makeResultsList holeInfo makeWidget group =
  case Property.value (hiState holeInfo) ^. HoleInfo.hsArgument of
  Just arg -> injectArg holeInfo makeWidget arg baseExpr
  Nothing -> toResList baseExpr
  where
    toResList = baseExprToResultsList holeInfo makeWidget
    baseExpr = group ^. groupBaseExpr

makeNewTagResultList ::
  MonadA m => HoleInfo m -> WidgetMaker m ->
  CT m (Maybe (ResultsList m))
makeNewTagResultList holeInfo makeNewTagResultWidget
  | null searchTerm = pure Nothing
  | otherwise =
      toMResultsList holeInfo makeNewTagResultWidget (Widget.Id ["NewTag"])
      [makeNewTag]
  where
    searchTerm = (Property.value . hiState) holeInfo ^. HoleInfo.hsSearchTerm
    makeNewTag = Sugar.ResultSeedNewTag searchTerm

data HaveHiddenResults = HaveHiddenResults | NoHiddenResults

collectResults :: MonadA m => ListT m (ResultsList f) -> m ([ResultsList f], HaveHiddenResults)
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
    step results x =
      results
      & case rType (rlMain x) of
        GoodResult -> Lens._1
        BadResult -> Lens._2
        %~ (x :)

makeAll ::
  MonadA m => HoleInfo m -> MakeWidgets m ->
  ExprGuiM m ([ResultsList m], HaveHiddenResults)
makeAll holeInfo makeWidget = do
  resultList <-
    List.catMaybes .
    (`mappend` (List.joinM . List.fromList) [makeNewTagResultList holeInfo (mkNewTagResultWidget makeWidget)]) .
    List.mapL (makeResultsList holeInfo (mkResultWidget makeWidget)) .
    List.fromList <$>
    ExprGuiM.transaction (makeAllGroups holeInfo)
  ExprGuiM.liftMemoT $ collectResults resultList

makeAllGroups :: MonadA m => HoleInfo m -> T m [GroupM m]
makeAllGroups holeInfo = do
  Scope
    { _scopeLocals = locals
    , _scopeGlobals = globals
    , _scopeTags = tags
    , _scopeGetParams = getParams
    } <- hiHoleActions holeInfo ^. Sugar.holeScope
  let
    relevantGroups = concat
      [ primitiveGroups, literalGroups
      , localsGroups, globalsGroups, tagsGroups, getParamsGroups
      ]
    sortedGroups f  = sortOn (^. groupNames) . map f
    localsGroups    = sortedGroups getVarsToGroup locals
    globalsGroups   = sortedGroups getVarsToGroup globals
    tagsGroups      = sortedGroups tagsToGroup tags
    getParamsGroups = sortedGroups getParamsToGroup getParams
  pure $ holeMatches (^. groupNames) searchTerm relevantGroups
  where
    literalGroups = makeLiteralGroups searchTerm
    state = Property.value $ hiState holeInfo
    searchTerm = state ^. HoleInfo.hsSearchTerm
    primitiveGroups =
      [ mkGroup ["Set", "Type"] $ Expr.BodyLeaf Expr.Set
      , mkGroup ["Integer", "ℤ", "Z"] $ Expr.BodyLeaf Expr.IntegerType
      , mkGroup ["->", "Pi", "→", "→", "Π", "π"] $
        ExprUtil.makePi (Guid.fromString "NewPi") holeExpr holeExpr
      , mkGroup ["\\", "Lambda", "Λ", "λ"] $
        ExprUtil.makeLambda (Guid.fromString "NewLambda") holeExpr holeExpr
      , mkGroup ["Record Type", "{"] . Expr.BodyRecord $
        Expr.Record Expr.Type mempty
      , mkGroup ["Record Value", "{"] . Expr.BodyRecord $
        Expr.Record Expr.Val mempty
      , mkGroup [".", "Get Field"] . Expr.BodyGetField $
        Expr.GetField ExprUtil.pureHole ExprUtil.pureHole
      ]
    holeExpr = ExprUtil.pureExpression $ Expr.BodyLeaf Expr.Hole

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
