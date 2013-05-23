{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Lamdu.CodeEdit.ExpressionEdit.HoleEdit.Results
  ( makeAll, pick, HaveHiddenResults(..)
  , Result(..), ResultsList(..)
  , prefixId, MakeWidgets(..)
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens.Operators
import Control.Monad ((<=<), guard, void, join)
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
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
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

mkGroup :: [String] -> Expression.BodyExpr def () -> Group def
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

resultComplexityScore :: DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m))) -> Int
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
      Expression
      ( ExprUtil.makeApply
        (Expression (Expression.BodyLeaf Expression.Hole) Nothing)
        expr
      )
      Nothing
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
  MonadA m => HoleInfo m -> WidgetMaker m -> DataIRef.ExpressionM m () ->
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
      ExprUtil.applyForms baseExprType baseExpr ++ do
        record <-
          baseExprType ^..
          ExprLens.exprLam . Expression.lambdaParamType .
          ExprLens.exprRecord
        let tags = record ^.. Expression.recordFields . traverse . Lens._1
        guard $ length tags == 1
        tag <- tags
        pure . ExprUtil.pureExpression .
          ExprUtil.makeLambda paramGuid ExprUtil.pureHole .
          ExprUtil.pureApply baseExpr . ExprUtil.pureExpression $
          Expression.BodyRecord
          Expression.Record
          { Expression._recordKind = Expression.Val
          , Expression._recordFields =
              [ ( tag
                , ExprUtil.pureExpression $ Lens.review ExprLens.bodyParameterRef paramGuid
                )
              ]
          }
    baseId = WidgetIds.hash baseExpr
    paramGuid = Guid.fromString "guidy"

applyOperatorResultsList ::
  MonadA m => HoleInfo m -> WidgetMaker m ->
  DataIRef.ExpressionM m (Maybe (Sugar.StorePoint (Tag m))) ->
  DataIRef.ExpressionM m () ->
  CT m (Maybe (ResultsList m))
applyOperatorResultsList holeInfo makeWidget argument baseExpr =
  toMResultsList holeInfo makeWidget baseId .
  map Sugar.ResultSeedExpression =<<
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
  apply <- expr ^? ExprLens.exprApply
  void $
    apply ^?
    Expression.applyFunc . Expression.eBody .
    Expression._BodyLeaf . Expression._Hole
  pure $ apply ^. Expression.applyArg

data ResultType = GoodResult | BadResult

makeResultsList ::
  MonadA m => HoleInfo m -> WidgetMaker m -> GroupM m ->
  CT m (Maybe (ResultsList m))
makeResultsList holeInfo makeWidget group =
  case Property.value (hiState holeInfo) ^. HoleInfo.hsArgument of
  Just arg -> applyOperatorResultsList holeInfo makeWidget arg baseExpr
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
