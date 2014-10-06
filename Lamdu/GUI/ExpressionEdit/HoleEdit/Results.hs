{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Results
  ( makeAll, HaveHiddenResults(..)
  , Result(..)
  , ResultsList(..), rlExtraResultsPrefixId, rlMain, rlExtra
  , prefixId
  , SugarExprPl
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad (void, join, guard)
import Control.Monad.ListT (ListT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either.Utils (leftToJust, justToLeft)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State (evalStateT)
import Control.MonadA (MonadA)
import Data.Derive.Monoid (makeMonoid)
import Data.DeriveTH (derive)
import Data.Function (on)
import Data.List (isInfixOf, isPrefixOf, partition)
import Data.List.Utils (sortOn, nonEmptyAll)
import Data.Maybe (catMaybes)
import Data.Maybe.Utils (maybeToMPlus)
import Data.Monoid (Monoid(..), Any(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse, sequenceA)
import Lamdu.Config (Config)
import Lamdu.Expr.IRef (DefIM)
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val(..))
import Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), hiSearchTerm, hiMArgument, hiActiveId)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Sugar.Types (Scope(..))
import System.Random.Utils (genFromHashable)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Char as Char
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.Class as ListClass
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Lamdu.Config as Config
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Scheme as S
import qualified Lamdu.Expr.Type as T
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Infer as Infer
import qualified Lamdu.Sugar.Types as Sugar
import qualified System.Random as Random

type T = Transaction

data SearchTerms = SearchTerms
  { _searchTerms :: [String]
  , __searchTermsHighPrecedence :: Any
  }

data Group def = Group
  { _groupId :: String
  , _groupSearchTerms :: SearchTerms
  , _groupBaseExpr :: Val ()
  }
type GroupM m = Group (DefIM m)

derive makeMonoid ''SearchTerms
Lens.makeLenses ''SearchTerms
Lens.makeLenses ''Group

data ResultType = GoodResult | BadResult
  deriving (Eq, Ord)

type SugarExprPl = (ExprGuiM.StoredGuids, ExprGuiM.Injected)

data Result m = Result
  { rType :: ResultType
  , rHoleResult :: Sugar.HoleResult Sugar.Name m SugarExprPl
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

getVarsToGroup :: (Sugar.GetVar Sugar.Name m, Val ()) -> Group def
getVarsToGroup (getVar, expr) = sugarNameToGroup (getVar ^. Sugar.gvName) expr

-- tagsToGroup :: (Sugar.TagG Sugar.Name, Val ()) -> Group def
-- tagsToGroup (tagG, expr) = sugarNameToGroup (tagG ^. Sugar.tagName) expr

getParamsToGroup :: (Sugar.GetParams Sugar.Name m, Val ()) -> Group def
getParamsToGroup (getParams, expr) =
  sugarNameToGroup (getParams ^. Sugar.gpDefName) expr
  & groupSearchTerms <>~ SearchTerms ["params"] (Any True)

sugarNameToGroup :: Sugar.Name -> Val () -> Group def
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

resultComplexityScore :: Val Sugar.Inferred -> [Int]
resultComplexityScore expr =
  [ length . show $ expr ^. V.payload . Infer.plType
  , length $ Foldable.toList expr
  ]

prefixId :: HoleInfo m -> WidgetId.Id
prefixId holeInfo = mconcat [hiActiveId holeInfo, WidgetId.Id ["results"]]

hole :: Monoid a => Val a
hole = Val mempty $ V.BLeaf V.LHole

storePointHoleWrap ::
  Monoid a =>
  Val (Sugar.MStorePoint m a) ->
  Val (Sugar.MStorePoint m a)
storePointHoleWrap expr =
  noStorePoint $ V.BApp $ V.Apply (noStorePoint (V.BLeaf V.LHole)) expr
  where
    noStorePoint = Val (Nothing, mempty)

typeCheckHoleResult ::
  MonadA m => HoleInfo m ->
  (Guid -> Random.StdGen) ->
  Val (Sugar.MStorePoint m SugarExprPl) ->
  T m (Maybe (ResultType, Sugar.HoleResult Sugar.Name m SugarExprPl))
typeCheckHoleResult holeInfo mkGen val = do
  mGood <- mkHoleResult val
  case mGood of
    Just good -> pure $ Just (GoodResult, good)
    Nothing ->
      fmap ((,) BadResult) <$>
      (mkHoleResult . storePointHoleWrap) val
  where
    mkHoleResult = Sugar.holeResult (hiActions holeInfo) mkGen

typeCheckResults ::
  MonadA m => HoleInfo m ->
  (Int -> Guid -> Random.StdGen) ->
  [Val (Sugar.MStorePoint m SugarExprPl)] ->
  T m [(ResultType, Sugar.HoleResult Sugar.Name m SugarExprPl)]
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
  HoleInfo m -> WidgetId.Id ->
  [(ResultType, Sugar.HoleResult Sugar.Name m SugarExprPl)] ->
  Maybe (ResultsList m)
mResultsListOf _ _ [] = Nothing
mResultsListOf holeInfo baseId (x:xs) = Just
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
      , rId = resultId
      }

typeCheckToResultsList ::
  MonadA m => HoleInfo m -> (Int -> Guid -> Random.StdGen) ->
  WidgetId.Id -> [Val (Sugar.MStorePoint m SugarExprPl)] ->
  T m (Maybe (ResultsList m))
typeCheckToResultsList holeInfo mkGen baseId options =
  mResultsListOf holeInfo baseId <$>
  typeCheckResults holeInfo mkGen options

applyForms :: Monoid a => Type -> Val a -> [Val a]
applyForms typ base =
  base : case typ of
         T.TFun _ res -> applyForms res (addApply base)
         _ -> []
  where
    addApply x = Val mempty $ V.BApp $ V.Apply x hole

baseExprWithApplyForms :: MonadA m => HoleInfo m -> Val () -> T m [Val ()]
baseExprWithApplyForms holeInfo baseExpr =
  maybe [] (`applyForms` baseExpr) <$>
  (hiActions holeInfo ^. Sugar.holeInferExprType) baseExpr

removeWrappers :: Val a -> Maybe (Val a)
removeWrappers (Val pl body) =
  case body of
  V.BApp (V.Apply (Val _ (V.BLeaf V.LHole)) arg) -> Just arg
  _ -> do
    _removedHole <- guard $ Just True == removedFromChildren ^? Lens.traversed . _1
    Just $ Val pl $ snd <$> removedFromChildren
  where
    removedFromChildren = body & Lens.traversed %~ f
    f child =
      case removeWrappers child of
      Nothing -> (False, child)
      Just x -> (True, x)

replaceEachHole :: Applicative f => (a -> f (Val a)) -> Val a -> [f (Val a)]
replaceEachHole replaceHole =
  (`evalStateT` False) . go
  where
    go oldVal@(Val x body) = do
      alreadyReplaced <- State.get
      if alreadyReplaced
        then return (pure oldVal)
        else
          case body of
          V.BLeaf V.LHole ->
            join $ lift
              [ return (pure oldVal)
              , do
                  State.put True
                  return $ replaceHole x
              ]
          _ -> fmap (Val x) . sequenceA <$> traverse go body

injectIntoHoles ::
  MonadA m =>
  Sugar.HoleActions name m ->
  Val (Sugar.MStorePoint m a) ->
  Val a ->
  T m [Val (Sugar.MStorePoint m a)]
injectIntoHoles actions arg =
  fmap catMaybes . mapM firstToTypeCheck .
  replaceEachHole (const (maybeToMPlus (removeWrappers arg) ++ [arg])) .
  fmap ((,) Nothing)
  where
    typeCheckOnSide val = do
      mType <- actions ^. Sugar.holeInferExprType $ void val
      return $ val <$ mType
    firstToTypeCheck =
      runMaybeT . leftToJust . mapM_ (justToLeft . MaybeT . typeCheckOnSide)

maybeInjectArgumentExpr ::
  MonadA m => HoleInfo m ->
  [Val ()] -> T m [Val (Sugar.MStorePoint m SugarExprPl)]
maybeInjectArgumentExpr holeInfo =
  case hiMArgument holeInfo of
  Nothing -> return . map ((Nothing, mempty) <$)
  Just holeArg ->
    fmap concat .
    traverse
    (injectIntoHoles (hiActions holeInfo) arg .
     (pl False <$))
    where
      pl isInjected = (ExprGuiM.StoredGuids [], ExprGuiM.Injected [isInjected])
      arg =
        holeArg ^. Sugar.haExprPresugared
        <&> _2 .~ pl False
        & V.payload . _2 .~ pl True

makeResultsList ::
  MonadA m => HoleInfo m -> GroupM m ->
  T m (Maybe (ResultsList m))
makeResultsList holeInfo group =
  (Lens.mapped . Lens.mapped %~ rlPreferred .~ toPreferred) .
  typeCheckToResultsList holeInfo mkGen baseId .
  filter (not . isHoleWrap) =<<
  maybeInjectArgumentExpr holeInfo =<<
  baseExprWithApplyForms holeInfo baseExpr
  where
    mkGen i guid = genFromHashable (guid, group ^. groupId, i)
    isHoleWrap = Lens.has (ExprLens.valApply . V.applyFunc . ExprLens.valHole)
    toPreferred
      | Lens.anyOf groupSearchTerms (preferFor searchTerm) group = Preferred
      | otherwise = NotPreferred
    searchTerm = hiSearchTerm holeInfo
    baseExpr = group ^. groupBaseExpr
    baseId = WidgetIds.hash baseExpr

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
    & _1 %~ sortOn resultsListScore
    & uncurry (++)
    & splitAt (Config.holeResultCount config)
    & _2 %~ haveHiddenResults
    & return
  where
    haveHiddenResults [] = NoHiddenResults
    haveHiddenResults _ = HaveHiddenResults
    resultsListScore x = (x ^. rlPreferred, rType (x ^. rlMain))
    step results x =
      results
      & case resultsListScore x of
        (NotPreferred, BadResult) -> _2
        _ -> _1
        %~ (x :)

makeAll ::
  MonadA m => Config -> HoleInfo m ->
  ExprGuiM m ([ResultsList m], HaveHiddenResults)
makeAll config holeInfo = do
  allGroups <- ExprGuiM.transaction $ makeAllGroups holeInfo
  let
    allGroupsList =
      ListClass.mapL (makeResultsList holeInfo) $
      ListClass.fromList allGroups
    resultList = ListClass.catMaybes allGroupsList
  ExprGuiM.transaction $ collectResults config resultList
  -- where
  --   isTagType =
  --     Lens.has ExprLens.valTagType $
  --     hiInferred holeInfo ^. Sugar.hiType

makeAllGroups :: MonadA m => HoleInfo m -> T m [GroupM m]
makeAllGroups holeInfo = do
  Scope
    { _scopeLocals = locals
    , _scopeGlobals = globals
    -- , _scopeTags = tags
    , _scopeGetParams = getParams
    } <- hiActions holeInfo ^. Sugar.holeScope
  let
    allGroups =
      addInferredGroups $
      concat
      [ primitiveGroups holeInfo
      , localsGroups
      , globalsGroups
      , getParamsGroups
      ]
    sortedGroups f  = sortOn (^. groupSearchTerms . searchTerms) . map f
    localsGroups    = sortedGroups getVarsToGroup locals
    globalsGroups   = sortedGroups getVarsToGroup globals
    getParamsGroups = sortedGroups getParamsToGroup getParams
  pure $ holeMatches (^. groupSearchTerms) (hiSearchTerm holeInfo) allGroups
  where
    inferredGroups =
      [ Group
        { _groupId = "inferred"
        , _groupSearchTerms = SearchTerms ["inferred"] (Any True)
        , _groupBaseExpr = iVal
        }
      | Lens.nullOf ExprLens.valHole iVal
      ]
    addInferredGroups groups =
      let
        (dupsOfInferred, others) =
          List.partition (V.alphaEq iVal . (^. groupBaseExpr)) groups
        dupsGroupNames = dupsOfInferred ^. Lens.traverse . groupSearchTerms
      in
        ( inferredGroups & Lens.traverse . groupSearchTerms <>~ dupsGroupNames
        ) ++ others
    iVal = hiInferred holeInfo ^. Sugar.hiSuggestedValue

primitiveGroups :: HoleInfo m -> [GroupM m]
primitiveGroups holeInfo =
  [ mkGroupBody True "LiteralInt" [searchTerm] $
    V.BLeaf $ V.LLiteralInteger $ read searchTerm
  | nonEmptyAll Char.isDigit searchTerm
  ] ++
  [ mkGroupBody False "Lambda" ["\\", "Lambda", "Λ", "λ"] $
    V.BAbs $ V.Lam "NewLambda" S.any hole
  -- , mkGroupBody False "GetField" [".", "Get Field"] . V.VGetField $
  --   V.GetField pureHole pureHole
  -- , Group "RecValue" (SearchTerms ["Record Value", "{"] (Any False)) .
  --   fromMaybe (record V.KVal) . ExprUtil.recordValForm .
  --   void $ hiInferred holeInfo ^. Sugar.hiType
  -- , Group "RecType" (SearchTerms ["Record Type", "{"] (Any False)) $
  --   record V.KType
  ]
  where
    searchTerm = hiSearchTerm holeInfo
    -- record k =
    --   ExprUtil.pureExpr . V.VRec . V.Record k $
    --   case hiMArgument holeInfo of
    --   Nothing -> []
    --   Just _ -> [(pureHole, pureHole)]
    mkGroupBody highPrecedence gId terms body = Group
      { _groupId = gId
      , _groupSearchTerms = SearchTerms terms (Any highPrecedence)
      , _groupBaseExpr = Val () body
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
