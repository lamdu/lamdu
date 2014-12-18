{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts, DeriveGeneric #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Results
  ( makeAll, HaveHiddenResults(..)
  , Result(..)
  , ResultsList(..), rlExtraResultsPrefixId, rlMain, rlExtra
  , prefixId
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad.ListT (ListT)
import Control.MonadA (MonadA)
import Data.Function (on)
import Data.List (isInfixOf, isPrefixOf)
import Data.List.Utils (sortOn, nonEmptyAll)
import Data.Monoid (Monoid(..))
import Data.Monoid.Generic (def_mempty, def_mappend)
import Data.Store.Transaction (Transaction)
import GHC.Generics (Generic)
import Lamdu.Config (Config)
import Lamdu.Expr.IRef (DefI)
import Lamdu.Expr.Val (Val(..))
import Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), hiSearchTerm, hiMArgument, hiActiveId)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Sugar.AddNames.Types (Name(..), NameCollision(..))
import Lamdu.Sugar.Types (Scope(..))
import qualified Control.Lens as Lens
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Class as ListClass
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Lamdu.Config as Config
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Expr.Val as V
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

data GroupPrecedence = LowPrecedence | HighPrecedence
  deriving Eq
instance Monoid GroupPrecedence where
  mempty = LowPrecedence
  mappend LowPrecedence LowPrecedence = LowPrecedence
  mappend _ _ = HighPrecedence

data GroupAttributes = GroupAttributes
  { _searchTerms :: [String]
  , __precedence :: GroupPrecedence
  } deriving (Generic)
instance Monoid GroupAttributes where
  mempty = def_mempty
  mappend = def_mappend

data Group def = Group
  { _groupSearchTerms :: GroupAttributes
  , _groupBaseExpr :: Val ()
  }
type GroupM m = Group (DefI m)

Lens.makeLenses ''GroupAttributes
Lens.makeLenses ''Group

data ResultType = GoodResult | BadResult
  deriving (Eq, Ord)

data Result m = Result
  { rType :: ResultType
  , -- Warning: This transaction should be ran at most once!
    -- Running it more than once will cause inconsistencies.
    rHoleResult :: T m (Sugar.HoleResult (Name m) m)
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

getVarsToGroup :: (Sugar.GetVar (Name m) m, Val ()) -> Group def
getVarsToGroup (getVar, expr) = sugarNameToGroup (getVar ^. Sugar.gvName) expr

getParamsToGroup :: (Sugar.GetParams (Name m) m, Val ()) -> Group def
getParamsToGroup (getParams, expr) =
  sugarNameToGroup (getParams ^. Sugar.gpDefName) expr
  & groupSearchTerms <>~ GroupAttributes ["params"] HighPrecedence

sugarNameToGroup :: Name m -> Val () -> Group def
sugarNameToGroup (Name _ collision _ varName) expr = Group
  { _groupSearchTerms = GroupAttributes (varName : collisionStrs) HighPrecedence
  , _groupBaseExpr = expr
  }
  where
    collisionStrs =
      case collision of
      NoCollision -> []
      Collision suffix -> [show suffix]

prefixId :: HoleInfo m -> WidgetId.Id
prefixId holeInfo = mconcat [hiActiveId holeInfo, WidgetId.Id ["results"]]

typeCheckResults ::
  MonadA m => HoleInfo m ->
  Val () ->
  T m [(ResultType, T m (Sugar.HoleResult (Name m) m))]
typeCheckResults holeInfo expr =
  (hiActions holeInfo ^. Sugar.holeResults) expr
  & ListClass.toList
  <&> sortOn (^. _1)
  <&> Lens.mapped . _1 %~ checkGood
  where
    checkGood x = if x < [5] then GoodResult else BadResult

mResultsListOf ::
  HoleInfo m -> WidgetId.Id ->
  [(ResultType, T m (Sugar.HoleResult (Name m) m))] ->
  Maybe (ResultsList m)
mResultsListOf _ _ [] = Nothing
mResultsListOf holeInfo baseId (x:xs) = Just
  ResultsList
  { _rlPreferred = NotPreferred
  , _rlExtraResultsPrefixId = extraResultsPrefixId
  , _rlMain = mkResult (mconcat [prefixId holeInfo, baseId]) x
  , _rlExtra = zipWith mkExtra [(0::Int)..] xs
  }
  where
    mkExtra = mkResult . extraResultId
    extraResultId i = mappend extraResultsPrefixId $ WidgetIds.hash i
    extraResultsPrefixId = mconcat [prefixId holeInfo, WidgetId.Id ["extra results"], baseId]
    mkResult resultId (typ, holeResult) =
      Result
      { rType = typ
      , rHoleResult = holeResult
      , rId = resultId
      }

typeCheckToResultsList ::
  MonadA m => HoleInfo m ->
  WidgetId.Id -> Val () ->
  T m (Maybe (ResultsList m))
typeCheckToResultsList holeInfo baseId expr =
  mResultsListOf holeInfo baseId <$>
  typeCheckResults holeInfo expr

makeResultsList ::
  MonadA m => HoleInfo m -> GroupM m ->
  T m (Maybe (ResultsList m))
makeResultsList holeInfo group =
  typeCheckToResultsList holeInfo baseId baseExpr
  <&> Lens.mapped %~ rlPreferred .~ toPreferred
  where
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

makeAllGroups :: MonadA m => HoleInfo m -> T m [GroupM m]
makeAllGroups holeInfo = do
  Scope
    { _scopeLocals = locals
    , _scopeGlobals = globals
    , _scopeGetParams = getParams
    } <- hiActions holeInfo ^. Sugar.holeScope
  let
    allGroups =
      addSuggestedGroups $
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
    suggestedGroups =
      [ Group
        { _groupSearchTerms = GroupAttributes ["suggested"] HighPrecedence
        , _groupBaseExpr = suggestedVal
        }
      | Lens.nullOf ExprLens.valHole suggestedVal
      ] ++
      [ Group
        { _groupSearchTerms = GroupAttributes ["get field", "."] HighPrecedence
        , _groupBaseExpr =
            Val () $ V.BGetField $ V.GetField P.hole tag
        }
      | tag <-
          hiMArgument holeInfo
          ^.. Lens._Just
          . Sugar.haExpr . Sugar.rPayload . Sugar.plInferredType
          . ExprLens._TRecord . ExprLens.compositeTags
      ] ++
      [ Group
        { _groupSearchTerms = GroupAttributes ["apply"] HighPrecedence
        , _groupBaseExpr =
            Val () $ V.BApp $ V.Apply P.hole P.hole
        }
      | _ <- hiMArgument holeInfo ^.. Lens._Just
      ]
    addSuggestedGroups groups =
      let
        (dupsOfSuggested, others) =
          List.partition (V.alphaEq suggestedVal . (^. groupBaseExpr)) groups
        dupsGroupNames = dupsOfSuggested ^. Lens.traverse . groupSearchTerms
      in
        ( suggestedGroups & Lens.traverse . groupSearchTerms <>~ dupsGroupNames
        ) ++ others
    suggestedVal = hiSuggested holeInfo ^. Sugar.hsValue

primitiveGroups :: HoleInfo m -> [GroupM m]
primitiveGroups holeInfo =
  [ mkGroupBody HighPrecedence [searchTerm] $
    V.BLeaf $ V.LLiteralInteger $ read searchTerm
  | nonEmptyAll Char.isDigit searchTerm
  ] ++
  [ mkGroupBody LowPrecedence ["\\", "Lambda", "Λ", "λ"] $
    V.BAbs $ V.Lam "NewLambda" P.hole
  , mkGroupBody LowPrecedence ["Empty", "Record", "{}", "0", "Ø"] $
    V.BLeaf V.LRecEmpty
  ]
  where
    searchTerm = hiSearchTerm holeInfo
    mkGroupBody prec terms body = Group
      { _groupSearchTerms = GroupAttributes terms prec
      , _groupBaseExpr = Val () body
      }

preferFor :: String -> GroupAttributes -> Bool
preferFor searchTerm (GroupAttributes terms HighPrecedence) = searchTerm `elem` terms
preferFor _ _ = False

groupOrdering :: String -> GroupAttributes -> [Bool]
groupOrdering searchTerm (GroupAttributes terms precedence) =
  map not
  [ precedence == HighPrecedence
  , match (==)
  , match isPrefixOf
  , match insensitivePrefixOf
  , match isInfixOf
  ]
  where
    insensitivePrefixOf = isPrefixOf `on` map Char.toLower
    match f = any (f searchTerm) terms

holeMatches :: (a -> GroupAttributes) -> String -> [a] -> [a]
holeMatches getSearchTerms searchTerm =
  sortOn (groupOrdering searchTerm . getSearchTerms) .
  filter (nameMatch . getSearchTerms)
  where
    nameMatch (GroupAttributes _ LowPrecedence)
      | null searchTerm = False
    nameMatch (GroupAttributes terms _) =
      any (insensitiveInfixOf searchTerm) terms
    insensitiveInfixOf = isInfixOf `on` map Char.toLower
