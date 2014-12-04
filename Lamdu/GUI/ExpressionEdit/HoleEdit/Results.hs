{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts, DeriveGeneric #-}
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
import Control.Monad.Trans.State (StateT(..))
import Control.MonadA (MonadA)
import Data.Function (on)
import Data.List (isInfixOf, isPrefixOf, partition)
import Data.List.Utils (sortOn, nonEmptyAll)
import Data.Maybe (catMaybes)
import Data.Maybe.Utils (maybeToMPlus)
import Data.Monoid (Monoid(..))
import Data.Monoid.Generic (def_mempty, def_mappend)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse, sequenceA)
import GHC.Generics (Generic)
import Lamdu.Config (Config)
import Lamdu.Expr.IRef (DefIM)
import Lamdu.Expr.Type (Type)
import Lamdu.Expr.Val (Val(..))
import Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), hiSearchTerm, hiMArgument, hiActiveId)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Sugar.AddNames.Types (Name(..), NameCollision(..))
import Lamdu.Sugar.Types (Scope(..))
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Class as ListClass
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Lamdu.Config as Config
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Pure as P
import qualified Lamdu.Expr.Type as T
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
type GroupM m = Group (DefIM m)

Lens.makeLenses ''GroupAttributes
Lens.makeLenses ''Group

data ResultType = GoodResult | BadResult
  deriving (Eq, Ord)

type SugarExprPl = (ExprGuiM.StoredEntityIds, ExprGuiM.Injected)

data Result m = Result
  { rType :: ResultType
  , rHoleResult :: Sugar.HoleResult (Name m) m SugarExprPl
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
  Val (Sugar.MStorePoint m SugarExprPl) ->
  T m (Maybe (ResultType, Sugar.HoleResult (Name m) m SugarExprPl))
typeCheckHoleResult holeInfo val = do
  mGood <- mkHoleResult val
  case mGood of
    Just good -> pure $ Just (GoodResult, good)
    Nothing ->
      fmap ((,) BadResult) <$>
      (mkHoleResult . storePointHoleWrap) val
  where
    mkHoleResult = Sugar.holeResult (hiActions holeInfo)

typeCheckResults ::
  MonadA m => HoleInfo m ->
  [Val (Sugar.MStorePoint m SugarExprPl)] ->
  T m [(ResultType, Sugar.HoleResult (Name m) m SugarExprPl)]
typeCheckResults holeInfo options = do
  rs <-
    options
    & traverse (typeCheckHoleResult holeInfo)
    <&> catMaybes
  let (goodResults, badResults) = partition ((== GoodResult) . fst) rs
  return $ sorted goodResults ++ sorted badResults
  where
    sorted = sortOn (^. _2 . Sugar.holeResultComplexityScore)

mResultsListOf ::
  HoleInfo m -> WidgetId.Id ->
  [(ResultType, Sugar.HoleResult (Name m) m SugarExprPl)] ->
  Maybe (ResultsList m)
mResultsListOf _ _ [] = Nothing
mResultsListOf holeInfo baseId (x:xs) = Just
  ResultsList
  { _rlPreferred = NotPreferred
  , _rlExtraResultsPrefixId = extraResultsPrefixId
  , _rlMain = mkResult (mconcat [prefixId holeInfo, baseId]) x
  , _rlExtra = map mkExtra xs
  }
  where
    mkExtra res@(_resultType, holeRes) = mkResult (extraResultId holeRes) res
    extraResultId holeRes =
      mappend extraResultsPrefixId $ WidgetIds.fromEntityId $
      holeRes ^. Sugar.holeResultConverted . Sugar.rPayload . Sugar.plEntityId
    extraResultsPrefixId = mconcat [prefixId holeInfo, WidgetId.Id ["extra results"], baseId]
    mkResult resultId (typ, holeResult) =
      Result
      { rType = typ
      , rHoleResult = holeResult
      , rId = resultId
      }

typeCheckToResultsList ::
  MonadA m => HoleInfo m ->
  WidgetId.Id -> [Val (Sugar.MStorePoint m SugarExprPl)] ->
  T m (Maybe (ResultsList m))
typeCheckToResultsList holeInfo baseId options =
  mResultsListOf holeInfo baseId <$>
  typeCheckResults holeInfo options

-- Modified from Lamdu.Suggest to be non-recursive on the field
-- values. TODO: Is code sharing possible?
suggestRecord :: Monoid a => T.Composite T.Product -> Val a
suggestRecord T.CVar{}          = P.hole
suggestRecord T.CEmpty          = P.recEmpty
suggestRecord (T.CExtend f _ r) = P.recExtend f P.hole $ suggestRecord r

applyForms :: Monoid a => Type -> Val a -> [Val a]
applyForms typ base =
  base : case typ of
         T.TFun paramTyp res -> applyForms res (addApply paramTyp base)
         _ -> []
  where
    suggestArg (T.TRecord prod) = suggestRecord prod
    suggestArg _ = P.hole
    addApply paramTyp x = P.app x $ suggestArg paramTyp

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
  map fst . filter snd . (`runStateT` False) . go
  where
    go oldVal@(Val x body) = do
      alreadyReplaced <- State.get
      if alreadyReplaced
        then return (pure oldVal)
        else
          case body of
          V.BLeaf V.LHole ->
            join $ lift
              [ do
                  State.put True
                  return $ replaceHole x
              , return (pure oldVal)
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
    traverse (injectIntoHoles (hiActions holeInfo) arg . (pl False <$))
    where
      pl isInjected = (ExprGuiM.StoredEntityIds [], ExprGuiM.Injected [isInjected])
      arg =
        holeArg ^. Sugar.haExprPresugared
        <&> _2 .~ pl False
        & V.payload . _2 .~ pl True

makeResultsList ::
  MonadA m => HoleInfo m -> GroupM m ->
  T m (Maybe (ResultsList m))
makeResultsList holeInfo group =
  baseExprWithApplyForms holeInfo baseExpr
  >>= maybeInjectArgumentExpr holeInfo
  >>= typeCheckToResultsList holeInfo baseId
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
  , mkGroupBody LowPrecedence ["Empty", "Record", "{"] $
    V.BLeaf V.LRecEmpty
  , mkGroupBody LowPrecedence ["Extend", "Record", "{"] $
    V.BRecExtend $ V.RecExtend newTag P.hole P.hole
  ]
  where
    newTag = hiActions holeInfo ^. Sugar.holeResultNewTag
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
