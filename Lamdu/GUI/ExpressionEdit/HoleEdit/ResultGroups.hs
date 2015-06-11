{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell, FlexibleContexts, DeriveGeneric #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups
    ( makeAll, HaveHiddenResults(..)
    , Result(..)
    , ResultsList(..), rlExtraResultsPrefixId, rlMain, rlExtra
    , prefixId
    ) where

import           Control.Applicative (Applicative(..), (<$>))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad.ListT (ListT)
import           Control.MonadA (MonadA)
import qualified Data.Char as Char
import           Data.Function (on)
import           Data.List (isInfixOf, isPrefixOf)
import qualified Data.List as List
import qualified Data.List.Class as ListClass
import           Data.List.Utils (sortOn, nonEmptyAll)
import           Data.Monoid (Monoid(..), (<>))
import           Data.Monoid.Generic (def_mempty, def_mappend)
import           Data.Store.Transaction (Transaction)
import           GHC.Generics (Generic)
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Lamdu.Config as Config
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.Expr.Pure as P
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), EditableHoleInfo(..), ehiSearchTerm, hiMArgument)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..), NameCollision(..))
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
    { _groupAttributes :: GroupAttributes
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

getVarToGroup :: Sugar.ScopeGetVar (Name n) m -> Group def
getVarToGroup (Sugar.ScopeGetVar (Sugar.GetVarNamed namedVar) expr) =
    sugarNameToGroup (namedVar ^. Sugar.nvName) expr
getVarToGroup (Sugar.ScopeGetVar (Sugar.GetVarParamsRecord paramsRecord) expr) =
    sugarNamesToGroup (paramsRecord ^. Sugar.prvFieldNames) expr
    & groupAttributes . searchTerms <>~ ["params", "record"]

sugarNameToGroup :: Name m -> Val () -> Group def
sugarNameToGroup name = sugarNamesToGroup [name]

sugarNamesToGroup :: [Name m] -> Val () -> Group def
sugarNamesToGroup names expr = Group
    { _groupAttributes = GroupAttributes (concatMap mkSearchTerms names)  HighPrecedence
    , _groupBaseExpr = expr
    }
    where
        mkSearchTerms (Name _ NoCollision _ varName) = [varName]
        mkSearchTerms (Name _ (Collision suffix) _ varName) = [varName ++ show suffix]

prefixId :: HoleInfo m -> WidgetId.Id
prefixId = HoleWidgetIds.hidResultsPrefix . hiIds

typeCheckResults ::
    MonadA m => EditableHoleInfo m ->
    Val () ->
    T m [(ResultType, T m (Sugar.HoleResult (Name m) m))]
typeCheckResults holeInfo expr =
    (ehiActions holeInfo ^. Sugar.holeResults) expr
    & ListClass.toList
    <&> sortOn (^. _1)
    <&> Lens.mapped . _1 %~ checkGood
    where
        checkGood x = if x < [5] then GoodResult else BadResult

mResultsListOf ::
    EditableHoleInfo m -> WidgetId.Id ->
    [(ResultType, T m (Sugar.HoleResult (Name m) m))] ->
    Maybe (ResultsList m)
mResultsListOf _ _ [] = Nothing
mResultsListOf holeInfo baseId (x:xs) = Just
    ResultsList
    { _rlPreferred = NotPreferred
    , _rlExtraResultsPrefixId = extraResultsPrefixId
    , _rlMain = mkResult (prefix <> baseId) x
    , _rlExtra = zipWith mkExtra [(0::Int)..] xs
    }
    where
        prefix = prefixId (ehiInfo holeInfo)
        mkExtra = mkResult . extraResultId
        extraResultId i = mappend extraResultsPrefixId $ WidgetIds.hash i
        extraResultsPrefixId = prefix <> WidgetId.Id ["extra results"] <> baseId
        mkResult resultId (typ, holeResult) =
            Result
            { rType = typ
            , rHoleResult = holeResult
            , rId = resultId
            }

typeCheckToResultsList ::
    MonadA m => EditableHoleInfo m ->
    WidgetId.Id -> Val () ->
    T m (Maybe (ResultsList m))
typeCheckToResultsList holeInfo baseId expr =
    mResultsListOf holeInfo baseId <$>
    typeCheckResults holeInfo expr

makeResultsList ::
    MonadA m => EditableHoleInfo m -> GroupM m ->
    T m (Maybe (ResultsList m))
makeResultsList holeInfo group =
    typeCheckToResultsList holeInfo baseId baseExpr
    <&> Lens.mapped %~ rlPreferred .~ toPreferred
    where
        toPreferred
            | Lens.anyOf groupAttributes (preferFor searchTerm) group = Preferred
            | otherwise = NotPreferred
        searchTerm = ehiSearchTerm holeInfo
        baseExpr = group ^. groupBaseExpr
        baseId = WidgetIds.hash baseExpr

data HaveHiddenResults = HaveHiddenResults | NoHiddenResults

collectResults :: MonadA m => Config.Hole -> ListT m (ResultsList f) -> m ([ResultsList f], HaveHiddenResults)
collectResults Config.Hole{..} resultsM =
    do
        (collectedResults, remainingResultsM) <-
            ListClass.splitWhenM (return . (>= holeResultCount) . length . fst) $
            ListClass.scanl step ([], []) resultsM
        remainingResults <- ListClass.toList $ ListClass.take 2 remainingResultsM
        let results =
                last (collectedResults ++ remainingResults)
                & Lens.both %~ reverse
        results
            & _1 %~ sortOn resultsListScore
            & uncurry (++)
            & splitAt holeResultCount
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
    MonadA m => EditableHoleInfo m ->
    ExprGuiM m ([ResultsList m], HaveHiddenResults)
makeAll holeInfo =
    do
        config <- ExprGuiM.readConfig <&> Config.hole
        makeAllGroups holeInfo
            <&> ListClass.fromList
            <&> ListClass.mapL (makeResultsList holeInfo)
            <&> ListClass.catMaybes
            >>= collectResults config
            & ExprGuiM.transaction

getVarTypesOrder :: [Sugar.NamedVarType]
getVarTypesOrder =
    [ Sugar.GetParameter
    , Sugar.GetFieldParameter
    , Sugar.GetDefinition
    ]

makeAllGroups :: MonadA m => EditableHoleInfo m -> T m [GroupM m]
makeAllGroups editableHoleInfo =
    do
        scopeGetVars <- ehiActions editableHoleInfo ^. Sugar.holeScope
        let allGroups =
                addSuggestedGroups $
                primitiveGroups editableHoleInfo ++
                concat
                [ scopeGetVars
                    & filter ((Just nvType ==) . (^? Sugar.sgvGetVar . Sugar._GetVarNamed . Sugar.nvVarType))
                    & sortedGetVarGroups
                | nvType <- getVarTypesOrder
                ] ++
                ( scopeGetVars
                    & filter (Lens.has (Sugar.sgvGetVar . Sugar._GetVarParamsRecord))
                    & sortedGetVarGroups
                )
        pure $ holeMatches (^. groupAttributes) (ehiSearchTerm editableHoleInfo) allGroups
    where
        sortedGetVarGroups getVars =
            getVars
            & map getVarToGroup
            & sortOn (^. groupAttributes . searchTerms)
        holeInfo = ehiInfo editableHoleInfo
        suggestedVal = hiSuggested holeInfo
        suggestedGroups =
            [ Group
                { _groupAttributes = GroupAttributes ["suggested"] HighPrecedence
                , _groupBaseExpr = suggestedVal
                }
            | Lens.nullOf ExprLens.valHole suggestedVal
            ] ++
            [ Group
                { _groupAttributes = GroupAttributes ["get field", "."] HighPrecedence
                , _groupBaseExpr =
                        Val () $ V.BGetField $ V.GetField P.hole tag
                }
            | tag <-
                    hiMArgument holeInfo
                    ^.. Lens._Just
                    . Sugar.haExpr . Sugar.rPayload . Sugar.plAnnotation . Sugar.aInferredType
                    . ExprLens._TRecord . ExprLens.compositeTags
            ] ++
            [ Group
                { _groupAttributes = GroupAttributes ["apply"] HighPrecedence
                , _groupBaseExpr =
                        Val () $ V.BApp $ V.Apply P.hole P.hole
                }
            | _ <- hiMArgument holeInfo ^.. Lens._Just
            ]
        addSuggestedGroups groups =
            let (dupsOfSuggested, others) =
                    List.partition (V.alphaEq suggestedVal . (^. groupBaseExpr)) groups
                dupsGroupNames = dupsOfSuggested ^. Lens.traverse . groupAttributes
            in  ( suggestedGroups & Lens.traverse . groupAttributes <>~ dupsGroupNames
                ) ++ others

primitiveGroups :: EditableHoleInfo m -> [GroupM m]
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
        searchTerm = ehiSearchTerm holeInfo
        mkGroupBody prec terms body = Group
            { _groupAttributes = GroupAttributes terms prec
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
