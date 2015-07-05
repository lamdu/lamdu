{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell #-}
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
import qualified Data.List.Class as ListClass
import           Data.List.Utils (sortOn, nonEmptyAll)
import           Data.Monoid (Monoid(..), (<>))
import           Data.Store.Transaction (Transaction)
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Lamdu.Config as Config
import           Lamdu.Expr.IRef (DefI)
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Expr.Val (Val(..))
import qualified Lamdu.Expr.Val as V
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), EditableHoleInfo(..), ehiSearchTerm)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Names.Get as NamesGet
import           Lamdu.Sugar.Names.Types (Name(..), NameCollision(..))
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

data Group def = Group
    { _groupSearchTerms :: [String]
    , _groupBaseExpr :: Val ()
    }
type GroupM m = Group (DefI m)

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

searchTermOfName :: Name m -> String
searchTermOfName (Name _ NoCollision _ varName) = varName
searchTermOfName (Name _ (Collision suffix) _ varName) = varName ++ show suffix

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
            | searchTerm `elem` (group ^. groupSearchTerms) = Preferred
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

searchTermsOfBody :: Sugar.Body (Name m) m expr -> [String]
searchTermsOfBody Sugar.BodyLam {} = ["lambda", "\\", "Λ", "λ"]
searchTermsOfBody Sugar.BodyApply {} = ["Apply"]
searchTermsOfBody Sugar.BodyList {} = ["list", "[]"]
searchTermsOfBody (Sugar.BodyRecord rec) =
    ["record", "{}"] ++
    do
        Sugar.Record [] Sugar.ClosedRecord{} _ <- [rec]
        ["Empty", "0", "Ø"]
searchTermsOfBody (Sugar.BodyGetField gf) =
    [".", "field", "." ++ searchTermOfName (gf ^. Sugar.gfTag . Sugar.tagGName)]
searchTermsOfBody (Sugar.BodyCase cas) =
    case cas of
    Sugar.Case Sugar.LambdaCase [] Sugar.ClosedCase{} _ _ -> ["absurd"]
    _ -> ["case", ":"]
searchTermsOfBody Sugar.BodyInject {} = ["inject", "[]"]
searchTermsOfBody Sugar.BodyToNom {} = []
searchTermsOfBody Sugar.BodyFromNom {} = []
searchTermsOfBody (Sugar.BodyLiteralInteger i) = [show i]
searchTermsOfBody Sugar.BodyHole {} = []
searchTermsOfBody Sugar.BodyGetVar {} = []

mkGroup :: MonadA m => Sugar.HoleOption (Name m) m -> T m (Group def)
mkGroup suggested =
    do
        sugaredBaseExpr <- suggested ^. Sugar.hsSugaredBaseExpr
        let searchTerms =
                (NamesGet.fromExpression sugaredBaseExpr <&> searchTermOfName)
                ++ concatMap searchTermsOfBody
                (sugaredBaseExpr ^..
                 SugarLens.subExprPayloads . Lens.asIndex . Sugar.rBody)
        pure Group
            { _groupSearchTerms = searchTerms
            , _groupBaseExpr = suggested ^. Sugar.hsVal
            }

addSuggestedGroups ::
    MonadA m => HoleInfo m ->
    [Sugar.HoleOption (Name m) m] ->
    [Sugar.HoleOption (Name m) m]
addSuggestedGroups holeInfo options =
    suggesteds ++ filter (not . equivalentToSuggested) options
    where
        suggesteds =
            hiHole holeInfo ^. Sugar.holeSuggesteds
            & filter (Lens.nullOf (Sugar.hsVal . ExprLens.valHole))
        equivalentToSuggested x =
            any (V.alphaEq (x ^. Sugar.hsVal)) (suggesteds ^.. Lens.traverse . Sugar.hsVal)

literalIntGroups :: EditableHoleInfo m -> [GroupM m]
literalIntGroups holeInfo =
    [ Group
      { _groupSearchTerms = [searchTerm]
      , _groupBaseExpr = Val () $ V.BLeaf $ V.LLiteralInteger $ read searchTerm
      }
    | nonEmptyAll Char.isDigit searchTerm
    ]
    where
        searchTerm = ehiSearchTerm holeInfo

makeAllGroups :: MonadA m => EditableHoleInfo m -> T m [GroupM m]
makeAllGroups editableHoleInfo =
    hiHole holeInfo ^. Sugar.holeOptions
    & addSuggestedGroups holeInfo
    & mapM mkGroup
    <&> (literalIntGroups editableHoleInfo ++)
    <&> holeMatches (ehiSearchTerm editableHoleInfo)
    where
        holeInfo = ehiInfo editableHoleInfo

groupOrdering :: String -> Group def -> [Bool]
groupOrdering searchTerm group =
    map not
    [ match (==)
    , match isPrefixOf
    , match insensitivePrefixOf
    , match isInfixOf
    ]
    where
        insensitivePrefixOf = isPrefixOf `on` map Char.toLower
        match f = any (f searchTerm) (group ^. groupSearchTerms)

holeMatches :: String -> [Group def] -> [Group def]
holeMatches searchTerm =
    sortOn (groupOrdering searchTerm) .
    filter nameMatch
    where
        nameMatch group = any (insensitiveInfixOf searchTerm) (group ^. groupSearchTerms)
        insensitiveInfixOf = isInfixOf `on` map Char.toLower
