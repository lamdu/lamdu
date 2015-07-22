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
import           Data.Traversable (sequenceA)
import qualified Graphics.UI.Bottle.WidgetId as WidgetId
import qualified Lamdu.Config as Config
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

data Group m = Group
    { _groupSearchTerms :: [String]
    , _groupId :: WidgetId.Id
    , _groupResults ::
        ListT (T m) (Sugar.HoleResultScore, T m (Sugar.HoleResult (Name m) m))
    }

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

makeResultsList ::
    MonadA m => EditableHoleInfo m -> Group m ->
    T m (Maybe (ResultsList m))
makeResultsList holeInfo group =
    group ^. groupResults
    & ListClass.toList
    <&> sortOn (^. _1)
    <&> Lens.mapped . _1 %~ checkGood
    <&> mResultsListOf holeInfo (group ^. groupId)
    <&> Lens.mapped %~ rlPreferred .~ toPreferred
    where
        checkGood x
            | x < [5] = GoodResult
            | otherwise = BadResult
        toPreferred
            | searchTerm `elem` (group ^. groupSearchTerms) = Preferred
            | otherwise = NotPreferred
        searchTerm = ehiSearchTerm holeInfo

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
    Sugar.Case Sugar.LambdaCase [] Sugar.ClosedCase{} _ _ -> ["absurd", "case", ":"]
    _ -> ["case", ":"]
searchTermsOfBody Sugar.BodyInject {} = ["inject", "[]"]
searchTermsOfBody Sugar.BodyToNom {} = []
searchTermsOfBody Sugar.BodyFromNom {} = []
searchTermsOfBody (Sugar.BodyLiteralInteger i) = [show i]
searchTermsOfBody Sugar.BodyHole {} = []
searchTermsOfBody Sugar.BodyGetVar {} = []

mkGroup :: MonadA m => Sugar.HoleOption (Name m) m -> T m (Group m)
mkGroup option =
    do
        sugaredBaseExpr <- option ^. Sugar.hoSugaredBaseExpr
        let searchTerms =
                (NamesGet.fromExpression sugaredBaseExpr <&> searchTermOfName)
                ++ concatMap searchTermsOfBody
                (sugaredBaseExpr ^..
                 SugarLens.subExprPayloads . Lens.asIndex . Sugar.rBody)
        pure Group
            { _groupSearchTerms = searchTerms
            , _groupResults = option ^. Sugar.hoResults
            , _groupId = WidgetIds.hash (option ^. Sugar.hoVal)
            }

literalIntGroups :: MonadA m => EditableHoleInfo m -> T m [Sugar.HoleOption (Name m) m]
literalIntGroups holeInfo =
    [ read searchTerm
      & ehiActions holeInfo ^. Sugar.holeOptionLiteralInt
    | nonEmptyAll Char.isDigit searchTerm
    ] & sequenceA
    where
        searchTerm = ehiSearchTerm holeInfo

makeAllGroups :: MonadA m => EditableHoleInfo m -> T m [Group m]
makeAllGroups editableHoleInfo =
    (++)
    <$> literalIntGroups editableHoleInfo
    <*> ehiActions editableHoleInfo ^. Sugar.holeOptions
    >>= mapM mkGroup
    <&> holeMatches (ehiSearchTerm editableHoleInfo)

groupOrdering :: String -> Group m -> [Bool]
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

holeMatches :: String -> [Group m] -> [Group m]
holeMatches searchTerm =
    sortOn (groupOrdering searchTerm) .
    filter nameMatch
    where
        nameMatch group = any (insensitiveInfixOf searchTerm) (group ^. groupSearchTerms)
        insensitiveInfixOf = isInfixOf `on` map Char.toLower
