{-# LANGUAGE LambdaCase, NoImplicitPrelude, FlexibleContexts, RecordWildCards, OverloadedStrings, TemplateHaskell #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups
    ( makeAll, HaveHiddenResults(..)
    , Result(..)
    , ResultsList(..), rlExtraResultsPrefixId, rlMain, rlExtra
    , prefixId
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad.ListT (ListT)
import           Control.MonadA (MonadA)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char as Char
import           Data.Function (on)
import           Data.List (isInfixOf, isPrefixOf)
import qualified Data.List.Class as ListClass
import           Data.List.Utils (sortOn, nonEmptyAll)
import           Data.Monoid ((<>))
import           Data.Store.Transaction (Transaction)
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
        extraResultId i = WidgetId.joinId extraResultsPrefixId [BS8.pack (show i)]
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
            | [searchTerm] == group ^. groupSearchTerms = Preferred
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

searchTermsOfBodyShape :: Sugar.Body (Name m) m expr -> [String]
searchTermsOfBodyShape = \case
    Sugar.BodyLam {} -> ["lambda", "\\", "Λ", "λ"]
    Sugar.BodyApply {} -> ["Apply"]
    Sugar.BodyList {} -> ["list", "[]"]
    (Sugar.BodyRecord r) ->
        ["record", "{}", "()"] ++
        case r of
        Sugar.Record [] Sugar.ClosedRecord{} _ -> ["empty"]
        _ -> []
    (Sugar.BodyGetField gf) ->
        [".", "field", "." ++ searchTermOfName (gf ^. Sugar.gfTag . Sugar.tagGName)]
    (Sugar.BodyCase cas) ->
        ["case", ":"] ++
        case cas of
            Sugar.Case Sugar.LambdaCase [] Sugar.ClosedCase{} _ _ -> ["absurd"]
            _ -> []
    Sugar.BodyInject {} -> ["inject", "[]"]
    (Sugar.BodyLiteralInteger i) -> [show i]
    Sugar.BodyGetVar Sugar.GetVarParamsRecord {} -> ["Params"]
    Sugar.BodyGetVar {} -> []
    Sugar.BodyToNom {} -> []
    Sugar.BodyFromNom {} -> []
    Sugar.BodyHole {} -> []

searchTermsOfBodyNames :: MonadA m => Sugar.Body (Name m) m expr -> [String]
searchTermsOfBodyNames = \case
    Sugar.BodyGetVar Sugar.GetVarParamsRecord {} -> []
    Sugar.BodyLam {} -> []
    body -> NamesGet.fromBody body <&> searchTermOfName

searchTermsOfBody :: MonadA m => Sugar.Body (Name m) m expr -> [String]
searchTermsOfBody = searchTermsOfBodyShape <> searchTermsOfBodyNames

mkGroup :: MonadA m => Sugar.HoleOption (Name m) m -> T m (Group m)
mkGroup option =
    do
        sugaredBaseExpr <- option ^. Sugar.hoSugaredBaseExpr
        let searchTerms =
                concatMap searchTermsOfBody
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
    [ null (group ^. groupSearchTerms)
    , match (==)
    , match isPrefixOf
    , match insensitivePrefixOf
    , match isInfixOf
    ]
    where
        insensitivePrefixOf = isPrefixOf `on` map Char.toLower
        match f = any (f searchTerm) (group ^. groupSearchTerms)

holeMatches :: String -> [Group m] -> [Group m]
holeMatches searchTerm groups =
    groups
    & filterBySearchTerm
    & sortOn (groupOrdering searchTerm)
    where
        filterBySearchTerm
            | null searchTerm = id
            | otherwise = filter nameMatch
        nameMatch group = any (insensitiveInfixOf searchTerm) (group ^. groupSearchTerms)
        insensitiveInfixOf = isInfixOf `on` map Char.toLower
