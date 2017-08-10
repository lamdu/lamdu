{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, RecordWildCards, OverloadedStrings, TemplateHaskell #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups
    ( makeAll, HaveHiddenResults(..)
    , Result(..)
    , ResultsList(..), rlExtraResultsPrefixId, rlMain, rlExtra
    , prefixId
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.ListT (ListT)
import           Control.Monad.Transaction (MonadTransaction(..))
import qualified Data.ByteString.Char8 as BS8
import           Data.Function (on)
import           Data.Functor.Identity (Identity(..))
import           Data.List (sortOn)
import qualified Data.List.Class as ListClass
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Config as Config
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Formatting (Format(..))
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleInfo(..), hiSearchTerm)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.ValTerms as ValTerms
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

data Group m = Group
    { _groupSearchTerms :: [Text]
    , _groupId :: WidgetId.Id
    , _groupResults ::
        ListT (T m) (Sugar.HoleResultScore, T m (Sugar.HoleResult (Name m) m))
    }

Lens.makeLenses ''Group

data Result m = Result
    { rScore :: Sugar.HoleResultScore
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

prefixId :: HoleInfo m -> WidgetId.Id
prefixId = HoleWidgetIds.hidResultsPrefix . hiIds

mResultsListOf ::
    HoleInfo m -> WidgetId.Id ->
    [(Sugar.HoleResultScore, T m (Sugar.HoleResult (Name m) m))] ->
    Maybe (ResultsList m)
mResultsListOf _ _ [] = Nothing
mResultsListOf holeInfo baseId (x:xs) = Just
    ResultsList
    { _rlPreferred = NotPreferred
    , _rlExtraResultsPrefixId = extraResultsPrefixId
    , _rlMain = mkResult (prefixId holeInfo <> baseId) x
    , _rlExtra = zipWith mkExtra [(0::Int)..] xs
    }
    where
        mkExtra = mkResult . extraResultId
        extraResultId i = WidgetId.joinId extraResultsPrefixId [BS8.pack (show i)]
        extraResultsPrefixId = prefixId holeInfo <> WidgetId.Id ["extra results"] <> baseId
        mkResult resultId (score, holeResult) =
            Result
            { rScore = score
            , rHoleResult = holeResult
            , rId = resultId
            }

makeResultsList ::
    Monad m => HoleInfo m -> Group m ->
    T m (Maybe (ResultsList m))
makeResultsList holeInfo group =
    group ^. groupResults
    & ListClass.toList
    <&> sortOn (^. _1)
    <&> mResultsListOf holeInfo (group ^. groupId)
    <&> Lens.mapped %~ rlPreferred .~ toPreferred
    where
        toPreferred
            | [searchTerm] == group ^. groupSearchTerms = Preferred
            | otherwise = NotPreferred
        searchTerm = hiSearchTerm holeInfo

data HaveHiddenResults = HaveHiddenResults | NoHiddenResults

collectResults :: Monad m => Config.Hole -> ListT m (ResultsList f) -> m ([ResultsList f], HaveHiddenResults)
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
        resultsListScore x = (x ^. rlPreferred, rScore (x ^. rlMain))
        step results x =
            results
            & case (x ^. rlPreferred, isGoodResult (rScore (x ^. rlMain))) of
                (NotPreferred, False) -> _2
                _ -> _1
                %~ (x :)
        isGoodResult = (< [5])

makeAll ::
    (Monad n, MonadTransaction n m, MonadReader env m, Config.HasConfig env) =>
    HoleInfo n ->
    m ([ResultsList n], HaveHiddenResults)
makeAll holeInfo =
    do
        config <- Lens.view Config.config <&> Config.hole
        makeAllGroups holeInfo
            <&> ListClass.fromList
            <&> ListClass.mapL (makeResultsList holeInfo)
            <&> ListClass.catMaybes
            >>= collectResults config
            & transaction

mkGroupId :: Show a => Val a -> WidgetId.Id
mkGroupId option =
    option
    & ExprLens.valLeafs . V._LLiteral . V.primData .~ mempty
    & WidgetIds.hash

mkGroup :: Monad m => Sugar.HoleOption (Name m) m -> T m (Group m)
mkGroup option =
    do
        sugaredBaseExpr <- option ^. Sugar.hoSugaredBaseExpr
        pure Group
            { _groupSearchTerms = sugaredBaseExpr & ValTerms.expr
            , _groupResults = option ^. Sugar.hoResults
            , _groupId = mkGroupId (option ^. Sugar.hoVal)
            }

tryBuildLiteral ::
    (Format a, Monad m) => (Identity a -> Sugar.Literal Identity) -> HoleInfo m ->
    T m (Maybe (Sugar.HoleOption (Name m) m))
tryBuildLiteral mkLiteral holeInfo =
    hiSearchTerm holeInfo
    & tryParse
    <&> Identity
    <&> mkLiteral
    & Lens._Just %%~ hiHole holeInfo ^. Sugar.holeActions . Sugar.holeOptionLiteral

literalGroups :: Monad m => HoleInfo m -> T m [Sugar.HoleOption (Name m) m]
literalGroups holeInfo =
    [ tryBuildLiteral Sugar.LiteralNum holeInfo
    , tryBuildLiteral Sugar.LiteralBytes holeInfo
    , tryBuildLiteral Sugar.LiteralText holeInfo
    ] & sequenceA <&> (^.. Lens.traverse . Lens._Just)

insensitivePrefixOf :: Text -> Text -> Bool
insensitivePrefixOf = Text.isPrefixOf `on` Text.toLower

infixAltOf :: Text -> Text -> Bool
infixAltOf needle haystack =
    mapM alts (Text.unpack haystack)
    <&> concat
    <&> Text.pack
    & any (Text.isInfixOf needle)
    where
        alts x = [x] : extras x
        extras '≥' = [">="]
        extras '≤' = ["<="]
        extras '≠' = ["/=", "!=", "<>"]
        extras '⋲' = ["<{"]
        extras 'α' = ["alpha"]
        extras 'β' = ["beta"]
        extras _ = []

insensitiveInfixAltOf :: Text -> Text -> Bool
insensitiveInfixAltOf = infixAltOf `on` Text.toLower

makeAllGroups :: Monad m => HoleInfo m -> T m [Group m]
makeAllGroups holeInfo =
    (++)
    <$> (literalGroups holeInfo >>= mapM mkGroup)
    <*> (hiHole holeInfo ^. Sugar.holeActions . Sugar.holeOptions
         >>= mapM mkGroup
         <&> holeMatches (hiSearchTerm holeInfo))

groupOrdering :: Text -> Group m -> [Bool]
groupOrdering searchTerm group =
    map not
    [ null (group ^. groupSearchTerms)
    , match (==)
    , match Text.isPrefixOf
    , match insensitivePrefixOf
    , match Text.isInfixOf
    ]
    where
        match f = any (f searchTerm) (group ^. groupSearchTerms)

holeMatches :: Text -> [Group m] -> [Group m]
holeMatches searchTerm groups =
    groups
    & filterBySearchTerm
    & sortOn (groupOrdering searchTerm)
    where
        filterBySearchTerm
            | Text.null searchTerm = id
            | otherwise = filter nameMatch
        nameMatch group = any (insensitiveInfixAltOf searchTerm) (group ^. groupSearchTerms)
