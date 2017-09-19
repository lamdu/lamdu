{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, FlexibleContexts, OverloadedStrings, NamedFieldPuns, DisambiguateRecordFields, DeriveTraversable #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups
    ( makeAll
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
import qualified GUI.Momentu.Widgets.Menu as Menu
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
        ListT (T m)
        ( Sugar.HoleResultScore
        , T m (Sugar.HoleResult m (Sugar.Expression (Name m) m ()))
        )
    }

Lens.makeLenses ''Group

data Result m = Result
    { _rScore :: Sugar.HoleResultScore
    , -- Warning: This transaction should be ran at most once!
      -- Running it more than once will cause inconsistencies.
      rHoleResult :: T m (Sugar.HoleResult m (Sugar.Expression (Name m) m ()))
    , rId :: WidgetId.Id
    }
Lens.makeLenses ''Result

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
    [(Sugar.HoleResultScore, T m (Sugar.HoleResult m (Sugar.Expression (Name m) m ())))] ->
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
            { _rScore = score
            , rHoleResult = holeResult
            , rId = resultId
            }

makeResultsList ::
    Monad m => HoleInfo m -> Group m ->
    T m (Maybe (ResultsList m))
makeResultsList holeInfo group =
    group ^. groupResults
    & ListClass.toList
    <&> sortOn fst
    <&> mResultsListOf holeInfo (group ^. groupId)
    <&> Lens.mapped %~ rlPreferred .~ toPreferred
    where
        toPreferred
            | [searchTerm] == group ^. groupSearchTerms = Preferred
            | otherwise = NotPreferred
        searchTerm = hiSearchTerm holeInfo

data GoodAndBad a = GoodAndBad { _good :: a, _bad :: a }
    deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''GoodAndBad

collectResults :: Monad m => Config.Hole -> ListT m (ResultsList f) -> m ([ResultsList f], Menu.HasMoreOptions)
collectResults Config.Hole{holeResultCount} resultsM =
    do
        (tooFewGoodResults, moreResultsM) <-
            ListClass.scanl prependResult (GoodAndBad [] []) resultsM
            & ListClass.splitWhenM (return . (>= holeResultCount) . length . _good)

        -- We need 2 of the moreResultsM:
        -- A. First is needed because it would be the first to have the correct
        --    number of good results
        -- B. Second is needed just to determine if there are any
        --    remaining results beyond it
        moreResults <- ListClass.toList $ ListClass.take 2 moreResultsM

        let results =
                last (tooFewGoodResults ++ moreResults)
        results
            & bad %~ reverse -- to reverse the prepended order
            & good %~ sortOn resultsListScore
            & concatBothGoodAndBad
            -- Re-split because now that we've added all the
            -- accumulated bad results we may have too many
            & splitAt holeResultCount
            & _2 %~ haveHiddenResults
            & return
    where
        concatBothGoodAndBad goodAndBad = goodAndBad ^. Lens.folded
        haveHiddenResults [] = Menu.NoMoreOptions
        haveHiddenResults _ = Menu.MoreOptionsAvailable
        resultsListScore x = (x ^. rlPreferred, x ^. rlMain . rScore . Sugar.hrsGoodResult)
        prependResult results x =
            results
            & case (x ^. rlPreferred, x ^. rlMain . rScore . Sugar.hrsGoodResult) of
                (NotPreferred, Sugar.BadResult) -> bad
                _ -> good
                %~ (x :)

makeAll ::
    (MonadTransaction n m, MonadReader env m, Config.HasConfig env) =>
    HoleInfo n ->
    m ([ResultsList n], Menu.HasMoreOptions)
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

mkGroup :: Monad m => Sugar.HoleOption m (Sugar.Expression (Name m) m ()) -> T m (Group m)
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
    Maybe (T m (Sugar.HoleOption m (Sugar.Expression (Name m) m ())))
tryBuildLiteral mkLiteral holeInfo =
    mkHoleOption <*> literal
    where
        literal =
            hiSearchTerm holeInfo
            & tryParse
            <&> Identity
            <&> mkLiteral
        mkHoleOption =
            hiHole holeInfo ^? Sugar.holeKind . Sugar._LeafHole .
            Sugar.holeOptionLiteral

literalGroups :: Monad m => HoleInfo m -> [T m (Sugar.HoleOption m (Sugar.Expression (Name m) m ()))]
literalGroups holeInfo =
    [ tryBuildLiteral Sugar.LiteralNum holeInfo
    , tryBuildLiteral Sugar.LiteralBytes holeInfo
    , tryBuildLiteral Sugar.LiteralText holeInfo
    ] ^.. Lens.traverse . Lens._Just

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
    <$> (literalGroups holeInfo & sequenceA >>= mapM mkGroup)
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
