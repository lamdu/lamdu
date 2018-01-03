{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, FlexibleContexts, OverloadedStrings, NamedFieldPuns, DisambiguateRecordFields, DeriveTraversable #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups
    ( makeAll
    , Result(..)
    , ResultsList(..), rlExtraResultsPrefixId, rlMain, rlExtra
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
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified Lamdu.Calc.Val as V
import           Lamdu.Calc.Val.Annotated (Val)
import qualified Lamdu.Config as Config
import qualified Lamdu.Expr.Lens as ExprLens
import           Lamdu.Formatting (Format(..))
import           Lamdu.Name (Name)
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.ValTerms as ValTerms
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import           Lamdu.GUI.ExpressionGui (ExpressionN)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

data Group m = Group
    { _groupSearchTerms :: [Text]
    , _groupId :: WidgetId.Id
    , _groupResults ::
        ListT m
        ( Sugar.HoleResultScore
        , m (Sugar.HoleResult m (Sugar.Expression (Name m) m ()))
        )
    }
Lens.makeLenses ''Group

data Result m = Result
    { _rScore :: Sugar.HoleResultScore
    , -- Warning: This transaction should be ran at most once!
      -- Running it more than once will cause inconsistencies.
      rHoleResult :: m (Sugar.HoleResult m (Sugar.Expression (Name m) m ()))
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

mResultsListOf ::
    WidgetIds -> WidgetId.Id ->
    [ ( Sugar.HoleResultScore
      , m (Sugar.HoleResult m (Sugar.Expression (Name m) m ()))
      )
    ] ->
    Maybe (ResultsList m)
mResultsListOf _ _ [] = Nothing
mResultsListOf widgetIds baseId (x:xs) = Just
    ResultsList
    { _rlPreferred = NotPreferred
    , _rlExtraResultsPrefixId = extraResultsPrefixId
    , _rlMain = mkResult (prefixId <> baseId) x
    , _rlExtra = zipWith mkExtra [(0::Int)..] xs
    }
    where
        prefixId = hidResultsPrefix widgetIds
        mkExtra = mkResult . extraResultId
        extraResultId i = WidgetId.joinId extraResultsPrefixId [BS8.pack (show i)]
        extraResultsPrefixId = prefixId <> baseId <> WidgetId.Id ["extra results"]
        mkResult resultId (score, holeResult) =
            Result
            { _rScore = score
            , rHoleResult = holeResult
            , rId = resultId
            }

makeResultsList ::
    Monad m =>
    WidgetIds -> Text -> Group m ->
    m (Maybe (ResultsList m))
makeResultsList widgetIds searchTerm group =
    group ^. groupResults
    & ListClass.toList
    <&> sortOn fst
    <&> mResultsListOf widgetIds (group ^. groupId)
    <&> Lens.mapped %~ rlPreferred .~ toPreferred
    where
        toPreferred
            | [searchTerm] == group ^. groupSearchTerms = Preferred
            | otherwise = NotPreferred

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
        moreResults <- ListClass.take 2 moreResultsM & ListClass.toList

        tooFewGoodResults ++ moreResults
            & last
            & traverse %~ reverse
            & concatBothGoodAndBad
            & sortOn resultsListScore
            -- Re-split because now that we've added all the
            -- accumulated bad results we may have too many
            & splitAt holeResultCount
            & _2 %~ haveHiddenResults
            & return
    where
        concatBothGoodAndBad goodAndBad = goodAndBad ^. Lens.folded
        haveHiddenResults [] = Menu.NoMoreOptions
        haveHiddenResults _ = Menu.MoreOptionsAvailable
        resultsListScore x = (x ^. rlPreferred, x ^. rlMain . rScore & isGoodResult & not)
        prependResult results x =
            results
            & case (x ^. rlPreferred, x ^. rlMain . rScore & isGoodResult) of
                (NotPreferred, False) -> bad
                _ -> good
                %~ (x :)

isGoodResult :: Sugar.HoleResultScore -> Bool
isGoodResult hrs = hrs ^. Sugar.hrsNumHoleWrappers == 0

makeAll ::
    (MonadTransaction n m, MonadReader env m, Config.HasConfig env, GuiState.HasState env) =>
    T n [Sugar.HoleOption (T n) (ExpressionN n ())] ->
    Maybe (Sugar.OptionLiteral (T n) (ExpressionN n ())) ->
    WidgetIds ->
    m ([ResultsList (T n)], Menu.HasMoreOptions)
makeAll options mOptionLiteral widgetIds =
    do
        searchTerm <- HoleState.readSearchTerm widgetIds
        config <- Lens.view Config.config <&> Config.hole
        literalGroups <-
            (mOptionLiteral <&> makeLiteralGroups searchTerm) ^.. (Lens._Just . traverse)
            & sequenceA
            & transaction
        (options >>= mapM mkGroup <&> holeMatches searchTerm)
            <&> (literalGroups <>)
            <&> ListClass.fromList
            <&> ListClass.mapL (makeResultsList widgetIds searchTerm)
            <&> ListClass.catMaybes
            >>= collectResults config
            & transaction

mkGroupId :: Show a => Val a -> WidgetId.Id
mkGroupId option =
    option
    & ExprLens.valLeafs . V._LLiteral . V.primData .~ mempty
    & WidgetIds.hash

mkGroup ::
    Monad m =>
    Sugar.HoleOption (T m) (ExpressionN m ()) ->
    T m (Group (T m))
mkGroup option =
    option ^. Sugar.hoSugaredBaseExpr
    <&>
    \sugaredBaseExpr ->
    Group
    { _groupSearchTerms = sugaredBaseExpr & ValTerms.expr
    , _groupResults = option ^. Sugar.hoResults
    , _groupId = mkGroupId (option ^. Sugar.hoVal)
    }

tryBuildLiteral ::
    (Format a, Monad m) =>
    Text ->
    (Identity a -> Sugar.Literal Identity) ->
    Sugar.OptionLiteral (T m) (ExpressionN m ()) ->
    Text ->
    Maybe (T m (Group (T m)))
tryBuildLiteral identText mkLiteral optionLiteral searchTerm =
    tryParse searchTerm
    <&> Identity
    <&> mkLiteral
    <&> optionLiteral
    <&> Lens.mapped %~ f
    where
        f x =
            Group
            { _groupSearchTerms = [searchTerm]
            , _groupResults = pure x
            , _groupId = WidgetIds.hash identText
            }

makeLiteralGroups ::
    Monad m =>
    Text ->
    Sugar.OptionLiteral (T m) (ExpressionN m ()) ->
    [T m (Group (T m))]
makeLiteralGroups searchTerm optionLiteral =
    [ tryBuildLiteral "Num"   Sugar.LiteralNum   optionLiteral searchTerm
    , tryBuildLiteral "Bytes" Sugar.LiteralBytes optionLiteral searchTerm
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
