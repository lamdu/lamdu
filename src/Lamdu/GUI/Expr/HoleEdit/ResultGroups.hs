{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.Expr.HoleEdit.ResultGroups
    ( makeAll
    , Result(..)
    , ResultGroup(..), rgPrefixId, rgMain, rgExtra, rgTerms
    , Mode(..)
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.ListT (ListT)
import qualified Data.ByteString.Char8 as BS8
import           Data.Function (on)
import           Data.List (sortOn, nubBy)
import qualified Data.List.Class as ListClass
import           Data.MRUMemo (memo)
import qualified Data.Text as Text
import qualified GUI.Momentu.Widget.Id as WidgetId
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified Lamdu.Config as Config
import           Lamdu.Fuzzy (Fuzzy)
import qualified Lamdu.Fuzzy as Fuzzy
import qualified Lamdu.GUI.Expr.HoleEdit.ValTerms as ValTerms
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

data Group i o = Group
    { _groupSearchTerms :: [Text]
    , _groupId :: WidgetId.Id
    , _groupResults ::
        ListT i
        ( Sugar.HoleResultScore
        , i (Sugar.HoleResult Name i o)
        )
    }
Lens.makeLenses ''Group

data Result i o = Result
    { _rScore :: Sugar.HoleResultScore
    , -- Warning: This action should be ran at most once!
      -- Running it more than once will cause inconsistencies.
      rHoleResult :: i (Sugar.HoleResult Name i o)
        -- TODO: Unit monad instead of i o for Expression above?
    , rId :: WidgetId.Id
    }
Lens.makeLenses ''Result

data IsExactMatch = ExactMatch | NotExactMatch
    deriving (Eq, Ord)

data ResultGroup i o = ResultGroup
    { _rgExactMatch :: IsExactMatch -- Move to top of result list
    , _rgPrefixId :: WidgetId.Id
    , _rgMain :: Result i o
    , _rgExtra :: [Result i o]
    , _rgTerms :: [Text] -- for debugging
    }
Lens.makeLenses ''ResultGroup

mResultGroupOf ::
    [Text] ->
    WidgetId.Id ->
    [ ( Sugar.HoleResultScore
      , i (Sugar.HoleResult Name i o)
      )
    ] ->
    Maybe (ResultGroup i o)
mResultGroupOf _ _ [] = Nothing
mResultGroupOf terms prefixId (x:xs) = Just
    ResultGroup
    { _rgExactMatch = NotExactMatch
    , _rgPrefixId = prefixId
    , _rgMain = mkResult prefixId x
    , _rgExtra = Lens.imap mkExtra xs
    , _rgTerms = terms
    }
    where
        mkExtra = mkResult . extraResultId
        extraResultId i = WidgetId.joinId extraResultsPrefixId [BS8.pack (show i)]
        extraResultsPrefixId = prefixId <> WidgetId.Id ["extra results"]
        mkResult resultId (score, holeResult) =
            Result
            { _rScore = score
            , rHoleResult = holeResult
            , rId = resultId
            }

makeResultGroup ::
    Monad i =>
    SearchMenu.ResultsContext -> Group i o ->
    i (Maybe (ResultGroup i o))
makeResultGroup ctx group =
    group ^. groupResults
    & ListClass.toList
    <&> sortOn fst
    <&> mResultGroupOf terms (ctx ^. SearchMenu.rResultIdPrefix <> group ^. groupId)
    <&> Lens.mapped %~ rgExactMatch .~ toExactMatch
    where
        terms = group ^. groupSearchTerms
        searchTerm = ctx ^. SearchMenu.rSearchTerm
        toExactMatch
            | searchTerm `elem` terms = ExactMatch
            | otherwise = NotExactMatch

data GoodAndBad a = GoodAndBad { _good :: a, _bad :: a }
    deriving (Functor, Foldable, Traversable)
Lens.makeLenses ''GoodAndBad

data Mode = PreferLocals | AllTheSame

collectResults ::
    Monad i =>
    Mode -> Config.Completion -> ListT i (ResultGroup i o) ->
    i (Menu.OptionList (ResultGroup i o))
collectResults mode config resultsM =
    do
        (tooFewGoodResults, moreResultsM) <-
            ListClass.scanl prependResult (GoodAndBad [] []) resultsM
            & ListClass.splitWhenM (pure . (>= resCount) . length . _good)

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
            & splitAt resCount
            & _2 %~ not . null
            & uncurry Menu.toOptionList
            & pure
    where
        resCount = config ^. Config.completionResultCount
        concatBothGoodAndBad goodAndBad = goodAndBad ^. Lens.folded
        resultsListScore x = (x ^. rgExactMatch, x ^. rgMain . rScore & isGoodResult mode & not)
        prependResult results x =
            results
            & case (x ^. rgExactMatch, x ^. rgMain . rScore & isGoodResult mode) of
                (NotExactMatch, False) -> bad
                _ -> good
                %~ (x :)

isGoodResult :: Mode -> Sugar.HoleResultScore -> Bool
isGoodResult PreferLocals hrs =
    case hrs ^. Sugar.hrsTier of
    Sugar.HoleResultGlobal -> hrs ^. Sugar.hrsNumFragments == 0
    _ -> True
isGoodResult AllTheSame hrs = hrs ^. Sugar.hrsNumFragments == 0

makeAll ::
    _ =>
    Mode ->
    [Sugar.HoleOption Name i o1] ->
    SearchMenu.ResultsContext ->
    GuiM env i o (Menu.OptionList (ResultGroup i o1))
makeAll mode options ctx =
    do
        config <- Lens.view (has . Config.completion)
        env <- Lens.view id
        traverse (mkGroup env) options
            <&> holeMatches searchTerm
            <&> ListClass.fromList
            <&> ListClass.mapL (makeResultGroup ctx)
            <&> ListClass.catMaybes
            >>= collectResults mode config
            & GuiM.im
    where
        searchTerm = ctx ^. SearchMenu.rSearchTerm

mkGroup :: _ => env -> Sugar.HoleOption Name i o -> i (Group i o)
mkGroup env option =
    option ^. Sugar.hoSearchTerms
    <&>
    \searchTerms ->
    Group
    { _groupSearchTerms = searchTerms >>= ValTerms.holeSearchTerm env
    , _groupResults = option ^. Sugar.hoResults
    , _groupId = option ^. Sugar.hoEntityId & WidgetIds.fromEntityId
    }

unicodeAlts :: Text -> [Text]
unicodeAlts haystack =
    traverse alts (Text.unpack haystack)
    <&> concat
    <&> Text.pack
    where
        alts x = [x] : extras x
        extras '≥' = [">="]
        extras '≤' = ["<="]
        extras '≠' = ["/=", "!=", "<>"]
        extras '⋲' = ["<"]
        extras _ = []

{-# NOINLINE fuzzyMaker #-}
fuzzyMaker :: [(Text, Int)] -> Fuzzy (Set Int)
fuzzyMaker = memo Fuzzy.make

holeMatches :: Text -> [Group i o] -> [Group i o]
holeMatches searchTerm groups
    | Text.null searchTerm = groups
    | otherwise =
        groups
        ^@.. Lens.ifolded
        <&> (\(idx, group) -> searchTerms group <&> ((,) ?? (idx, group)))
        & concat
        & (Fuzzy.memoableMake fuzzyMaker ?? searchTerm)
        <&> snd
        & nubBy ((==) `on` fst)
        <&> snd
    where
        searchTerms group =
            case group ^. groupSearchTerms of
            [] -> [""]
            terms -> terms >>= unicodeAlts
