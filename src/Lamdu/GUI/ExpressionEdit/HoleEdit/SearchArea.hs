{-# LANGUAGE NoImplicitPrelude, NamedFieldPuns, DisambiguateRecordFields, OverloadedStrings #-}
-- | The search area (search term + results) of a hole.
-- When it is open it hovers over the space it takes when closed.
--
-- For non-wrapper holes this is the whole hole.

module Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea
    ( make, allowedSearchTermCommon
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (MonadTransaction(..))
import qualified Data.Char as Char
import qualified Data.Monoid as Monoid
import           Data.Store.Transaction (Transaction)
import qualified Data.Text as Text
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.CharClassification as Chars
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Open (makeOpenSearchAreaGui, ResultOption(..))
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups (ResultGroup(..), Result(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups as ResultGroups
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.ResultWidget as ResultWidget
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.State as HoleState
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui (ExpressionGui, ExpressionN)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import qualified Lamdu.GUI.ExpressionGui.Annotation as Annotation
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

fdConfig :: Config.Hole -> FocusDelegator.Config
fdConfig Config.Hole{holeOpenKeys, holeCloseKeys} = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = holeOpenKeys
    , FocusDelegator.focusChildDoc = E.Doc ["Navigation", "Hole", "Open"]
    , FocusDelegator.focusParentKeys = holeCloseKeys
    , FocusDelegator.focusParentDoc = E.Doc ["Navigation", "Hole", "Close"]
    }

searchTermEditEventMap ::
    (MonadReader env m, GuiState.HasState env, HasConfig env) =>
    WidgetIds -> (Text -> Bool) -> m (EventMap GuiState.Update)
searchTermEditEventMap widgetIds allowedTerms =
    HoleState.readSearchTerm widgetIds
    <&>
    \searchTerm ->
    let appendCharEventMap =
            Text.snoc searchTerm
            & E.allChars "Character"
            (E.Doc ["Edit", "Search Term", "Append character"])
            & if Text.null searchTerm then E.filter notOp else id
        deleteCharEventMap
            | Text.null searchTerm = mempty
            | otherwise =
                    Text.init searchTerm
                    & E.keyPress (ModKey mempty MetaKey.Key'Backspace)
                    (E.Doc ["Edit", "Search Term", "Delete backwards"])
    in
    appendCharEventMap <> deleteCharEventMap
    & E.filter allowedTerms
    <&> GuiState.updateWidgetState (hidOpen widgetIds)
    where
        notOp = Text.any (`notElem` Chars.operator)

makeShownResult ::
    Monad m =>
    Sugar.Payload f ExprGui.Payload -> Result (T m) ->
    ExprGuiM m
    ( EventMap (T m GuiState.Update)
    , ExprGuiM m (WithTextPos (Widget (T m GuiState.Update)))
    )
makeShownResult pl result =
    do
        -- Warning: rHoleResult should be ran at most once!
        -- Running it more than once caused a horrible bug (bugfix: 848b6c4407)
        res <- rHoleResult result & transaction
        theme <- Theme.hole <$> Lens.view Theme.theme
        stdSpacing <- Spacer.getSpaceSize
        let padding = Theme.holeResultPadding theme <&> realToFrac & (* stdSpacing)
        ResultWidget.make pl (rId result) res <&> _2 . Lens.mapped %~ Element.pad padding

makeResultOption ::
    Monad m =>
    Sugar.Payload f ExprGui.Payload ->
    ResultGroup (T m) ->
    ExprGuiM m (ResultOption m)
makeResultOption pl results =
    makeShownResult pl (results ^. ResultGroups.rgMain)
    <&>
    \(pickMain, mkMainResultWidget) ->
    ResultOption
    { _roOption =
        Menu.Option
        { Menu._oId = results ^. ResultGroups.rgPrefixId
        , Menu._oWidget = mkMainResultWidget
        , Menu._oSubmenuWidgets =
            case results ^. ResultGroups.rgExtra of
            [] -> Menu.SubmenuEmpty
            extras -> Menu.SubmenuItems (traverse (makeShownResult pl) extras >>= traverse snd)
        }
    , _roPickMainEventMap = pickMain
    }

makeInferredTypeAnnotation ::
    ( MonadReader env m, Theme.HasTheme env, Element.HasAnimIdPrefix env
    , MonadTransaction n0 m, Spacer.HasStdSpacing env
    ) =>
    Sugar.Payload m0 a0 -> m View
makeInferredTypeAnnotation pl =
    Annotation.addAnnotationBackground
    <*> TypeView.make (pl ^. Sugar.plAnnotation . Sugar.aInferredType)
    <&> (^. Align.tValue)
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId =
            pl ^. Sugar.plEntityId & HoleWidgetIds.make & hidHole & Widget.toAnimId

-- Has a typeView under the search term
make ::
    Monad m =>
    T m [Sugar.HoleOption (T m) (ExpressionN m ())] ->
    Maybe (Sugar.OptionLiteral (T m) (ExpressionN m ())) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    (Text -> Bool) ->
    ExprGuiM m (Menu.Placement -> ExpressionGui m)
make options mOptionLiteral pl allowedTerms =
    do
        config <- Lens.view Config.config
        let fdWrap =
                FocusDelegator.make ?? fdConfig (Config.hole config)
                ?? FocusDelegator.FocusEntryParent ?? hidClosedSearchArea widgetIds
                <&> (Align.tValue %~)
        closedSearchTermGui <-
            maybeAddAnnotationPl pl
            <*>
            ( fdWrap
                <*> SearchTerm.make widgetIds allowedTerms <&> Responsive.fromWithTextPos
            )
        isActive <- HoleWidgetIds.isActive widgetIds
        searchTermEventMap <- searchTermEditEventMap widgetIds allowedTerms <&> fmap pure
        let inPlaceOfClosed open =
                closedSearchTermGui & Widget.widget %~
                Hover.hoverInPlaceOf [Hover.anchor open] . Hover.anchor
        if isActive && not isAHoleInHole
            then
                do
                    typeView <- makeInferredTypeAnnotation pl
                    -- ideally the fdWrap would be "inside" the
                    -- type-view addition and stdWrap, but it's not
                    -- important in the case the FD is selected, and
                    -- it is harder to implement, so just wrap it
                    -- here
                    (fdWrap <&> (Lens.mapped %~))
                        <*> ( ResultGroups.makeAll options mOptionLiteral widgetIds
                                >>= traverse (makeResultOption pl)
                                >>= makeOpenSearchAreaGui searchTermEventMap
                                    allowedTerms typeView pl)
                        <&> Lens.mapped %~ inPlaceOfClosed . (^. Align.tValue)
            else
                (if isActive then Widget.setFocused else id)
                closedSearchTermGui
                & Widget.weakerEvents
                  (-- Editing search term of a closed hole opens it:
                      searchTermEventMap <&> Lens.mapped . GuiState.uCursor %~
                      mappend (Monoid.Last (Just (hidOpen widgetIds)))
                  )
                & const & pure
    where
        widgetIds = pl ^. Sugar.plEntityId & HoleWidgetIds.make
        isAHoleInHole = ExprGui.isHoleResult pl

allowedSearchTermCommon :: Text -> Bool
allowedSearchTermCommon searchTerm =
    any (searchTerm &)
    [ Text.all (`elem` Chars.operator)
    , Text.all Char.isAlphaNum
    , (`Text.isPrefixOf` "{}")
    ]
