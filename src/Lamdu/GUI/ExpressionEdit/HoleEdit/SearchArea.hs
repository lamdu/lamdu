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
import           GUI.Momentu (View, (/-/))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.MetaKey as MetaKey
import           GUI.Momentu.ModKey (ModKey(..))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups (ResultGroup(..), Result(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups as ResultGroups
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.ResultWidget as ResultWidget
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui (ExpressionGui, ExpressionN)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import qualified Lamdu.GUI.ExpressionGui.Annotation as Annotation
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Parens as AddParens
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
    (MonadReader env m, GuiState.HasState env) =>
    Widget.Id -> (Text -> Bool) -> m (EventMap GuiState.Update)
searchTermEditEventMap searchMenuId allowedTerms =
    SearchMenu.readSearchTerm searchMenuId
    <&>
    \searchTerm ->
    let appendCharEventMap =
            Text.snoc searchTerm
            & E.allChars "Character"
            (E.Doc ["Edit", "Search Term", "Append character"])
            -- Don't add first operator char,
            -- we let ExprressionEdit.EventMap do that
            -- because it knows how to work with precedend and prefix chars.
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
    <&> GuiState.updateWidgetState searchMenuId
    where
        notOp = Text.any (`notElem` Chars.operator)

makeRenderedResult ::
    Monad m =>
    Sugar.Payload f ExprGui.Payload -> SearchMenu.ResultsContext -> Result (T m) ->
    ExprGuiM m (Menu.RenderedOption (T m))
makeRenderedResult pl ctx result =
    -- Warning: rHoleResult should be ran at most once!
    -- Running it more than once caused a horrible bug (bugfix: 848b6c4407)
    rHoleResult result
    & transaction
    <&> Lens.mapped %~ postProcessSugar (pl ^. Sugar.plData . ExprGui.plMinOpPrec)
    >>= ResultWidget.make ctx (rId result)

postProcessSugar :: Int -> ExpressionN m () -> ExpressionN m ExprGui.Payload
postProcessSugar minOpPrec expr =
    expr
    & AddParens.addWith minOpPrec
    <&> pl
    & SugarLens.wrappedExprs . Sugar.plData . ExprGui.plShowAnnotation
    .~ ExprGui.alwaysShowAnnotations
    where
        pl (x, needParens, ()) =
            ExprGui.Payload
            { ExprGui._plStoredEntityIds = []
            , ExprGui._plNearestHoles = NearestHoles.none
            , ExprGui._plShowAnnotation = ExprGui.neverShowAnnotations
            , ExprGui._plNeedParens = needParens == AddParens.NeedsParens
            , ExprGui._plMinOpPrec = x
            }

makeResultOption ::
    Monad m =>
    Sugar.Payload f ExprGui.Payload -> SearchMenu.ResultsContext -> ResultGroup (T m) ->
    Menu.Option (ExprGuiM m) (T m)
makeResultOption pl ctx results =
    Menu.Option
    { Menu._oId = results ^. ResultGroups.rgPrefixId
    , Menu._oRender = makeRenderedResult pl ctx (results ^. ResultGroups.rgMain)
    , Menu._oSubmenuWidgets =
        case results ^. ResultGroups.rgExtra of
        [] -> Menu.SubmenuEmpty
        extras ->
            traverse (makeRenderedResult pl ctx) extras
            <&> map makeSubMenu
            & Menu.SubmenuItems
    }
    where
        makeSubMenu extraResultWidget =
            Menu.Option
            { Menu._oId = results ^. ResultGroups.rgPrefixId -- UGLY HACK
            , Menu._oRender = pure extraResultWidget
            , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
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

-- Filter out events which should be taken by search term event map instead.
filterSearchTermEvents :: (Text -> Bool) -> Text -> EventMap a -> EventMap a
filterSearchTermEvents allowedTerms searchTerm =
    E.filterChars (not . allowedTerms . (searchTerm <>) . Text.singleton)

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
                ?? FocusDelegator.FocusEntryParent ?? hidClosed widgetIds
                <&> (Align.tValue %~)
        closedSearchTermGui <-
            maybeAddAnnotationPl pl
            <*>
            ( fdWrap
                <*> SearchTerm.make searchMenuId allowedTerms <&> Responsive.fromWithTextPos
            )
        isActive <- HoleWidgetIds.isActive widgetIds
        searchTermEventMap <- searchTermEditEventMap searchMenuId allowedTerms <&> fmap pure
        let inPlaceOfClosed open =
                closedSearchTermGui & Widget.widget %~
                Hover.hoverInPlaceOf [Hover.anchor open] . Hover.anchor
        if isActive && not isAHoleInHole
            then
                do
                    annotation <-
                        (/-/)
                        <$> Annotation.annotationSpacer
                        <*> makeInferredTypeAnnotation pl
                    -- ideally the fdWrap would be "inside" the
                    -- type-view addition and stdWrap, but it's not
                    -- important in the case the FD is selected, and
                    -- it is harder to implement, so just wrap it
                    -- here
                    (fdWrap <&> (Lens.mapped %~))
                        <*> SearchMenu.make (SearchTerm.make searchMenuId allowedTerms)
                            makeOptions annotation mNextEntry searchMenuId
                        <&> Lens.mapped . Align.tValue . Widget.eventMapMaker . Lens.mapped %~ (<> searchTermEventMap)
                        <&> Lens.mapped %~ inPlaceOfClosed . (^. Align.tValue)
            else
                (if isActive then Widget.setFocused else id)
                closedSearchTermGui
                & Widget.weakerEvents
                  (-- Editing search term of a closed hole opens it:
                      searchTermEventMap <&> Lens.mapped . GuiState.uCursor %~
                      mappend (Monoid.Last (Just searchMenuId))
                  )
                & const & pure
    where
        widgetIds = pl ^. Sugar.plEntityId & HoleWidgetIds.make
        searchMenuId = hidOpen widgetIds
        isAHoleInHole = ExprGui.isHoleResult pl
        mNextEntry = pl ^. Sugar.plData . ExprGui.plNearestHoles . NearestHoles.next <&> WidgetIds.fromEntityId
        makeOptions ctx =
            ResultGroups.makeAll options mOptionLiteral ctx
            <&> Lens.mapped %~ makeResultOption pl ctx
            <&> Lens.mapped . Menu.optionWidgets . Align.tValue . Widget.eventMapMaker . Lens.mapped %~
                filterSearchTermEvents allowedTerms (ctx ^. SearchMenu.rSearchTerm)

allowedSearchTermCommon :: Text -> Bool
allowedSearchTermCommon searchTerm =
    any (searchTerm &)
    [ Text.all (`elem` Chars.operator)
    , Text.all Char.isAlphaNum
    , (`Text.isPrefixOf` "{}")
    ]
