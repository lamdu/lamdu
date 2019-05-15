{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields, FlexibleContexts #-}
-- | The search area (search term + results) of a hole.
-- When it is open it hovers over the space it takes when closed.
--
-- For non-fragments this is the whole hole.

module Lamdu.GUI.ExpressionEdit.HoleEdit.SearchArea
    ( make
    ) where

import           AST (Tree, Ann(..), annotations)
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import           GUI.Momentu (View, (/-/))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as MDraw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Hover as Hover
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.State (Gui)
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
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.ExpressionGui.Annotation (maybeAddAnnotationPl)
import qualified Lamdu.GUI.ExpressionGui.Annotation as Annotation
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.I18N.Language as Language
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Parens as AddParens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

fdConfig :: Config.Completion -> FocusDelegator.Config
fdConfig config = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = config ^. Config.completionOpenKeys
    , FocusDelegator.focusChildDoc = E.Doc ["Navigation", "Completion", "Open"]
    , FocusDelegator.focusParentKeys = config ^. Config.completionCloseKeys
    , FocusDelegator.focusParentDoc = E.Doc ["Navigation", "Completion", "Close"]
    }

makeRenderedResult ::
    (Monad i, Monad o) =>
    Sugar.Payload name i o ExprGui.Payload -> SearchMenu.ResultsContext ->
    Result i o ->
    ExprGuiM i o (Menu.RenderedOption o)
makeRenderedResult pl ctx result =
    do
        -- Warning: rHoleResult should be ran at most once!
        -- Running it more than once caused a horrible bug (bugfix: 848b6c4407)
        res <- rHoleResult result & ExprGuiM.im
        res ^. Sugar.holeResultConverted
            & postProcessSugar (pl ^. Sugar.plData . ExprGui.plMinOpPrec)
            & ResultWidget.make ctx (rId result)
                (res ^. Sugar.holeResultPick)

postProcessSugar ::
    AddParens.MinOpPrec ->
    Tree (Ann (Sugar.Payload (Name o) i o ())) (Sugar.Binder (Name o) i o) ->
    Tree (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) (Sugar.Binder (Name o) i o)
postProcessSugar minOpPrec binder =
    AddParens.addToBinderWith minOpPrec binder
    & annotations %~ pl
    where
        pl (x, needParens, sugarPl) =
            ExprGui.Payload
            { ExprGui._plHiddenEntityIds = []
            , ExprGui._plNeedParens = needParens == AddParens.NeedsParens
            , ExprGui._plMinOpPrec = x
            }
            <$ sugarPl

makeResultOption ::
    (Monad i, Monad o) =>
    Sugar.Payload name i o ExprGui.Payload -> SearchMenu.ResultsContext ->
    ResultGroup i o -> Menu.Option (ExprGuiM i o) o
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
    , Spacer.HasStdSpacing env, Language.HasLanguage env
    ) =>
    Sugar.Payload (Name g) i o a0 -> m View
makeInferredTypeAnnotation pl =
    Annotation.addAnnotationBackground
    <*> TypeView.make (pl ^?! Sugar.plAnnotation . SugarLens.annotationTypes)
    <&> (^. Align.tValue)
    & Reader.local (Element.animIdPrefix .~ animId)
    where
        animId =
            pl ^. Sugar.plEntityId & HoleWidgetIds.make & hidHole & Widget.toAnimId

-- Filter out events which should be taken by search term event map instead.
filterSearchTermEvents :: (Text -> Bool) -> Text -> EventMap a -> EventMap a
filterSearchTermEvents allowedTerms searchTerm
    | Text.null searchTerm =
        E.filterChars (`elem` Chars.operator)
    | otherwise =
        E.filterChars (not . allowedTerms . (searchTerm <>) . Text.singleton)

make ::
    (Monad i, Monad o) =>
    i [Sugar.HoleOption (Name o) i o] ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    (Text -> Bool) ->
    ExprGuiM i o (Menu.Placement -> Gui Responsive o)
make mkOptions pl allowedTerms =
    do
        config <- Lens.view Config.config
        let fdWrap =
                FocusDelegator.make ?? fdConfig (config ^. Config.completion)
                ?? FocusDelegator.FocusEntryParent ?? hidClosed widgetIds
                <&> (Align.tValue %~)
        term <- makeTerm Menu.NoPickFirstResult
        closedSearchTermGui <-
            (maybeAddAnnotationPl pl <&> (Widget.widget %~)) <*>
            (fdWrap ?? term ^. SearchMenu.termWidget <&> Responsive.fromWithTextPos)
        isActive <- HoleWidgetIds.isActive widgetIds
        anc <- Hover.anchor
        let inPlaceOfClosed open =
                closedSearchTermGui & Widget.widget %~
                (anc open `Hover.emplaceAt`) . anc
        isAHoleInHole <- ExprGuiM.isHoleResult
        if isActive && not isAHoleInHole
            then
                do
                    annotation <-
                        Annotation.annotationSpacer
                        /-/ makeInferredTypeAnnotation pl
                    options <- ExprGuiM.im mkOptions
                    -- ideally the fdWrap would be "inside" the
                    -- type-view addition and stdWrap, but it's not
                    -- important in the case the FD is selected, and
                    -- it is harder to implement, so just wrap it
                    -- here
                    (fdWrap <&> (Lens.mapped %~))
                        <*> SearchMenu.make makeTerm
                            (filteredOptions options) annotation searchMenuId
                        <&> Lens.mapped %~ inPlaceOfClosed . (^. Align.tValue)
            else
                closedSearchTermGui
                & (if isActive
                    then Widget.setFocused
                    else id
                    )
                & (if isAHoleInHole
                    then
                        Widget.widget . Widget.wState .
                        Widget._StateUnfocused . Widget.uMStroll .~ Nothing
                    else id)
                & Widget.weakerEvents
                  (-- Editing search term of a closed hole opens it:
                      term ^. SearchMenu.termEditEventMap
                      <&> Lens.mapped . GuiState.uCursor %~
                      mappend (Monoid.Last (Just searchMenuId))
                  )
                & const & pure
    where
        makeTerm mPickFirst =
            do
                theme <- Lens.view (Theme.theme . Theme.hole)
                frameWidth <- Spacer.stdFontHeight <&> pure <&> (* theme ^. Theme.holeFrameWidth)
                addFrame <-
                    MDraw.addInnerFrame ?? theme ^. Theme.holeFrameColor ?? frameWidth
                    & Reader.local (Element.animIdPrefix .~ animId <> ["hole-frame"])
                SearchMenu.searchTermEdit searchMenuId allowedTermsCtx mPickFirst
                    <&> SearchMenu.termWidget %~
                        addFrame . Element.padAround (frameWidth & _2 .~ 0)
        animId =
            pl ^. Sugar.plEntityId & HoleWidgetIds.make & hidHole & Widget.toAnimId
        widgetIds = pl ^. Sugar.plEntityId & HoleWidgetIds.make
        searchMenuId = hidOpen widgetIds
        allowedTermsCtx txt =
            SearchMenu.TermCtx
            { SearchMenu._tcTextEdit = allowedTerms txt
            , SearchMenu._tcAdHoc =
                -- Don't add first operator char,
                -- we let ExprressionEdit.EventMap do that
                -- because it knows how to work with precedence and prefix chars
                (Text.length txt /= 1 || Text.any (`notElem` Chars.operator) txt)
                && allowedTerms txt
            }
        filteredOptions opts ctx =
            ResultGroups.makeAll opts ctx
            <&> Lens.mapped %~ makeResultOption pl ctx
            <&> Lens.mapped . Menu.optionWidgets . Align.tValue . Widget.eventMapMaker . Lens.mapped %~
                filterSearchTermEvents allowedTerms (ctx ^. SearchMenu.rSearchTerm)
