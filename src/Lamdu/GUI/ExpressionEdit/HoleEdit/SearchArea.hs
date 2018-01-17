{-# LANGUAGE NoImplicitPrelude, NamedFieldPuns, DisambiguateRecordFields, OverloadedStrings #-}
-- | The search area (search term + results) of a hole.
-- When it is open it hovers over the space it takes when closed.
--
-- For non-fragments this is the whole hole.

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
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Draw as Draw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified Lamdu.CharClassification as Chars
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups (ResultGroup(..), Result(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.ResultGroups as ResultGroups
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.ResultWidget as ResultWidget
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

fdConfig :: Config.Completion -> FocusDelegator.Config
fdConfig Config.Completion{completionOpenKeys, completionCloseKeys} = FocusDelegator.Config
    { FocusDelegator.focusChildKeys = completionOpenKeys
    , FocusDelegator.focusChildDoc = E.Doc ["Navigation", "Completion", "Open"]
    , FocusDelegator.focusParentKeys = completionCloseKeys
    , FocusDelegator.focusParentDoc = E.Doc ["Navigation", "Completion", "Close"]
    }

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
    & SugarLens.fragmentExprs . Sugar.plData . ExprGui.plShowAnnotation
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

makeSearchTerm ::
    ( MonadReader env m, HasTheme env, TextEdit.HasStyle env, GuiState.HasState env, Menu.HasConfig env
    , Applicative f
    ) =>
    Maybe Widget.Id ->
    Widget.Id ->
    (Text -> Bool) ->
    Maybe (Widget.PreEvent (f Menu.PickResult)) ->
    m (WithTextPos (Widget (f GuiState.Update)))
makeSearchTerm mNextEntry searchMenuId allowedSearchTerm mPickFirst =
    do
        isActive <- GuiState.isSubCursor ?? searchMenuId
        let bgColor
                | isActive = Theme.holeActiveSearchTermBGColor
                | otherwise = Theme.holeSearchTermBGColor
        theme <- Lens.view Theme.theme <&> Theme.hole
        (SearchMenu.addPickFirstResultEvent mNextEntry mPickFirst <&> (Align.tValue %~))
            <*> ( SearchMenu.basicSearchTermEdit searchMenuId allowedSearchTerm
                    <&> Align.tValue . Lens.mapped %~ pure
                )
            <&> Draw.backgroundColor
                (Widget.toAnimId searchMenuId <> ["hover background"])
                (bgColor theme)

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
                FocusDelegator.make ?? fdConfig (Config.completion config)
                ?? FocusDelegator.FocusEntryParent ?? hidClosed widgetIds
                <&> (Align.tValue %~)
        closedSearchTermGui <-
            maybeAddAnnotationPl pl
            <*>
            ( fdWrap
                <*> makeSearchTerm Nothing searchMenuId allowedTerms Nothing <&> Responsive.fromWithTextPos
            )
        isActive <- HoleWidgetIds.isActive widgetIds
        searchTermEventMap <- SearchMenu.searchTermEditEventMap searchMenuId adhocAllowedTerms <&> fmap pure
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
                        <*> SearchMenu.make (makeSearchTerm mNextEntry searchMenuId allowedTerms)
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
        adhocAllowedTerms txt
            | Text.length txt == 1 && Text.all (`elem` Chars.operator) txt =
                -- Don't add first operator char,
                -- we let ExprressionEdit.EventMap do that
                -- because it knows how to work with precedence and prefix chars
                False
            | otherwise = allowedTerms txt
        makeOptions ctx =
            ResultGroups.makeAll options mOptionLiteral ctx
            <&> Lens.mapped %~ makeResultOption pl ctx
            <&> Lens.mapped . Menu.optionWidgets . Align.tValue . Widget.eventMapMaker . Lens.mapped %~
                filterSearchTermEvents allowedTerms (ctx ^. SearchMenu.rSearchTerm)

type Suffix = Char

allowedSearchTermCommon :: [Suffix] -> Text -> Bool
allowedSearchTermCommon suffixes searchTerm =
    any (searchTerm &)
    [ Text.all (`elem` Chars.operator)
    , Text.all Char.isAlphaNum
    , (`Text.isPrefixOf` "{}")
    , Lens.has (Lens.reversed . Lens._Cons . Lens.filtered inj)
    ]
    where
        inj (lastChar, revInit) =
            lastChar `elem` suffixes && Text.all Char.isAlphaNum revInit
