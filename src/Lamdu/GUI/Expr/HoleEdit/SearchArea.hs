-- | The search area (search term + results) of a hole.
-- When it is open it hovers over the space it takes when closed.
--
-- For non-fragments this is the whole hole.

module Lamdu.GUI.Expr.HoleEdit.SearchArea
    ( make
    , AnnotationMode(..)
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import           GUI.Momentu.Align (TextWidget)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Draw as MDraw
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/))
import qualified GUI.Momentu.Glue as Glue
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.FocusDelegator as FocusDelegator
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import           Hyper
import qualified Lamdu.CharClassification as Chars
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.Expr.HoleEdit.ResultGroups (ResultGroup(..), Result(..))
import qualified Lamdu.GUI.Expr.HoleEdit.ResultGroups as ResultGroups
import qualified Lamdu.GUI.Expr.HoleEdit.ResultWidget as ResultWidget
import           Lamdu.GUI.Expr.HoleEdit.WidgetIds (WidgetIds(..))
import qualified Lamdu.GUI.Expr.HoleEdit.WidgetIds as HoleWidgetIds
import           Lamdu.GUI.Annotation (maybeAddAnnotationPl)
import qualified Lamdu.GUI.Annotation as Annotation
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Monad as GuiM
import qualified Lamdu.GUI.Types as ExprGui
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Name as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Parens as AddParens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

fdConfig ::
    ( Has Config env
    , Has (Texts.CodeUI Text) env
    , Has (MomentuTexts.Texts Text) env
    ) => env -> FocusDelegator.Config
fdConfig env = FocusDelegator.Config
    { FocusDelegator.focusChildKeys =
        env ^. has . Config.completion . Config.completionOpenKeys
    , FocusDelegator.focusChildDoc =
        E.toDoc env
        [ has . MomentuTexts.navigation
        , has . Texts.completion
        , has . Texts.open
        ]
    , FocusDelegator.focusParentKeys = env ^. has . Config.completion . Config.completionCloseKeys
    , FocusDelegator.focusParentDoc =
        E.toDoc env
        [ has . MomentuTexts.navigation
        , has . Texts.completion
        , has . Texts.close
        ]
    }

makeRenderedResult ::
    (Monad i, Monad o, Has (MomentuTexts.Texts Text) env) =>
    ExprGui.GuiPayload -> SearchMenu.ResultsContext ->
    Result i o ->
    GuiM env i o (Menu.RenderedOption o)
makeRenderedResult pl ctx result =
    do
        -- Warning: rHoleResult should be ran at most once!
        -- Running it more than once caused a horrible bug (bugfix: 848b6c4407)
        res <- rHoleResult result & GuiM.im
        res ^. Sugar.holeResultConverted
            & postProcessSugar (pl ^. ExprGui.plParenInfo . Sugar.piMinOpPrec)
            & ResultWidget.make ctx (rId result)
                (res ^. Sugar.holeResultPick)

postProcessSugar ::
    AddParens.MinOpPrec ->
    Sugar.Expr Sugar.Binder (Sugar.Annotation () Name) Name i o () ->
    ExprGui.Expr Sugar.Binder i o
postProcessSugar minOpPrec binder =
    binder
    & SugarLens.annotations . Sugar._AnnotationVal .~ mempty
    & AddParens.addToBinderWith minOpPrec
    & hflipped %~ hmap (\_ -> Lens._Wrapped %~ pl)
    where
        pl (parenInfo, sugarPl) =
            ExprGui.GuiPayload
            { ExprGui._plHiddenEntityIds = []
            , ExprGui._plParenInfo = parenInfo
            }
            <$ sugarPl

makeResultOption ::
    (Monad i, Monad o, Has (MomentuTexts.Texts Text) env) =>
    ExprGui.GuiPayload -> SearchMenu.ResultsContext ->
    ResultGroup i o -> Menu.Option (GuiM env i o) o
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
    ( MonadReader env m, Has Theme env, Element.HasAnimIdPrefix env
    , Spacer.HasStdSpacing env, Has (Texts.Name Text) env, Glue.HasTexts env
    , Has (Texts.Code Text) env
    ) =>
    Sugar.Annotation v Name -> AnimId -> m View
makeInferredTypeAnnotation ann animId =
    Annotation.addAnnotationBackground
    <*> TypeView.make (ann ^?! Sugar._AnnotationType)
    <&> (^. Align.tValue)
    & Reader.local (Element.animIdPrefix .~ animId)

-- Filter out events which should be taken by search term event map instead.
filterSearchTermEvents :: (Text -> Bool) -> Text -> EventMap a -> EventMap a
filterSearchTermEvents allowedTerms searchTerm
    | Text.null searchTerm =
        E.filterChars (`elem` Chars.operator)
    | otherwise =
        E.filterChars (not . allowedTerms . (searchTerm <>) . Text.singleton)

data AnnotationMode = WithAnnotation | WithoutAnnotation

make ::
    ( Monad i, Monad o
    , Glue.HasTexts env
    , Has (Texts.Name Text) env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (TextEdit.Texts Text) env
    , SearchMenu.HasTexts env
    ) =>
    AnnotationMode ->
    i [Sugar.HoleOption Name i o] ->
    ExprGui.Payload i o -> (Text -> Bool) -> WidgetIds ->
    GuiM env i o (Menu.Placement -> TextWidget o)
make annMode mkOptions pl allowedTerms widgetIds =
    do
        env <- Lens.view id
        let fdWrap =
                FocusDelegator.make ?? fdConfig env
                ?? FocusDelegator.FocusEntryParent ?? hidClosed widgetIds
                <&> (Align.tValue %~)
        term <- makeTerm Menu.NoPickFirstResult
        closedSearchTermGui <-
            maybeAddAnn <*> (fdWrap ?? term ^. SearchMenu.termWidget)
        isActive <- HoleWidgetIds.isActive widgetIds
        padToSize <- Element.padToSize
        let inPlaceOfClosed = padToSize (closedSearchTermGui ^. Element.size) 0
        isAHoleInHole <- GuiM.isHoleResult
        if isActive && not isAHoleInHole
            then
                do
                    annotationGui <-
                        case annMode of
                        WithoutAnnotation -> pure Element.empty
                        WithAnnotation ->
                            Annotation.annotationSpacer
                            /-/ makeInferredTypeAnnotation (pl ^. _1 . Sugar.plAnnotation) animId
                    options <- GuiM.im mkOptions
                    -- ideally the fdWrap would be "inside" the
                    -- type-view addition and stdWrap, but it's not
                    -- important in the case the FD is selected, and
                    -- it is harder to implement, so just wrap it
                    -- here
                    (fdWrap <&> (Lens.mapped %~))
                        <*> SearchMenu.make makeTerm
                            (filteredOptions options) annotationGui searchMenuId
                        <&> Lens.mapped . Align.tValue %~ inPlaceOfClosed
            else
                closedSearchTermGui
                <&> (if isActive then Widget.setFocused else id)
                <&> (if isAHoleInHole
                    then Widget.disableStroll
                    else Widget.takesStroll (hidClosed widgetIds))
                <&> Widget.weakerEvents
                  (-- Editing search term of a closed hole opens it:
                      term ^. SearchMenu.termEditEventMap
                      <&> Lens.mapped . GuiState.uCursor %~
                      mappend (Monoid.Last (Just searchMenuId))
                  )
                & const & pure
    where
        maybeAddAnn =
            case annMode of
            WithoutAnnotation -> pure id
            WithAnnotation -> maybeAddAnnotationPl (pl ^. _1) <&> (Align.tValue %~)
        makeTerm mPickFirst =
            do
                theme <- Lens.view (has . Theme.hole)
                frameWidth <- Spacer.stdFontHeight <&> pure <&> (* theme ^. Theme.holeFrameWidth)
                addFrame <-
                    MDraw.addInnerFrame ?? theme ^. Theme.holeFrameColor ?? frameWidth
                    & Reader.local (Element.animIdPrefix .~ animId <> ["hole-frame"])
                SearchMenu.searchTermEdit searchMenuId allowedTermsCtx mPickFirst
                    <&> SearchMenu.termWidget %~
                        addFrame . Element.padAround (frameWidth & _2 .~ 0)
        animId = hidClosed widgetIds & Widget.toAnimId
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
            <&> Lens.mapped %~ makeResultOption (pl ^. _2) ctx
            <&> Lens.mapped . Menu.optionWidgets . Align.tValue . Widget.eventMapMaker . Lens.mapped %~
                filterSearchTermEvents allowedTerms (ctx ^. SearchMenu.rSearchTerm)
