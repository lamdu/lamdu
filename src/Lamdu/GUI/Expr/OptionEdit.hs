{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Lamdu.GUI.Expr.OptionEdit where

import qualified Control.Lens as Lens
import           Hyper
import qualified GUI.Momentu as M
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.ModKey as ModKey
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeQuery :: _ => SearchMenu.ResultsContext -> f (Sugar.Query Text)
makeQuery ctx = 
    Lens.view id <&>
    \env ->
    Sugar.Query
    { Sugar._qLangInfo = Sugar.QueryLangInfo (env ^. has) (env ^. has) (env ^. has) (env ^. has) (env ^. has)
    , Sugar._qSearchTerm = ctx ^. SearchMenu.rSearchTerm
    }

-- Remove unwanted event handlers from a hole result.
-- without it these keys perform the actions on the hole result,
-- while that isn't the probable intended result.
removeUnwanted :: _ => m (EventMap a -> EventMap a)
removeUnwanted =
    Lens.view id <&>
    \c ->
    Config.delKeys c -- Delete key has behaviours in various editors like if-else
    <> (c ^.. Config.hasConfig . Config.grid . Lens.folded) -- Arrow keys taken by hole is weird
    <&> E.KeyEvent ModKey.KeyState'Pressed
    & E.deleteKeys

makeResult ::
    forall i o t m.
    _ =>
    (Annotated (Sugar.Payload (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) o) #
        t (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) Name i o ->
        m (Responsive.Responsive o)
    ) ->
    SearchMenu.ResultsContext ->
    Sugar.Option t Name i o ->
    Menu.Option m o
makeResult mkGui ctx res =
    Menu.Option
    { Menu._oId = resId
    , Menu._oRender =
        do
            chooseText <- Lens.view (has . MomentuTexts.choose)
            remUnwanted <- removeUnwanted
            res ^. Sugar.optionExpr
                & SugarLens.hAnnotations
                    @(Sugar.Annotation () Name)
                    @(Sugar.Annotation (Sugar.EvaluationScopes Name i) Name)
                    @(Ann (Const (Sugar.Payload (Sugar.Annotation () Name) o)))
                    @(Ann (Const (Sugar.Payload (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) o)))
                    @(t (Sugar.Annotation () Name) Name i o)
                    @(t (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) Name i o)
                    .~ Sugar.AnnotationNone
                & mkGui
                & GuiState.assignCursor resId dstId
                <&>
                \w ->
                Menu.RenderedOption
                { Menu._rWidget =
                    w ^. Responsive.rWide & M.tValue . Widget.enterResultCursor .~ resId
                    <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ remUnwanted
                    <&> -- Disable strolling between hole results
                        Widget.widget %~ Widget.disableStroll
                , Menu._rPick =
                    Widget.PreEvent
                    { Widget._pDesc = chooseText
                    , Widget._pAction = Menu.PickResult dstId innerHole <$ res ^. Sugar.optionPick
                    , Widget._pTextRemainder = ""
                    }
                }
    , Menu._oSubmenuWidgets = Menu.SubmenuEmpty
    }
    where
        dstId = fromMaybe (WidgetIds.fromExprPayload (res ^. Sugar.optionExpr . annotation)) innerHole
        resId = ctx ^. SearchMenu.rResultIdPrefix <> dstId
        innerHole = res ^? Sugar.optionExpr . SugarLens.unfinishedPayloads <&> WidgetIds.fromExprPayload
