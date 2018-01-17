{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}
module Lamdu.GUI.ExpressionEdit.InjectEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Monad.Transaction (MonadTransaction)
import           Data.Store.Transaction (Transaction)
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrap, stdWrapParentExpr)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

makeCommon ::
    ( Monad m, MonadReader env f, MonadTransaction m f
    , HasConfig env, HasTheme env, GuiState.HasState env
    , Spacer.HasStdSpacing env, Element.HasAnimIdPrefix env, Menu.HasConfig env
    , Hover.HasStyle env, TextEdit.HasStyle env
    ) =>
    Options.Disambiguators (T m GuiState.Update) ->
    Sugar.Tag (Name (T m)) (T m) ->
    E.EventMap (T m GuiState.Update) ->
    NearestHoles -> WithTextPos View -> [ExpressionGui m] ->
    f (ExpressionGui m)
makeCommon disamb tag delEventMap nearestHoles colonLabel valEdits =
    (Options.boxSpaced ?? disamb)
    <*>
    ( TagEdit.makeCaseTag nearestHoles tag
        <&> (/|/ colonLabel)
        <&> Lens.mapped %~ Widget.weakerEvents delEventMap
        <&> Responsive.fromWithTextPos <&> (: valEdits)
    )

injectIndicator ::
    ( MonadReader env f, TextView.HasStyle env, HasTheme env
    , Element.HasAnimIdPrefix env
    ) => Text -> f (WithTextPos View)
injectIndicator text =
    (Styled.grammarText ?? text) <*> Element.subAnimId ["injectIndicator"]

makeNullaryInject ::
    Monad m =>
    Sugar.Tag (Name (T m)) (T m) -> Sugar.Payload (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui m)
makeNullaryInject tag pl =
    do
        dot <- injectIndicator "."
        stdWrap pl
            <*>
            makeCommon
            -- Give the tag widget the identity of the whole inject
            Options.disambiguationNone
            (tag & Sugar.tagInfo . Sugar.tagInstance .~ (pl ^. Sugar.plEntityId))
            mempty (pl ^. Sugar.plData . ExprGui.plNearestHoles) dot []

makeInject ::
    Monad m =>
    ExprGui.SugarExpr m ->
    Sugar.Tag (Name (T m)) (T m) -> Sugar.Payload (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui m)
makeInject val tag pl =
    do
        disamb <-
            if pl ^. Sugar.plData . ExprGui.plNeedParens
            then ResponsiveExpr.disambiguators <*> Lens.view Element.animIdPrefix
            else pure Options.disambiguationNone
        arg <-
            ExprGuiM.makeSubexpression val <&> (:[])
        colon <- injectIndicator ":"
        config <- Lens.view Config.config
        let replaceParentEventMap =
                -- Deleting the inject is replacing the whole expr
                -- with the injected value "child"
                case mReplaceParent of
                Nothing -> mempty
                Just replaceParent ->
                    replaceParent <&> WidgetIds.fromEntityId
                    & E.keysEventMapMovesCursor (Config.delKeys config) delDoc

        stdWrapParentExpr pl
            <*> makeCommon disamb tag replaceParentEventMap (ExprGui.nextHolesBefore val) colon arg
    where
        delDoc = E.Doc ["Edit", "Delete"]
        mReplaceParent = val ^. Sugar.rPayload . Sugar.plActions . Sugar.mReplaceParent


make ::
    Monad m =>
    Sugar.Inject (Name (T m)) (T m) (ExprGui.SugarExpr m) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.Inject tag mVal) = maybe makeNullaryInject makeInject mVal tag
