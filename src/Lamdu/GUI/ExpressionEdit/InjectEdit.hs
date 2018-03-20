{-# LANGUAGE FlexibleContexts #-}
module Lamdu.GUI.ExpressionEdit.InjectEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.Responsive.Options as Options
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar
import           Revision.Deltum.Transaction (Transaction)

import           Lamdu.Prelude

type T = Transaction

injectIndicator ::
    ( MonadReader env f, TextView.HasStyle env, HasTheme env
    , Element.HasAnimIdPrefix env
    ) => Text -> f (WithTextPos View)
injectIndicator text =
    (Styled.grammarText ?? text) <*> Element.subAnimId ["injectIndicator"]

makeNullaryInject ::
    Monad m =>
    Sugar.Tag (Name (T m)) (T m) -> Sugar.Payload name (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui m)
makeNullaryInject tag pl =
    stdWrapParentExpr pl <*>
    do
        dot <- injectIndicator "."
        TagEdit.makeVariantTag nearestHoles tag <&> (/|/ dot)
            <&> Responsive.fromWithTextPos
    where
        nearestHoles = pl ^. Sugar.plData . ExprGui.plNearestHoles

makeInject ::
    Monad m =>
    ExprGui.SugarExpr m ->
    Sugar.Tag (Name (T m)) (T m) -> Sugar.Payload name (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui m)
makeInject val tag pl =
    stdWrapParentExpr pl <*>
    do
        disamb <-
            if pl ^. Sugar.plData . ExprGui.plNeedParens
            then ResponsiveExpr.disambiguators <*> Lens.view Element.animIdPrefix
            else pure Options.disambiguationNone
        arg <- ExprGuiM.makeSubexpression val
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

        (Options.boxSpaced ?? disamb)
            <*>
            ( TagEdit.makeVariantTag nearestHoles tag
                <&> (/|/ colon)
                <&> Lens.mapped %~ Widget.weakerEvents replaceParentEventMap
                <&> Responsive.fromWithTextPos
                <&> (: [arg])
            )
    where
        nearestHoles = ExprGui.nextHolesBefore val
        delDoc = E.Doc ["Edit", "Delete"]
        mReplaceParent = val ^. Sugar.rPayload . Sugar.plActions . Sugar.mReplaceParent

make ::
    Monad m =>
    Sugar.Inject (Name (T m)) (T m) (ExprGui.SugarExpr m) ->
    Sugar.Payload name (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.Inject tag mVal) = maybe makeNullaryInject makeInject mVal tag
