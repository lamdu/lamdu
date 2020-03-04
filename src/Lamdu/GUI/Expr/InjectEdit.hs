{-# LANGUAGE RankNTypes #-}
module Lamdu.GUI.Expr.InjectEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Extended (OneOf)
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Direction as Dir
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Grid as Grid
import qualified GUI.Momentu.Widgets.Menu.Search as SearchMenu
import qualified GUI.Momentu.Widgets.TextEdit as TextEdit
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui.Monad (GuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as GuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import           Lamdu.GUI.Styled (text, grammar)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.Code as Texts
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Definitions as Texts
import qualified Lamdu.I18N.Name as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

injectIndicator ::
    ( MonadReader env f, Has TextView.Style env, Has Theme env
    , Element.HasAnimIdPrefix env, Has (Texts.Code Text) env
    , Has Dir.Layout env
    ) => OneOf Texts.Code -> f (WithTextPos View)
injectIndicator l = grammar (text ["injectIndicator"] l)

makeInject ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.Expr Sugar.Term (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
    Sugar.TagRef Name i o ->
    Sugar.Payload (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
    GuiM env i o (Responsive o)
makeInject val tag pl =
    do
        env <- Lens.view id
        let delDoc = E.toDoc env [has . MomentuTexts.edit, has . MomentuTexts.delete]
        arg <- GuiM.makeSubexpression val
        let replaceParentEventMap replaceParent =
                -- Deleting the inject is replacing the whole expr
                -- with the injected value "child"
                replaceParent <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys env) delDoc

        (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
            <*>
            ( TagEdit.makeVariantTag tag
                /|/ injectIndicator Texts.injectSymbol
                <&> Lens.mapped %~ Widget.weakerEvents (foldMap replaceParentEventMap mReplaceParent)
                <&> Responsive.fromWithTextPos
                <&> (: [arg])
            )
        & stdWrapParentExpr pl
    where
        mReplaceParent = val ^. annotation . Sugar.plActions . Sugar.mReplaceParent

emptyRec ::
    Annotated a # Const (Sugar.NullaryVal name i o) ->
    Annotated a # Sugar.Term v name i o
emptyRec (Ann (Const pl) (Const (Sugar.NullaryVal closedActions addItem))) =
    Sugar.Composite [] [] (Sugar.ClosedComposite closedActions) addItem
    & Sugar.BodyRecord
    & Ann (Const pl)

makeNullaryInject ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Annotated
    (Sugar.Payload (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload) #
        Const (Sugar.NullaryVal Name i o) ->
    Sugar.TagRef Name i o ->
    Sugar.Payload (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
    GuiM env i o (Responsive o)
makeNullaryInject nullary tag pl =
    GuiState.isSubCursor ?? nullaryRecEntityId
    >>= \case
    True -> makeInject (emptyRec nullary) tag pl
    False ->
        do
            env <- Lens.view id
            let expandNullaryVal =
                    GuiState.updateCursor nullaryRecEntityId & pure & const
                    & E.charGroup Nothing
                    (E.toDoc env
                        [ has . MomentuTexts.edit
                        , has . Texts.inject
                        , has . Texts.value
                        ]) ":"
            TagEdit.makeVariantTag tag
                /|/ injectIndicator Texts.nullaryInjectSymbol
                <&> Responsive.fromWithTextPos
                <&> Widget.weakerEvents expandNullaryVal
                & stdWrapParentExpr pl
    where
        nullaryRecEntityId =
            nullary ^. annotation . Sugar.plEntityId
            & WidgetIds.fromEntityId

make ::
    ( Monad i, Monad o
    , Grid.HasTexts env
    , TextEdit.HasTexts env
    , SearchMenu.HasTexts env
    , Has (Texts.Code Text) env
    , Has (Texts.CodeUI Text) env
    , Has (Texts.Definitions Text) env
    , Has (Texts.Name Text) env
    , Has (Texts.Navigation Text) env
    ) =>
    Sugar.Expr Sugar.Inject (Sugar.EvaluationScopes Name i) Name i o ExprGui.Payload ->
    GuiM env i o (Responsive o)
make (Ann (Const pl) (Sugar.Inject tag mVal)) =
    ( case mVal of
        Sugar.InjectNullary nullary -> makeNullaryInject nullary
        Sugar.InjectVal val -> makeInject val
    ) tag pl
