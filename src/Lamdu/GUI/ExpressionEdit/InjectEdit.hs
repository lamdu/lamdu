{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Lamdu.GUI.ExpressionEdit.InjectEdit
    ( make
    ) where

import           AST (Tree, Ann(..), ann)
import qualified Control.Lens as Lens
import           Data.Functor.Const (Const(..))
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/|/))
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.Responsive.Expression as ResponsiveExpr
import           GUI.Momentu.State (Gui)
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Payload as ExprGui
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrapParentExpr)
import           Lamdu.GUI.Styled (text, grammar)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.I18N.Texts (Texts)
import qualified Lamdu.I18N.Texts as Texts
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

injectIndicator ::
    ( MonadReader env f, TextView.HasStyle env, HasTheme env
    , Element.HasAnimIdPrefix env, Texts.HasTexts env
    ) => (forall a. Lens.ALens' (Texts a) a) -> f (WithTextPos View)
injectIndicator l = grammar (text ["injectIndicator"] l)

makeInject ::
    (Monad i, Monad o) =>
    ExprGui.SugarExpr i o ->
    Sugar.Tag (Name o) i o ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive o)
makeInject val tag pl =
    stdWrapParentExpr pl <*>
    do
        arg <- ExprGuiM.makeSubexpression val
        config <- Lens.view Config.config
        let replaceParentEventMap replaceParent =
                -- Deleting the inject is replacing the whole expr
                -- with the injected value "child"
                replaceParent <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys config) delDoc

        (ResponsiveExpr.boxSpacedMDisamb ?? ExprGui.mParensId pl)
            <*>
            ( TagEdit.makeVariantTag tag
                /|/ injectIndicator (Texts.code . Texts.inject)
                <&> Lens.mapped %~ Widget.weakerEvents (foldMap replaceParentEventMap mReplaceParent)
                <&> Responsive.fromWithTextPos
                <&> (: [arg])
            )
    where
        delDoc = E.Doc ["Edit", "Delete"]
        mReplaceParent = val ^. ann . Sugar.plActions . Sugar.mReplaceParent

emptyRec ::
    Tree (Ann a) (Const (Sugar.NullaryVal name i o)) ->
    Sugar.Expression name i o a
emptyRec (Ann pl (Const (Sugar.NullaryVal closedActions addItem))) =
    Sugar.Composite [] (Sugar.ClosedComposite closedActions) addItem
    & Sugar.BodyRecord
    & Ann pl

makeNullaryInject ::
    (Monad i, Monad o) =>
    Tree (Ann (Sugar.Payload (Name o) i o ExprGui.Payload))
    (Const (Sugar.NullaryVal (Name o) i o)) ->
    Sugar.Tag (Name o) i o ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive o)
makeNullaryInject nullary tag pl =
    GuiState.isSubCursor ?? nullaryRecEntityId
    >>= \case
    True -> makeInject (emptyRec nullary) tag pl
    False ->
        stdWrapParentExpr pl <*>
        (TagEdit.makeVariantTag tag /|/ injectIndicator (Texts.code . Texts.nullaryInject)
            <&> Responsive.fromWithTextPos
            <&> Widget.weakerEvents expandNullaryVal)
    where
        expandNullaryVal =
            GuiState.updateCursor nullaryRecEntityId & pure & const
            & E.charGroup Nothing (E.Doc ["Edit", "Inject", "Value"]) ":"
        nullaryRecEntityId =
            nullary ^. ann . Sugar.plEntityId
            & WidgetIds.fromEntityId

make ::
    (Monad i, Monad o) =>
    Tree (Sugar.Inject (Name o) i o)
        (Ann (Sugar.Payload (Name o) i o ExprGui.Payload)) ->
    Sugar.Payload (Name o) i o ExprGui.Payload ->
    ExprGuiM i o (Gui Responsive o)
make (Sugar.Inject tag mVal) =
    case mVal of
    Sugar.InjectNullary nullary -> makeNullaryInject nullary tag
    Sugar.InjectVal val -> makeInject val tag
