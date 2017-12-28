{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}
module Lamdu.GUI.ExpressionEdit.RecordEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import qualified GUI.Momentu.Align as Align
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Wrap (stdWrap, stdWrapParentExpr)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

doc :: E.Subtitle -> E.Doc
doc text = E.Doc ["Edit", "Record", text]

mkAddFieldEventMap ::
    Functor f =>
    Config -> f Sugar.CompositeAddItemResult -> EventMap (f GuiState.Update)
mkAddFieldEventMap config addField =
    addField
    <&> (^. Sugar.cairNewTag . Sugar.tagInstance)
    <&> WidgetIds.fromEntityId
    <&> WidgetIds.tagHoleId
    & E.keysEventMapMovesCursor (Config.recordAddFieldKeys config)
      (doc "Add Field")

makeUnit ::
    Monad m =>
    Sugar.ClosedCompositeActions (T m) -> T m Sugar.CompositeAddItemResult ->
    Sugar.Payload (T m) ExprGui.Payload -> ExprGuiM m (ExpressionGui m)
makeUnit _actions addField pl =
    do
        config <- Lens.view Config.config
        makeFocusable <- Widget.makeFocusableView ?? myId <&> (Align.tValue %~)
        (/|/) <$> Styled.grammarLabel "{" <*> Styled.grammarLabel "}"
            <&> makeFocusable
            <&> Align.tValue %~ Widget.weakerEvents (mkAddFieldEventMap config addField)
            <&> Responsive.fromWithTextPos
    -- Don't add the closedRecordEventMap (_actions) - it only adds the open
    -- action which is equivalent ot deletion on the unit record
    & stdWrap pl
    where
        myId = WidgetIds.fromExprPayload pl

make ::
    Monad m =>
    Sugar.Composite (Name (T m)) (T m) (ExprGui.SugarExpr m) ->
    Sugar.Payload (T m) ExprGui.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.Composite [] (Sugar.ClosedComposite actions) addField) pl =
    makeUnit actions addField pl
make (Sugar.Composite fields recordTail addField) pl =
    do
        config <- Lens.view Config.config
        let eventMap =
                case recordTail of
                Sugar.ClosedComposite actions ->
                    closedRecordEventMap config actions
                Sugar.OpenComposite actions restExpr ->
                    openRecordEventMap config actions restExpr
        let addFieldEventMap = mkAddFieldEventMap config addField
        makeRecord fields addFieldEventMap postProcess
            & stdWrapParentExpr pl
            <&> Widget.weakerEvents eventMap
    where
        postProcess =
            case recordTail of
            Sugar.OpenComposite actions restExpr ->
                makeOpenRecord actions restExpr
            _ -> pure

makeRecord ::
    Monad m =>
    [Sugar.CompositeItem (Name (T m)) (T m) (ExprGui.SugarExpr m)] ->
    EventMap (T m GuiState.Update) ->
    (ExpressionGui m -> ExprGuiM m (ExpressionGui m)) ->
    ExprGuiM m (ExpressionGui m)
makeRecord fields addFieldEventMap postProcess =
    Styled.addValFrame <*>
    do
        opener <- Styled.grammarLabel "{"
        closer <- Styled.grammarLabel "}"
        case fields of
            [] -> Responsive.fromTextView closer & pure
            _ ->
                Responsive.taggedList
                <*> ( mapM makeFieldRow fields
                    <&> Lens.reversed . Lens.ix 0 . Responsive.tagPost .~ (closer <&> Widget.fromView)
                    )
                >>= postProcess
                <&> Widget.weakerEvents addFieldEventMap
            <&> (opener /|/)

makeFieldRow ::
    Monad m =>
    Sugar.CompositeItem (Name (T m)) (T m) (ExprGui.SugarExpr m) ->
    ExprGuiM m (Responsive.TaggedItem (T m GuiState.Update))
makeFieldRow (Sugar.CompositeItem delete tag fieldExpr) =
    do
        config <- Lens.view Config.config
        let itemEventMap = recordDelEventMap config delete
        tagLabel <-
            TagEdit.makeRecordTag (ExprGui.nextHolesBefore fieldExpr) tag
            <&> Align.tValue %~ Widget.weakerEvents itemEventMap
        hspace <- Spacer.stdHSpace
        fieldGui <- ExprGuiM.makeSubexpression fieldExpr
        pure Responsive.TaggedItem
            { Responsive._tagPre = tagLabel /|/ hspace
            , Responsive._taggedItem = Widget.weakerEvents itemEventMap fieldGui
            , Responsive._tagPost = Element.empty
            }

separationBar :: Theme.CodeForegroundColors -> Widget.R -> Anim.AnimId -> View
separationBar theme width animId =
    View.unitSquare (animId <> ["tailsep"])
    & Element.tint (Theme.recordTailColor theme)
    & Element.scale (Vector2 width 10)

makeOpenRecord ::
    Monad m =>
    Sugar.OpenCompositeActions (T m) -> ExprGui.SugarExpr m ->
    ExpressionGui m -> ExprGuiM m (ExpressionGui m)
makeOpenRecord (Sugar.OpenCompositeActions close) rest fieldsGui =
    do
        theme <- Lens.view Theme.theme
        vspace <- Spacer.stdVSpace
        restExpr <- Styled.addValPadding <*> ExprGuiM.makeSubexpression rest
        config <- Lens.view Config.config
        let restEventMap =
                close <&> WidgetIds.fromEntityId
                & E.keysEventMapMovesCursor (Config.delKeys config) (doc "Close")
        animId <- Lens.view Element.animIdPrefix
        let layout layoutMode fields =
                fields
                /-/
                separationBar (Theme.codeForegroundColors theme) (max minWidth targetWidth) animId
                /-/
                vspace
                /-/
                restW
                where
                    restW =
                        (restExpr ^. Responsive.render) layoutMode
                        <&> Widget.weakerEvents restEventMap
                    minWidth = restW ^. Element.width
                    targetWidth = fields ^. Element.width
        fieldsGui & Responsive.render . Lens.imapped %@~ layout & pure

openRecordEventMap ::
    Functor m =>
    Config -> Sugar.OpenCompositeActions (T m) ->
    Sugar.Expression name (T m) a ->
    EventMap (T m GuiState.Update)
openRecordEventMap config (Sugar.OpenCompositeActions close) restExpr
    | isHole restExpr =
        close <&> WidgetIds.fromEntityId
        & E.keysEventMapMovesCursor (Config.recordCloseKeys config) (doc "Close")
    | otherwise = mempty
    where
        isHole = Lens.has (Sugar.rBody . Sugar._BodyHole)

closedRecordEventMap ::
    Functor m =>
    Config -> Sugar.ClosedCompositeActions (T m) ->
    EventMap (T m GuiState.Update)
closedRecordEventMap config (Sugar.ClosedCompositeActions open) =
    open <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (Config.recordOpenKeys config) (doc "Open")

recordDelEventMap ::
    Functor m =>
    Config -> m Sugar.EntityId -> EventMap (m GuiState.Update)
recordDelEventMap config delete =
    delete <&> WidgetIds.fromEntityId
    & E.keysEventMapMovesCursor (Config.delKeys config) (doc "Delete Field")
