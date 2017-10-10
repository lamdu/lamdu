{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}
module Lamdu.GUI.ExpressionEdit.RecordEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Animation as Anim
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.View as View
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionEdit.Composite (destCursorId)
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

shouldAddBg :: Sugar.Composite name m a -> Bool
shouldAddBg (Sugar.Composite [] Sugar.ClosedComposite{} _) = False
shouldAddBg _ = True

doc :: E.Subtitle -> E.Doc
doc text = E.Doc ["Edit", "Record", text]

make ::
    Monad m =>
    Sugar.Composite (Name (T m)) (T m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload (T m) ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make record@(Sugar.Composite fields recordTail addField) pl =
    do
        config <- Lens.view Config.config
        let eventMap =
                case recordTail of
                Sugar.ClosedComposite actions ->
                    closedRecordEventMap config actions
                Sugar.OpenComposite actions restExpr ->
                    openRecordEventMap config actions restExpr
        do
            (gui, resultPicker) <-
                ExprGuiM.listenResultPicker $
                do
                    fieldsGui <- makeFieldsWidget fields myId
                    case recordTail of
                        Sugar.ClosedComposite _ -> pure fieldsGui
                        Sugar.OpenComposite actions rest ->
                            makeOpenRecord fieldsGui actions rest (Widget.toAnimId myId)
            let addFieldEventMap =
                    addField
                    <&> (^. Sugar.cairNewTag . Sugar.tagInstance)
                    <&> WidgetIds.fromEntityId
                    <&> TagEdit.tagHoleId
                    & Widget.keysEventMapMovesCursor (Config.recordAddFieldKeys config)
                      (doc "Add Field")
                    & ExprGuiM.withHolePicker resultPicker
            (if addBg then ExpressionGui.addValFrame else return id)
                ?? E.weakerEvents addFieldEventMap gui
            & wrap
            <&> E.weakerEvents eventMap
    where
        haveChildren = Lens.has Lens.folded record
        defaultDestCursor = destCursorId fields (pl ^. Sugar.plEntityId)
        wrap
            | haveChildren = ExpressionGui.stdWrapParentExpr pl defaultDestCursor
            | otherwise = ExpressionGui.stdWrap pl
        myId = WidgetIds.fromExprPayload pl
        addBg = shouldAddBg record

makeFieldRow ::
    Monad m =>
    Sugar.CompositeItem (Name (T m)) (T m) (Sugar.Expression (Name (T m)) (T m) ExprGuiT.Payload) ->
    ExprGuiM m
    ( WithTextPos (Widget (T m Widget.EventResult))
    , ExpressionGui m
    )
makeFieldRow (Sugar.CompositeItem delete tag fieldExpr) =
    do
        config <- Lens.view Config.config
        let itemEventMap = recordDelEventMap config delete
        tagLabel <-
            TagEdit.makeRecordTag TagEdit.WithTagHoles (ExprGuiT.nextHolesBefore fieldExpr) tag
            <&> Align.tValue %~ E.weakerEvents itemEventMap
        hspace <- Spacer.stdHSpace
        fieldGui <- ExprGuiM.makeSubexpression fieldExpr
        return (tagLabel /|/ hspace, E.weakerEvents itemEventMap fieldGui)

makeFieldsWidget ::
    Monad m =>
    [Sugar.CompositeItem (Name (T m)) (T m) (Sugar.Expression (Name (T m)) (T m) ExprGuiT.Payload)] ->
    Widget.Id -> ExprGuiM m (ExpressionGui m)
makeFieldsWidget fields myId =
    do
        opener <- ExpressionGui.grammarLabel "{"
        closer <- ExpressionGui.grammarLabel "}"
        case fields of
            [] ->
                (Widget.makeFocusableView ?? myId <&> (Align.tValue %~))
                ?? (opener /|/ closer)
                <&> Responsive.fromWithTextPos
            _ ->
                do
                    xs <- Responsive.taggedList <*> mapM makeFieldRow fields
                    opener /|/ xs /|/ closer & return

separationBar :: Theme.CodeForegroundColors -> Widget.R -> Anim.AnimId -> View
separationBar theme width animId =
    View.unitSquare (animId <> ["tailsep"])
    & Element.tint (Theme.recordTailColor theme)
    & Element.scale (Vector2 width 10)

makeOpenRecord ::
    Monad m =>
    ExpressionGui m -> Sugar.OpenCompositeActions (T m) -> ExprGuiT.SugarExpr m ->
    AnimId -> ExprGuiM m (ExpressionGui m)
makeOpenRecord fieldsGui (Sugar.OpenCompositeActions close) rest animId =
    do
        theme <- Lens.view Theme.theme
        vspace <- Spacer.stdVSpace
        restExpr <- ExpressionGui.addValPadding <*> ExprGuiM.makeSubexpression rest
        config <- Lens.view Config.config
        let restEventMap =
                close <&> WidgetIds.fromEntityId
                & Widget.keysEventMapMovesCursor (Config.delKeys config) (doc "Close")
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
                        <&> E.weakerEvents restEventMap
                    minWidth = restW ^. Element.width
                    targetWidth = fields ^. Element.width
        fieldsGui & Responsive.render . Lens.imapped %@~ layout & pure

openRecordEventMap ::
    Functor m =>
    Config -> Sugar.OpenCompositeActions (T m) ->
    Sugar.Expression name (T m) a ->
    Widget.EventMap (T m Widget.EventResult)
openRecordEventMap config (Sugar.OpenCompositeActions close) restExpr
    | isHole restExpr =
        close <&> WidgetIds.fromEntityId
        & Widget.keysEventMapMovesCursor (Config.recordCloseKeys config) (doc "Close")
    | otherwise = mempty
    where
        isHole = Lens.has (Sugar.rBody . Sugar._BodyHole)

closedRecordEventMap ::
    Functor m =>
    Config -> Sugar.ClosedCompositeActions (T m) ->
    Widget.EventMap (T m Widget.EventResult)
closedRecordEventMap config (Sugar.ClosedCompositeActions open) =
    open <&> WidgetIds.fromEntityId
    & Widget.keysEventMapMovesCursor (Config.recordOpenKeys config) (doc "Open")

recordDelEventMap ::
    Functor m =>
    Config -> m Sugar.EntityId -> Widget.EventMap (m Widget.EventResult)
recordDelEventMap config delete =
    delete <&> WidgetIds.fromEntityId
    & Widget.keysEventMapMovesCursor (Config.delKeys config) (doc "Delete Field")
