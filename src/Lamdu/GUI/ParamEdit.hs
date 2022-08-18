module Lamdu.GUI.ParamEdit
    ( addAnnotationAndEvents, addAnnotation, eventMapAddNextParamOrPickTag, mkAddParam, makeParam
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu (Responsive, EventMap, TextWidget, noMods, Update)
import qualified GUI.Momentu as M
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Classes as C
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import qualified Lamdu.GUI.Annotation as Annotation
import           Lamdu.GUI.Monad (GuiM)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TaggedList as TaggedList
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import qualified Lamdu.I18N.Navigation as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

eventMapAddNextParamOrPickTag ::
    _ => Widget.Id -> Sugar.AddParam name i o -> m (EventMap (o Update))
eventMapAddNextParamOrPickTag myId Sugar.AddNext{} =
    Lens.view (has . Config.addNextParamKeys) >>=
    (TaggedList.addNextEventMap (has . Texts.parameter) ?? myId)
eventMapAddNextParamOrPickTag _ (Sugar.NeedToPickTagToAddNext x) =
    Lens.view id <&>
    \env ->
    E.keysEventMapMovesCursor (env ^. has . Config.addNextParamKeys)
    (E.toDoc env [has . MomentuTexts.edit, has . Texts.nameFirstParameter]) (pure (WidgetIds.tagHoleId (WidgetIds.fromEntityId x)))

mkParamPickResult :: Sugar.EntityId -> Menu.PickResult
mkParamPickResult tagInstance =
    Menu.PickResult
    { Menu._pickDest = WidgetIds.fromEntityId tagInstance
    , Menu._pickMNextEntry =
        WidgetIds.fromEntityId tagInstance & TagEdit.addItemId & Just
    }

addAnnotation ::
    _ =>
    Annotation.EvalAnnotationOptions ->
    Sugar.FuncParam (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) ->
    Widget.Id -> TextWidget o ->
    GuiM env i o (Responsive o)
addAnnotation annotationOpts param myId widget =
    do
        postProcessAnnotation <-
            GuiState.isSubCursor ?? myId
            <&> Annotation.postProcessAnnotationFromSelected
        Annotation.maybeAddAnnotationWith annotationOpts postProcessAnnotation
            (param ^. Sugar.fpAnnotation)
            <&> (Widget.widget %~)
            ?? Responsive.fromWithTextPos widget
    & local (M.animIdPrefix .~ Widget.toAnimId myId)

addAnnotationAndEvents ::
    _ =>
    Annotation.EvalAnnotationOptions ->
    Sugar.FuncParam (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name) ->
    Widget.Id -> TextWidget o ->
    GuiM env i o (Responsive o)
addAnnotationAndEvents annotationOpts param myId widget =
    do
        env <- Lens.view id
        let eventMap =
                foldMap
                ( E.keysEventMapMovesCursor (env ^. has . Config.inlineKeys)
                    (E.toDoc env
                        [ has . MomentuTexts.navigation
                        , has . Texts.jumpToFirstUse
                        ])
                    . pure . WidgetIds.fromEntityId
                ) (param ^? Sugar.fpUsages . traverse)
        addAnnotation annotationOpts param myId widget
            <&> M.weakerEvents eventMap

mkAddParam :: _ => i (Sugar.TagChoice Name o) -> Widget.Id -> m [Responsive o]
mkAddParam addParam myId =
    GuiState.isSubCursor ?? addId >>=
    \case
    False -> pure []
    True ->
        addParam & C.liftInfo
        >>= TagEdit.makeTagHoleEdit mkParamPickResult addId
        & local (has . Menu.configKeysPickOptionAndGotoNext <>~ [noMods M.Key'Space])
        & Styled.withColor TextColors.variableColor
        <&> Responsive.fromWithTextPos
        <&> (:[])
    where
        addId = TagEdit.addItemId myId

makeParam ::
    _ =>
    Annotation.EvalAnnotationOptions ->
    TaggedList.Item Name i o (Sugar.LhsField Name (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name)) ->
    GuiM env i o [Responsive o]
makeParam annotationOpts item =
    (TagEdit.makeParamTag Nothing (item ^. TaggedList.iTag)
        >>= addAnnotationAndEvents annotationOpts (item ^. TaggedList.iValue . Sugar.fParam) myId
        <&> M.weakerEvents (item ^. TaggedList.iEventMap) <&> (:[]))
    <> mkAddParam (item ^. TaggedList.iAddAfter) myId
    & local (M.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = TaggedList.itemId (item ^. TaggedList.iTag)
