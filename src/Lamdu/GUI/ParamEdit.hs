module Lamdu.GUI.ParamEdit
    ( makeParams, addAnnotation, eventMapAddNextParamOrPickTag, mkAddParam
    , mkParamPickResult
    ) where

import qualified Control.Lens as Lens
import qualified GUI.Momentu as M
import           GUI.Momentu.Align (TextWidget)
import           GUI.Momentu.Direction (Orientation(..), Order(..))
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.ModKey (noMods)
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import           GUI.Momentu.Widgets.StdKeys (dirKey)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import qualified Lamdu.GUI.Annotation as Annotation
import           Lamdu.GUI.Monad (GuiM, im)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.TaggedList as TaggedList
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

eventMapAddNextParamOrPickTag ::
    _ => Widget.Id -> Sugar.AddParam name i o -> m (EventMap (o GuiState.Update))
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

mkAddParam :: _ => i (Sugar.TagChoice Name a) -> Widget.Id -> GuiM env i a [Responsive a]
mkAddParam addParam myId =
    GuiState.isSubCursor ?? addId >>=
    \case
    False -> pure []
    True ->
        addParam & im
        >>= TagEdit.makeTagHoleEdit mkParamPickResult addId
        & local (has . Menu.configKeysPickOptionAndGotoNext <>~ [noMods M.Key'Space])
        & Styled.withColor TextColors.parameterColor
        <&> Responsive.fromWithTextPos
        <&> (:[])
    where
        addId = TagEdit.addItemId myId

makeParam ::
    _ =>
    Annotation.EvalAnnotationOptions ->
    TaggedList.Item Name i o (Sugar.FuncParam (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name)) ->
    GuiM env i o [Responsive o]
makeParam annotationOpts item =
    (<>)
    <$> (TagEdit.makeParamTag Nothing (item ^. TaggedList.iTag)
            >>= addAnnotation annotationOpts (item ^. TaggedList.iValue) myId
            <&> M.weakerEvents (item ^. TaggedList.iEventMap) <&> (:[]))
    <*> mkAddParam (item ^. TaggedList.iAddAfter) myId
    & local (M.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = TaggedList.itemId (item ^. TaggedList.iTag)

makeParams ::
    _ =>
    Annotation.EvalAnnotationOptions ->
    Widget.Id -> Widget.Id ->
    Sugar.TaggedList Name i o (Sugar.FuncParam (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name)) ->
    GuiM env i o (EventMap (o GuiState.Update), [Responsive o])
makeParams annotationOpts prevId nextId items =
    do
        o <- Lens.view has <&> \d -> Lens.cloneLens . dirKey d Horizontal
        keys <-
            traverse Lens.view TaggedList.Keys
            { TaggedList._kAdd = has . Config.addNextParamKeys
            , TaggedList._kOrderBefore = has . Config.orderDirKeys . o Backward
            , TaggedList._kOrderAfter = has . Config.orderDirKeys . o Forward
            }
        (addFirstEventMap, itemsR) <- TaggedList.make (has . Texts.parameter) keys prevId nextId items
        (<>)
            <$> mkAddParam (items ^. Sugar.tlAddFirst) prevId
            <*> (traverse (makeParam annotationOpts) itemsR <&> concat)
            <&> (,) addFirstEventMap
