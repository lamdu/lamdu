module Lamdu.GUI.ParamEdit
    ( makeParams, addAnnotation, paramDelEventMap, eventMapAddNextParamOrPickTag, addAddParam
    , eventMapAddFirstParam, mkParamPickResult
    ) where

import qualified Control.Lens as Lens
import           Data.List.Extended (withPrevNext)
import qualified GUI.Momentu as M
import           GUI.Momentu.Align (TextWidget)
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.I18N as MomentuTexts
import           GUI.Momentu.ModKey (ModKey, noMods)
import           GUI.Momentu.Responsive (Responsive)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.Expr.TagEdit as TagEdit
import qualified Lamdu.GUI.Annotation as Annotation
import           Lamdu.GUI.Monad (GuiM, im)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.I18N.CodeUI as Texts
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

eventMapAddFirstParam ::
    _ => Widget.Id -> Sugar.AddFirstParam name i o -> m (EventMap (o GuiState.Update))
eventMapAddFirstParam binderId addFirst =
    Lens.view id
    <&>
    \env ->
    E.keysEventMapMovesCursor (env ^. has . Config.addNextParamKeys)
    (E.toDoc env [has . MomentuTexts.edit, has . doc]) action
    where
        enterParam = WidgetIds.tagHoleId . WidgetIds.fromEntityId
        (action, doc) =
            case addFirst of
            Sugar.NeedToPickTagToAddFirst x ->
                (pure (enterParam x), Texts.nameFirstParameter)
            Sugar.PrependParam{} ->
                (pure (TagEdit.addParamId binderId), Texts.addParameter)
            Sugar.AddInitialParam x ->
                (x <&> enterParam, Texts.addParameter)

eventMapAddNextParam ::
    _ => env -> Widget.Id -> EventMap (o GuiState.Update)
eventMapAddNextParam env myId =
    E.keysEventMapMovesCursor (env ^. has . Config.addNextParamKeys)
    (E.toDoc env [has . MomentuTexts.edit, has . Texts.addNextParameter])
    (pure (TagEdit.addParamId myId))

eventMapAddNextParamOrPickTag ::
    _ => env -> Widget.Id -> Sugar.AddNextParam name i o -> EventMap (o GuiState.Update)
eventMapAddNextParamOrPickTag env myId Sugar.AddNext{} =
    eventMapAddNextParam env myId
eventMapAddNextParamOrPickTag env _ (Sugar.NeedToPickTagToAddNext x) =
    E.keysEventMapMovesCursor (env ^. has . Config.addNextParamKeys)
    (E.toDoc env [has . MomentuTexts.edit, has . Texts.nameFirstParameter]) (pure (WidgetIds.tagHoleId (WidgetIds.fromEntityId x)))

eventMapOrderParam ::
    _ =>
    env ->
    Lens.ALens' (Config ModKey) [ModKey] ->
    Lens.ALens' (Texts.CodeUI Text) Text -> m () ->
    EventMap (m GuiState.Update)
eventMapOrderParam env keys moveDoc =
    E.keysEventMap (env ^# has . keys)
    (E.toDoc env
        [has . MomentuTexts.edit, has . Texts.parameter, has . moveDoc])

paramDelEventMap ::
    _ => env -> m () -> Widget.Id -> Widget.Id -> EventMap (m GuiState.Update)
paramDelEventMap env fpDel prevId nextId =
    dir Config.delBackwardKeys Texts.deleteParameterBackwards prevId <>
    dir Config.delForwardKeys Texts.deleteParameter nextId
    where
        dir keys delParam dstPosId =
            GuiState.updateCursor dstPosId <$ fpDel
            & E.keyPresses (env ^. has . keys) (E.toDoc env [has . MomentuTexts.edit, has . delParam])

mkParamPickResult :: Sugar.EntityId -> Menu.PickResult
mkParamPickResult tagInstance =
    Menu.PickResult
    { Menu._pickDest = WidgetIds.fromEntityId tagInstance
    , Menu._pickMNextEntry =
        WidgetIds.fromEntityId tagInstance & TagEdit.addParamId & Just
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

addAddParam :: _ => i (Sugar.TagChoice Name a) -> Widget.Id -> Responsive a -> GuiM env i a [Responsive a]
addAddParam addParam myId paramEdit =
    GuiState.isSubCursor ?? addId >>=
    \case
    False -> pure [paramEdit]
    True ->
        addParam & im
        >>= TagEdit.makeTagHoleEdit mkParamPickResult addId
        & local (has . Menu.configKeysPickOptionAndGotoNext <>~ [noMods M.Key'Space])
        & Styled.withColor TextColors.parameterColor
        <&> Responsive.fromWithTextPos
        <&> (:[]) <&> (paramEdit :)
    where
        addId = TagEdit.addParamId myId

itemId :: Sugar.TaggedItem name i o a -> Widget.Id
itemId item = item ^. Sugar.tiTag . Sugar.tagRefTag . Sugar.tagInstance & WidgetIds.fromEntityId

makeTagParamEdit ::
    _ =>
    Annotation.EvalAnnotationOptions ->
    Sugar.TaggedItem Name i o (Sugar.FuncParam (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name)) ->
    GuiM env i o (Sugar.TaggedItem Name i o (Responsive o))
makeTagParamEdit annotationOpts item =
    do
        nameEdit <-
            TagEdit.makeParamTag Nothing (item ^. Sugar.tiTag)
            >>= addAnnotation annotationOpts (item ^. Sugar.tiValue) myId
        eventMap <- Lens.view id <&> eventMapAddNextParam ?? itemId item
        item & Sugar.tiValue .~ M.weakerEvents eventMap nameEdit & pure
    & local (M.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = itemId item

makeParams ::
    _ =>
    Annotation.EvalAnnotationOptions ->
    Widget.Id -> Widget.Id ->
    Sugar.TaggedListBody Name i o (Sugar.FuncParam (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name)) ->
    GuiM env i o [Responsive o]
makeParams annotationOpts prevId nextId items =
    do
        infos <-
            (:)
            <$> makeTagParamEdit annotationOpts (items ^. Sugar.tlHead)
            <*> traverse swapable (items ^. Sugar.tlTail)
        let orderAfters =
                (items ^.. Sugar.tlTail . traverse . Sugar.tsiSwapWithPrevious <&> Just) <> [Nothing]
        env <- Lens.view id
        let addOrderAfter Nothing = id
            addOrderAfter (Just orderAfter) =
                Sugar.tiValue %~
                M.weakerEvents (eventMapOrderParam env Config.paramOrderAfterKeys Texts.moveAfter orderAfter)
        zipWith addOrderAfter orderAfters infos
            & withPrevNext prevId nextId itemId
            & traverse make
    <&> concat
    where
        swapable item =
            do
                eventMap <-
                    Lens.view id <&>
                    \env -> eventMapOrderParam env Config.paramOrderBeforeKeys Texts.moveBefore (item ^. Sugar.tsiSwapWithPrevious)
                makeTagParamEdit annotationOpts (item ^. Sugar.tsiItem)
                    <&> Sugar.tiValue %~ M.weakerEvents eventMap
        make (p, n, item) =
            do
                env <- Lens.view id
                M.weakerEvents (paramDelEventMap env (() <$ item ^. Sugar.tiDelete) p n) (item ^. Sugar.tiValue)
                    & addAddParam (item ^. Sugar.tiAddAfter) (itemId item)
