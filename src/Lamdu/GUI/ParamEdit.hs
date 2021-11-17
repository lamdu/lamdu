{-# LANGUAGE TemplateHaskell #-}

module Lamdu.GUI.ParamEdit
    ( Info(..), iNameEdit, iDel, iAddNext, iMOrderBefore, iMOrderAfter, iId
    , make, addAnnotation, paramDelEventMap, eventMapAddNextParam, addAddParam
    , eventMapAddFirstParam, mkParamPickResult
    ) where

import qualified Control.Lens as Lens
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

data Info i o = Info
    { _iNameEdit :: TextWidget o
    , _iDel :: o ()
    , _iAddNext :: Maybe (Sugar.AddNextParam Name i o)
    , _iMOrderBefore :: Maybe (o ())
    , _iMOrderAfter :: Maybe (o ())
    , _iId :: Widget.Id
    }
Lens.makeLenses ''Info

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
    _ => env -> Widget.Id -> Sugar.AddNextParam name i o -> EventMap (o GuiState.Update)
eventMapAddNextParam env myId addNext =
    E.keysEventMapMovesCursor (env ^. has . Config.addNextParamKeys)
    (E.toDoc env [has . MomentuTexts.edit, has . doc]) (pure dst)
    where
        (dst, doc) =
            case addNext of
            Sugar.AddNext{} ->
                (TagEdit.addParamId myId, Texts.addNextParameter)
            Sugar.NeedToPickTagToAddNext x ->
                ( WidgetIds.tagHoleId (WidgetIds.fromEntityId x)
                , Texts.nameFirstParameter
                )

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

-- exported for use in definition sugaring.
make ::
    _ =>
    Annotation.EvalAnnotationOptions ->
    Widget.Id -> Widget.Id ->
    (Sugar.FuncParam (Sugar.Annotation (Sugar.EvaluationScopes Name i) Name), Info i o) ->
    GuiM env i o [Responsive o]
make annotationOpts prevId nextId (param, info) =
    do
        env <- Lens.view id
        let paramEventMap =
                mconcat
                [ paramDelEventMap env (info ^. iDel) prevId nextId
                , foldMap (eventMapAddNextParam env myId) (info ^. iAddNext)
                , foldMap (eventMapOrderParam env Config.paramOrderBeforeKeys Texts.moveBefore) (info ^. iMOrderBefore)
                , foldMap (eventMapOrderParam env Config.paramOrderAfterKeys Texts.moveAfter) (info ^. iMOrderAfter)
                ]
        paramEdit <-
            addAnnotation annotationOpts param myId (info ^. iNameEdit)
            <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ (<> paramEventMap)
            & local (M.animIdPrefix .~ Widget.toAnimId myId)
        case info ^? iAddNext . Lens._Just . Sugar._AddNext of
            Nothing -> pure [paramEdit]
            Just addNext -> addAddParam addNext myId paramEdit
    where
        myId = info ^. iId
