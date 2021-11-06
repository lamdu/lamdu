{-# LANGUAGE TemplateHaskell #-}

module Lamdu.GUI.ParamEdit
    ( Info(..), iNameEdit, iDel, iAddNext, iMOrderBefore, iMOrderAfter, iId
    , make
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
import           Lamdu.GUI.Monad (GuiM)
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

eventParamDelEventMap ::
    _ =>
    env -> m () ->
    Lens.ALens' (Config ModKey) [ModKey] ->
    Lens.ALens' (Texts.CodeUI Text) Text -> Widget.Id -> EventMap (m GuiState.Update)
eventParamDelEventMap env fpDel keys delParam dstPosId =
    GuiState.updateCursor dstPosId <$ fpDel
    & E.keyPresses (env ^# has . keys)
        (E.toDoc env [has . MomentuTexts.edit, has . delParam])

mkParamPickResult :: Sugar.EntityId -> Menu.PickResult
mkParamPickResult tagInstance =
    Menu.PickResult
    { Menu._pickDest = WidgetIds.fromEntityId tagInstance
    , Menu._pickMNextEntry =
        WidgetIds.fromEntityId tagInstance & TagEdit.addParamId & Just
    }

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
                [ eventParamDelEventMap env (info ^. iDel) Config.delForwardKeys Texts.deleteParameter nextId
                , eventParamDelEventMap env (info ^. iDel) Config.delBackwardKeys Texts.deleteParameterBackwards prevId
                , foldMap (eventMapAddNextParam env myId) (info ^. iAddNext)
                , foldMap (eventMapOrderParam env Config.paramOrderBeforeKeys Texts.moveBefore) (info ^. iMOrderBefore)
                , foldMap (eventMapOrderParam env Config.paramOrderAfterKeys Texts.moveAfter) (info ^. iMOrderAfter)
                ]
        postProcessAnnotation <-
            GuiState.isSubCursor ?? myId
            <&> Annotation.postProcessAnnotationFromSelected
        paramEdit <-
            Annotation.maybeAddAnnotationWith annotationOpts
            postProcessAnnotation
            (param ^. Sugar.fpAnnotation)
            <&> (Widget.widget %~)
            ?? Responsive.fromWithTextPos (info ^. iNameEdit)
            <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ (<> paramEventMap)
            & local (M.animIdPrefix .~ Widget.toAnimId myId)
        mAddParam <-
            GuiState.isSubCursor ?? addId
            <&> guard
            <&> (>> (info ^? iAddNext . Lens._Just . Sugar._AddNext))
        addParamEdits <-
            case mAddParam of
            Nothing -> pure []
            Just addParam ->
                TagEdit.makeTagHoleEdit addParam mkParamPickResult addId
                & local (has . Menu.configKeysPickOptionAndGotoNext <>~ [noMods M.Key'Space])
                & Styled.withColor TextColors.parameterColor
                <&> Responsive.fromWithTextPos
                <&> (:[])
        paramEdit : addParamEdits & pure
    where
        myId = info ^. iId
        addId = TagEdit.addParamId myId
