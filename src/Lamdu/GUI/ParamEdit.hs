{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ParamEdit
    ( Info(..), make
    , eventMapAddFirstParam, mkParamPickResult
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.MetaKey (MetaKey, toModKey)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.Menu as Menu
import           Lamdu.Config (Config, HasConfig(..))
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme.TextColors as TextColors
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import qualified Lamdu.GUI.ExpressionGui.Annotation as Annotation
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.Styled as Styled
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Name (Name)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

eventMapAddFirstParam ::
    (MonadReader env m, Applicative am, HasConfig env) =>
    Widget.Id ->
    Sugar.AddFirstParam name im am ->
    m (EventMap (am GuiState.Update))
eventMapAddFirstParam binderId addFirst =
    Lens.view (Config.config . Config.addNextParamKeys)
    <&>
    \keys ->
    E.keysEventMapMovesCursor keys (E.Doc ["Edit", doc]) action
    where
        enterParam = WidgetIds.tagHoleId . WidgetIds.fromEntityId
        (action, doc) =
            case addFirst of
            Sugar.NeedToPickTagToAddFirst x -> (pure (enterParam x), "Name first parameter")
            Sugar.PrependParam{} -> (pure (TagEdit.addParamId binderId), "Add parameter")
            Sugar.AddInitialParam x -> (x <&> enterParam, "Add parameter")

eventMapAddNextParam ::
    Applicative am =>
    Config -> Widget.Id -> Sugar.AddNextParam name im am ->
    EventMap (am GuiState.Update)
eventMapAddNextParam conf myId addNext =
    E.keysEventMapMovesCursor (conf ^. Config.addNextParamKeys)
    (E.Doc ["Edit", doc]) (pure dst)
    where
        (dst, doc) =
            case addNext of
            Sugar.AddNext{} -> (TagEdit.addParamId myId, "Add next parameter")
            Sugar.NeedToPickTagToAddNext x ->
                (WidgetIds.tagHoleId (WidgetIds.fromEntityId x), "Name first parameter")

eventMapOrderParam ::
    Monad m =>
    [MetaKey] -> Text -> m () -> EventMap (m GuiState.Update)
eventMapOrderParam keys docSuffix =
    E.keysEventMap keys (E.Doc ["Edit", "Parameter", "Move " <> docSuffix])

eventParamDelEventMap ::
    Monad m => m () -> [MetaKey] -> Text -> Widget.Id -> EventMap (m GuiState.Update)
eventParamDelEventMap fpDel keys docSuffix dstPosId =
    GuiState.updateCursor dstPosId <$ fpDel
    & E.keyPresses (keys <&> toModKey)
        (E.Doc ["Edit", "Delete parameter" <> docSuffix])

data Info im am = Info
    { iNameEdit :: WithTextPos (Widget (am GuiState.Update))
    , iDel :: am ()
    , iAddNext :: Maybe (Sugar.AddNextParam (Name am) im am)
    , iMOrderBefore :: Maybe (am ())
    , iMOrderAfter :: Maybe (am ())
    , iId :: Widget.Id
    }

mkParamPickResult :: Sugar.EntityId -> a -> Menu.PickResult
mkParamPickResult tagInstance _ =
    Menu.PickResult
    { Menu._pickDest = WidgetIds.fromEntityId tagInstance
    , Menu._pickNextEntryPoint =
        WidgetIds.fromEntityId tagInstance & TagEdit.addParamId
    }

-- exported for use in definition sugaring.
make ::
    (Monad im, Monad am) =>
    Annotation.EvalAnnotationOptions ->
    Widget.Id -> Widget.Id ->
    Sugar.FuncParam (Name am) (Info im am) ->
    ExprGuiM im am [ExpressionGui am]
make annotationOpts prevId nextId param =
    do
        conf <- Lens.view Config.config
        let paramEventMap =
                mconcat
                [ eventParamDelEventMap (iDel info) (conf ^. Config.delForwardKeys) "" nextId
                , eventParamDelEventMap (iDel info) (conf ^. Config.delBackwardKeys) " backwards" prevId
                , foldMap (eventMapAddNextParam conf myId) (iAddNext info)
                , foldMap (eventMapOrderParam (conf ^. Config.paramOrderBeforeKeys) "before") (iMOrderBefore info)
                , foldMap (eventMapOrderParam (conf ^. Config.paramOrderAfterKeys) "after") (iMOrderAfter info)
                ]
        wideAnnotationBehavior <-
            GuiState.isSubCursor ?? myId
            <&> Annotation.wideAnnotationBehaviorFromSelected
        paramEdit <-
            Annotation.maybeAddAnnotationWith annotationOpts
            wideAnnotationBehavior ExprGui.showAnnotationWhenVerbose
            (param ^. Sugar.fpAnnotation)
            ?? Responsive.fromWithTextPos (iNameEdit info)
            <&> Widget.widget . Widget.eventMapMaker . Lens.mapped %~ (<> paramEventMap)
            & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
        mAddParam <-
            GuiState.isSubCursor ?? addId
            <&> guard
            <&> (>> (iAddNext info ^? Lens._Just . Sugar._AddNext))
        addParamEdits <-
            case mAddParam of
            Nothing -> pure []
            Just addParam ->
                TagEdit.makeTagHoleEdit addParam mkParamPickResult addId
                & Styled.withColor TextColors.parameterColor
                <&> Responsive.fromWithTextPos
                <&> (:[])
        paramEdit : addParamEdits & pure
    where
        myId = iId info
        addId = TagEdit.addParamId myId
        info = param ^. Sugar.fpInfo
