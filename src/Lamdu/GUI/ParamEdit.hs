{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}
module Lamdu.GUI.ParamEdit
    ( Info(..), make
    , eventMapAddFirstParam
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Data.Store.Transaction (Transaction)
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Element as Element
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.MetaKey (MetaKey, toModKey)
import qualified GUI.Momentu.Responsive as Responsive
import qualified GUI.Momentu.State as GuiState
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionGui.Annotation as Annotation
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

chooseAddResultEntityId :: Sugar.ParamAddResult -> GuiState.Update
chooseAddResultEntityId (Sugar.ParamAddResultVarToTags Sugar.VarToTags {..}) =
    vttNewTag ^. Sugar.tagInstance
    & WidgetIds.fromEntityId & WidgetIds.tagHoleId & GuiState.updateCursor
chooseAddResultEntityId (Sugar.ParamAddResultNewVar entityId _) =
    WidgetIds.fromEntityId entityId & WidgetIds.nameEditOf & GuiState.updateCursor
chooseAddResultEntityId (Sugar.ParamAddResultNewTag entityId) =
    WidgetIds.fromEntityId entityId & WidgetIds.tagHoleId & GuiState.updateCursor

eventMapAddFirstParam ::
    Functor m => Config -> T m Sugar.ParamAddResult ->
    EventMap (T m GuiState.Update)
eventMapAddFirstParam config addFirstParam =
    addFirstParam
    <&> chooseAddResultEntityId
    & E.keyPresses (Config.addNextParamKeys config <&> toModKey)
        (E.Doc ["Edit", "Add parameter"])

eventMapAddNextParam ::
    Functor m =>
    Config -> T m Sugar.ParamAddResult ->
    EventMap (T m GuiState.Update)
eventMapAddNextParam config fpAdd =
    fpAdd
    <&> chooseAddResultEntityId
    & E.keyPresses (Config.addNextParamKeys config <&> toModKey)
        (E.Doc ["Edit", "Add next parameter"])

eventMapOrderParam ::
    Monad m =>
    [MetaKey] -> Text -> m () -> EventMap (m GuiState.Update)
eventMapOrderParam keys docSuffix =
    E.keysEventMap keys (E.Doc ["Edit", "Parameter", "Move " <> docSuffix])

eventParamDelEventMap ::
    Monad m =>
    m Sugar.ParamDelResult -> [MetaKey] -> Text -> Widget.Id ->
    EventMap (m GuiState.Update)
eventParamDelEventMap fpDel keys docSuffix dstPosId =
    GuiState.updateCursor dstPosId <$ fpDel
    & E.keyPresses (keys <&> toModKey)
        (E.Doc ["Edit", "Delete parameter" <> docSuffix])

data Info m = Info
    { iNameEdit :: WithTextPos (Widget (T m GuiState.Update))
    , iDel :: T m Sugar.ParamDelResult
    , iMAddNext :: Maybe (T m Sugar.ParamAddResult)
    , iMOrderBefore :: Maybe (T m ())
    , iMOrderAfter :: Maybe (T m ())
    , iId :: Widget.Id
    }

-- exported for use in definition sugaring.
make ::
    Monad m =>
    Annotation.EvalAnnotationOptions ->
    Widget.Id -> Widget.Id ->
    Sugar.FuncParam (Info m) -> ExprGuiM m (ExpressionGui m)
make annotationOpts prevId nextId param =
    do
        config <- Lens.view Config.config
        let paramEventMap = mconcat
                [ eventParamDelEventMap (iDel info) (Config.delForwardKeys config) "" nextId
                , eventParamDelEventMap (iDel info) (Config.delBackwardKeys config) " backwards" prevId
                , foldMap (eventMapAddNextParam config) (iMAddNext info)
                , foldMap (eventMapOrderParam (Config.paramOrderBeforeKeys config) "before") (iMOrderBefore info)
                , foldMap (eventMapOrderParam (Config.paramOrderAfterKeys config) "after") (iMOrderAfter info)
                ]
        wideAnnotationBehavior <-
            GuiState.isSubCursor ?? myId
            <&> Annotation.wideAnnotationBehaviorFromSelected
        Annotation.maybeAddAnnotationWith annotationOpts
            wideAnnotationBehavior ExprGui.showAnnotationWhenVerbose
            (param ^. Sugar.fpAnnotation)
            ?? Responsive.fromWithTextPos (iNameEdit info)
            <&> Widget.weakerEvents paramEventMap
            & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = iId info
        info = param ^. Sugar.fpInfo
