{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}
module Lamdu.GUI.ParamEdit
    ( Info(..), make
    , eventMapAddFirstParam
    , diveToNameEdit
    ) where

import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import qualified Data.Map as Map
import           Data.Store.Transaction (Transaction)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

type T = Transaction

singletonIdMap ::
    Sugar.EntityId -> Sugar.EntityId ->
    Map.Map Widget.Id Widget.Id
singletonIdMap key val =
    Map.singleton (WidgetIds.fromEntityId key) (WidgetIds.fromEntityId val)

chooseAddResultEntityId :: Sugar.ParamAddResult -> Widget.EventResult
chooseAddResultEntityId (Sugar.ParamAddResultVarToTags Sugar.VarToTags {..}) =
    eventResultFromEntityId (vttNewTag ^. Sugar.tagInstance)
    & Widget.applyIdMapping widgetIdMap
    where
        widgetIdMap =
            singletonIdMap vttReplacedVarEntityId
            (vttReplacedByTag ^. Sugar.tagInstance)
chooseAddResultEntityId (Sugar.ParamAddResultNewVar entityId _) =
    eventResultFromEntityId entityId
chooseAddResultEntityId (Sugar.ParamAddResultNewTag newParamTag) =
    eventResultFromEntityId $ newParamTag ^. Sugar.tagInstance

eventResultFromEntityId :: Sugar.EntityId -> Widget.EventResult
eventResultFromEntityId = Widget.eventResultFromCursor . cursorFromEntityId

cursorFromEntityId :: Sugar.EntityId -> Widget.Id
cursorFromEntityId = diveToNameEdit . WidgetIds.fromEntityId

diveToNameEdit :: Widget.Id -> Widget.Id
diveToNameEdit = ExpressionGui.diveToNameEdit

eventMapAddFirstParam ::
    Functor m => Config -> Maybe (T m Sugar.ParamAddResult) ->
    Widget.EventHandlers (T m)
eventMapAddFirstParam _ Nothing = mempty
eventMapAddFirstParam config (Just addFirstParam) =
    addFirstParam
    <&> chooseAddResultEntityId
    & E.keyPresses (Config.addNextParamKeys config)
        (E.Doc ["Edit", "Add parameter"])

eventMapAddNextParam ::
    Functor m =>
    Config ->
    Maybe (T m Sugar.ParamAddResult) ->
    Widget.EventHandlers (T m)
eventMapAddNextParam _ Nothing = mempty
eventMapAddNextParam config (Just fpAdd) =
    fpAdd
    <&> chooseAddResultEntityId
    & E.keyPresses (Config.addNextParamKeys config)
        (E.Doc ["Edit", "Add next parameter"])

eventParamDelEventMap ::
    MonadA m =>
    Maybe (T m Sugar.ParamDelResult) ->
    [ModKey] -> String -> Widget.Id ->
    Widget.EventHandlers (T m)
eventParamDelEventMap Nothing _ _ _ = mempty
eventParamDelEventMap (Just fpDel) keys docSuffix dstPosId =
    do
        res <- fpDel
        let widgetIdMap =
                case res of
                Sugar.ParamDelResultTagsToVar Sugar.TagsToVar {..} ->
                    singletonIdMap (ttvReplacedTag ^. Sugar.tagInstance)
                    ttvReplacedByVarEntityId
                _ -> Map.empty
        Widget.eventResultFromCursor dstPosId
            & Widget.applyIdMapping widgetIdMap
            & return
    & E.keyPresses keys
        (E.Doc ["Edit", "Delete parameter" ++ docSuffix])

data Info m = Info
    { iMakeNameEdit :: Widget.Id -> ExprGuiM m (ExpressionGui m)
    , iMDel :: Maybe (T m Sugar.ParamDelResult)
    , iMAddNext :: Maybe (T m Sugar.ParamAddResult)
    }

-- exported for use in definition sugaring.
make ::
    MonadA m =>
    ExpressionGui.AnnotationOptions ->
    ExprGuiT.ShowAnnotation -> Widget.Id -> Widget.Id ->
    Sugar.FuncParam (Info m) -> ExprGuiM m (ExpressionGui m)
make annotationOpts showType prevId nextId param =
    assignCursor $
    do
        config <- ExprGuiM.readConfig
        let paramEventMap = mconcat
                [ eventParamDelEventMap mFpDel (Config.delForwardKeys config) "" nextId
                , eventParamDelEventMap mFpDel (Config.delBackwardKeys config) " backwards" prevId
                , eventMapAddNextParam config mFpAdd
                ]
        makeNameEdit myId
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents paramEventMap
            <&> ExpressionGui.egAlignment . _1 .~ 0.5
            >>= ExpressionGui.maybeAddAnnotationWith annotationOpts showType
                (param ^. Sugar.fpAnnotation)
                (param ^. Sugar.fpId)
    where
        entityId = param ^. Sugar.fpId
        myId = WidgetIds.fromEntityId entityId
        Info makeNameEdit mFpDel mFpAdd = param ^. Sugar.fpInfo
        hiddenIds = map WidgetIds.fromEntityId $ param ^. Sugar.fpHiddenIds
        assignCursor x =
            foldr (`ExprGuiM.assignCursorPrefix` const myId) x hiddenIds
