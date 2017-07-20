{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}
module Lamdu.GUI.ParamEdit
    ( Info(..), make
    , eventMapAddFirstParam
    ) where

import qualified Control.Lens as Lens
import qualified Data.Map as Map
import           Data.Store.Transaction (Transaction)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.MetaKey (MetaKey, toModKey)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

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
cursorFromEntityId = WidgetIds.nameEditOf . WidgetIds.fromEntityId

eventMapAddFirstParam ::
    Functor m => Config -> T m Sugar.ParamAddResult ->
    Widget.EventMap (T m Widget.EventResult)
eventMapAddFirstParam config addFirstParam =
    addFirstParam
    <&> chooseAddResultEntityId
    & E.keyPresses (Config.addNextParamKeys config <&> toModKey)
        (E.Doc ["Edit", "Add parameter"])

eventMapAddNextParam ::
    Functor m =>
    Config -> T m Sugar.ParamAddResult ->
    Widget.EventMap (T m Widget.EventResult)
eventMapAddNextParam config fpAdd =
    fpAdd
    <&> chooseAddResultEntityId
    & E.keyPresses (Config.addNextParamKeys config <&> toModKey)
        (E.Doc ["Edit", "Add next parameter"])

eventMapOrderParam ::
    Monad m =>
    [MetaKey] -> Text -> m () -> Widget.EventMap (m Widget.EventResult)
eventMapOrderParam keys docSuffix =
    Widget.keysEventMap keys (E.Doc ["Edit", "Parameter", "Move " <> docSuffix])

eventParamDelEventMap ::
    Monad m =>
    m Sugar.ParamDelResult -> [MetaKey] -> Text -> Widget.Id ->
    Widget.EventMap (m Widget.EventResult)
eventParamDelEventMap fpDel keys docSuffix dstPosId =
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
    & E.keyPresses (keys <&> toModKey)
        (E.Doc ["Edit", "Delete parameter" <> docSuffix])

data Info m = Info
    { iMakeNameEdit :: Widget.Id -> ExprGuiM m (ExpressionGui m)
    , iDel :: T m Sugar.ParamDelResult
    , iMAddNext :: Maybe (T m Sugar.ParamAddResult)
    , iMOrderBefore :: Maybe (T m ())
    , iMOrderAfter :: Maybe (T m ())
    }

-- exported for use in definition sugaring.
make ::
    Monad m =>
    ExpressionGui.EvalAnnotationOptions ->
    ExprGuiT.ShowAnnotation -> Widget.Id -> Widget.Id ->
    Sugar.FuncParam (Info m) -> ExprGuiM m (ExpressionGui m)
make annotationOpts showAnnotation prevId nextId param =
    assignCursor $
    do
        config <- Lens.view Config.config
        let paramEventMap = mconcat
                [ eventParamDelEventMap (iDel info) (Config.delForwardKeys config) "" nextId
                , eventParamDelEventMap (iDel info) (Config.delBackwardKeys config) " backwards" prevId
                , maybe mempty (eventMapAddNextParam config) (iMAddNext info)
                , maybe mempty (eventMapOrderParam (Config.paramOrderBeforeKeys config) "before") (iMOrderBefore info)
                , maybe mempty (eventMapOrderParam (Config.paramOrderAfterKeys config) "after") (iMOrderAfter info)
                ]
        fpIsSelected <- Widget.isSubCursor ?? myId
        let wideAnnotationBehavior =
                ExpressionGui.wideAnnotationBehaviorFromSelected fpIsSelected
        ExpressionGui.maybeAddAnnotationWith annotationOpts
            wideAnnotationBehavior showAnnotation
            (param ^. Sugar.fpAnnotation)
            entityId
            <*>
            ( iMakeNameEdit info myId
              <&> E.weakerEvents paramEventMap
              -- TODO (ALIGN):
              -- <&> TreeLayout.alignment . _1 .~ 0.5
            )
    where
        entityId = param ^. Sugar.fpId
        myId = WidgetIds.fromEntityId entityId
        info = param ^. Sugar.fpInfo
        hiddenIds = map WidgetIds.fromEntityId $ param ^. Sugar.fpHiddenIds
        assignCursor x =
            foldr (`Widget.assignCursorPrefix` const myId) x hiddenIds
