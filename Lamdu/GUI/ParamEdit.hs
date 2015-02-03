{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Lamdu.GUI.ParamEdit
  ( make
  , eventMapAddFirstParam
  , diveToNameEdit
  ) where

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import qualified Data.Map as Map
import           Data.Monoid (Monoid(..))
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
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

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
  Maybe (Sugar.FuncParamActions m) ->
  Widget.EventHandlers (T m)
eventMapAddNextParam _ Nothing = mempty
eventMapAddNextParam config (Just actions) =
  actions ^. Sugar.fpAddNext
  <&> chooseAddResultEntityId
  & E.keyPresses (Config.addNextParamKeys config)
    (E.Doc ["Edit", "Add next parameter"])

eventParamDelEventMap ::
  MonadA m =>
  Maybe (Sugar.FuncParamActions m) ->
  [ModKey] -> String -> Widget.Id ->
  Widget.EventHandlers (T m)
eventParamDelEventMap Nothing _ _ _ = mempty
eventParamDelEventMap (Just actions) keys docSuffix dstPosId =
  do
    res <- actions ^. Sugar.fpDelete
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

-- exported for use in definition sugaring.
make ::
  MonadA m => ExprGuiM.ShowType -> Widget.Id -> Widget.Id ->
  Sugar.FuncParam v (Name m) m ->
  ExprGuiM m (ExpressionGui m)
make showType prevId nextId param =
  assignCursor $ do
    config <- ExprGuiM.readConfig
    let
      paramEventMap = mconcat
        [ eventParamDelEventMap mActions (Config.delForwardKeys config) "" nextId
        , eventParamDelEventMap mActions (Config.delBackwardKeys config) " backwards" prevId
        , eventMapAddNextParam config mActions
        ]
    paramNameEdit <-
      ExpressionGui.makeNameOriginEdit (param ^. Sugar.fpName) myId
      <&> Widget.weakerEvents paramEventMap
      <&> ExpressionGui.fromValueWidget
    paramNameEdit
      & ExpressionGui.maybeAddInferredType showType
        (param ^. Sugar.fpInferredType)
        (param ^. Sugar.fpId)
  where
    entityId = param ^. Sugar.fpId
    myId = WidgetIds.fromEntityId entityId
    mActions = param ^. Sugar.fpMActions
    hiddenIds = map WidgetIds.fromEntityId $ param ^. Sugar.fpHiddenIds
    assignCursor x =
      foldr (`ExprGuiM.assignCursorPrefix` const myId) x hiddenIds
