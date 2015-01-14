{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ParamEdit
  ( make
  , eventMapAddFirstParam
  ) where

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Monoid (Monoid(..))
import           Data.Store.Transaction (Transaction)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

chooseAddResultEntityId :: Sugar.ParamAddResult -> Sugar.EntityId
chooseAddResultEntityId (Sugar.ParamAddResultNewVar entityId _) = entityId
chooseAddResultEntityId (Sugar.ParamAddResultVarToTags (Sugar.VarToTags _var _oldParamAsTag newParamTag)) =
  newParamTag ^. Sugar.tagInstance
chooseAddResultEntityId (Sugar.ParamAddResultNewTag newParamTag) =
  newParamTag ^. Sugar.tagInstance

toEventMapAction :: Sugar.EntityId -> Widget.Id
toEventMapAction = FocusDelegator.delegatingId . WidgetIds.fromEntityId

eventMapAddFirstParam ::
  Functor m => Config -> Maybe (T m Sugar.ParamAddResult) ->
  Widget.EventHandlers (T m)
eventMapAddFirstParam _ Nothing = mempty
eventMapAddFirstParam config (Just addFirstParam) =
  addFirstParam
  <&> toEventMapAction . chooseAddResultEntityId
  & Widget.keysEventMapMovesCursor (Config.addNextParamKeys config)
    (E.Doc ["Edit", "Add parameter"])

eventMapAddNextParam ::
  Functor m =>
  Config ->
  Maybe (Sugar.FuncParamActions m) ->
  Widget.EventHandlers (T m)
eventMapAddNextParam _ Nothing = mempty
eventMapAddNextParam config (Just actions) =
  actions ^. Sugar.fpAddNext
  <&> toEventMapAction . chooseAddResultEntityId
  & Widget.keysEventMapMovesCursor (Config.addNextParamKeys config)
    (E.Doc ["Edit", "Add next parameter"])

-- exported for use in definition sugaring.
make ::
  MonadA m => ExprGuiM.ShowType -> Widget.Id -> Widget.Id ->
  Sugar.FuncParam v (Name m) m ->
  ExprGuiM m (ExpressionGui m)
make showType prevId nextId param =
  assignCursor $ do
    config <- ExprGuiM.widgetEnv WE.readConfig
    let
      paramEventMap = mconcat
        [ paramDeleteEventMap (Config.delForwardKeys config) "" nextId
        , paramDeleteEventMap (Config.delBackwardKeys config) " backwards" prevId
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
      foldr (`ExprGuiM.assignCursorPrefix` myId) x hiddenIds
    paramDeleteEventMap keys docSuffix dstPos =
      maybe mempty
      (Widget.keysEventMapMovesCursor keys (E.Doc ["Edit", "Delete parameter" ++ docSuffix]) .
       (>> return dstPos) .
       (^. Sugar.fpDelete))
      mActions
