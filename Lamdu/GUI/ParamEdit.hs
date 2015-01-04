{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ParamEdit
  ( make
  ) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

-- exported for use in definition sugaring.
make ::
  MonadA m => ExprGuiM.ShowType -> Widget.Id -> Widget.Id ->
  Sugar.FuncParam (Name m) m ->
  ExprGuiM m (ExpressionGui m)
make showType prevId nextId param =
  assignCursor $ do
    config <- ExprGuiM.widgetEnv WE.readConfig
    let
      paramAddNextEventMap =
        maybe mempty
        (Widget.keysEventMapMovesCursor (Config.addNextParamKeys config)
         (E.Doc ["Edit", "Add next parameter"]) .
         fmap (FocusDelegator.delegatingId . WidgetIds.fromEntityId) .
         (^. Sugar.fpListItemActions . Sugar.itemAddNext))
        mActions
      paramEventMap = mconcat
        [ paramDeleteEventMap (Config.delForwardKeys config) "" nextId
        , paramDeleteEventMap (Config.delBackwardKeys config) " backwards" prevId
        , paramAddNextEventMap
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
       (^. Sugar.fpListItemActions . Sugar.itemDelete))
      mActions
