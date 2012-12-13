{-# LANGUAGE TypeFamilies #-}
module Lamdu.CodeEdit.ExpressionEdit.InferredEdit(make) where

import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Lamdu.Anchors (ViewM)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetEnvT as WE

fdConfig :: FocusDelegator.Config
fdConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = Config.replaceInferredValueKey
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Replace inferred value"]
  , FocusDelegator.stopDelegatingKey = Config.keepInferredValueKey
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Keep inferred value"]
  }

make
  :: (MonadA m, m ~ ViewM) => Sugar.Inferred m (Sugar.Expression m) -> Guid -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make inferred guid =
  ExpressionGui.wrapDelegated fdConfig FocusDelegator.NotDelegating $
  makeUnwrapped inferred guid

makeUnwrapped
  :: Sugar.Inferred ViewM (Sugar.Expression ViewM) -> Guid
  -> Widget.Id
  -> ExprGuiM ViewM (ExpressionGui ViewM)
makeUnwrapped inferred guid myId = do
  mInnerCursor <- ExprGuiM.widgetEnv $ WE.subCursor myId
  case mInnerCursor of
    Nothing ->
      Lens.traverseOf ExpressionGui.egWidget
      ( ExprGuiM.widgetEnv
      . BWidgets.makeFocusableView myId
      . Widget.tint Config.inferredValueTint
      . Widget.scale Config.inferredValueScaleFactor
      ) =<< ExprGuiM.makeSubexpresion (Sugar.iValue inferred)
    Just _ ->
      HoleEdit.makeUnwrapped (Sugar.iHole inferred) Nothing guid myId
