{-# LANGUAGE TypeFamilies #-}
module Lamdu.CodeEdit.ExpressionEdit.InferredEdit(make) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
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
  { FocusDelegator.startDelegatingKeys = Config.replaceInferredValueKeys
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Replace inferred value"]
  , FocusDelegator.stopDelegatingKeys = Config.keepInferredValueKeys
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Keep inferred value"]
  }

make
  :: MonadA m => Sugar.Inferred Sugar.Name m (Sugar.ExpressionN m) -> Guid -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make inferred guid =
  ExpressionGui.wrapDelegated fdConfig FocusDelegator.NotDelegating $
  makeUnwrapped inferred guid

makeUnwrapped ::
  MonadA m =>
  Sugar.Inferred Sugar.Name m (Sugar.ExpressionN m) -> Guid -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeUnwrapped inferred guid myId = do
  mInnerCursor <- ExprGuiM.widgetEnv $ WE.subCursor myId
  case mInnerCursor of
    Nothing ->
      Lens.traverseOf ExpressionGui.egWidget
      ( ExprGuiM.widgetEnv
      . BWidgets.makeFocusableView myId
      . Widget.tint Config.inferredValueTint
      . Widget.scale Config.inferredValueScaleFactor
      ) =<< ExprGuiM.makeSubexpresion (inferred ^. Sugar.iValue)
    Just _ ->
      HoleEdit.makeUnwrapped (inferred ^. Sugar.iHole) Nothing guid myId
