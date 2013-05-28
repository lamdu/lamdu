{-# LANGUAGE TypeFamilies #-}
module Lamdu.CodeEdit.ExpressionEdit.InferredEdit(make) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (mempty)
import Data.Store.Guid (Guid)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui, ParentPrecedence(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
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
import qualified Lamdu.WidgetIds as WidgetIds

fdConfig :: FocusDelegator.Config
fdConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = Config.replaceInferredValueKeys
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Inferred value", "Replace"]
  , FocusDelegator.stopDelegatingKeys = Config.keepInferredValueKeys
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Inferred value", "Back"]
  }

make ::
  MonadA m => ParentPrecedence ->
  Sugar.Inferred Sugar.Name m (Sugar.ExpressionN m) -> Guid -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
make parentPrecedence inferred guid =
  (fmap . fmap)
    (ExpressionGui.egWidget %~ Widget.weakerEvents eventMap) .
  ExpressionGui.wrapDelegated fdConfig FocusDelegator.NotDelegating $
  makeUnwrapped parentPrecedence inferred guid
  where
    eventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor
       Config.acceptInferredValueKeys
       (E.Doc ["Edit", "Inferred value", "Accept"]) .
       fmap WidgetIds.fromGuid) $
      inferred ^. Sugar.iMAccept

makeUnwrapped ::
  MonadA m => ParentPrecedence ->
  Sugar.Inferred Sugar.Name m (Sugar.ExpressionN m) -> Guid -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeUnwrapped (ParentPrecedence parentPrecedence) inferred guid myId = do
  mInnerCursor <- ExprGuiM.widgetEnv $ WE.subCursor myId
  case mInnerCursor of
    Nothing ->
      ExpressionGui.egWidget
      ( ExprGuiM.widgetEnv
      . BWidgets.makeFocusableView myId
      . Widget.tint Config.inferredValueTint
      . Widget.scale Config.inferredValueScaleFactor
      ) =<< ExprGuiM.makeSubexpresion parentPrecedence (inferred ^. Sugar.iValue)
    Just _ ->
      HoleEdit.makeUnwrapped (inferred ^. Sugar.iHole) Nothing guid myId
