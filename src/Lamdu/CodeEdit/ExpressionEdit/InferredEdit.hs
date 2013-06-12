{-# LANGUAGE TypeFamilies #-}
module Lamdu.CodeEdit.ExpressionEdit.InferredEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (mempty)
import Data.Store.Guid (Guid)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui, ParentPrecedence(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Config (Config)
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

fdConfig :: Config -> FocusDelegator.Config
fdConfig config = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = Config.replaceInferredValueKeys config
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Inferred value", "Replace"]
  , FocusDelegator.stopDelegatingKeys = Config.keepInferredValueKeys config
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Inferred value", "Back"]
  }

make ::
  MonadA m => ParentPrecedence ->
  Sugar.Inferred Sugar.Name m (Sugar.ExpressionN m) -> Guid -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
make parentPrecedence inferred guid myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    eventMap =
      maybe mempty
      (Widget.keysEventMapMovesCursor
       (Config.acceptInferredValueKeys config)
       (E.Doc ["Edit", "Inferred value", "Accept"]) .
       fmap WidgetIds.fromGuid) $
      inferred ^. Sugar.iMAccept
  ExpressionGui.wrapDelegated (fdConfig config) FocusDelegator.NotDelegating
    (makeUnwrapped parentPrecedence inferred guid) myId
    <&> ExpressionGui.egWidget %~ Widget.weakerEvents eventMap


makeUnwrapped ::
  MonadA m => ParentPrecedence ->
  Sugar.Inferred Sugar.Name m (Sugar.ExpressionN m) -> Guid -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeUnwrapped (ParentPrecedence parentPrecedence) inferred guid myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  mInnerCursor <- ExprGuiM.widgetEnv $ WE.subCursor myId
  case mInnerCursor of
    Nothing ->
      ExpressionGui.egWidget
      ( ExprGuiM.widgetEnv
      . BWidgets.makeFocusableView myId
      . Widget.tint (Config.inferredValueTint config)
      . Widget.scale (realToFrac <$> Config.inferredValueScaleFactor config)
      ) =<< ExprGuiM.makeSubexpresion parentPrecedence (inferred ^. Sugar.iValue)
    Just _ ->
      HoleEdit.makeUnwrapped Nothing (inferred ^. Sugar.iHole) Nothing guid myId
