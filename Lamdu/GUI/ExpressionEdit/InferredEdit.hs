{-# LANGUAGE TypeFamilies #-}
module Lamdu.GUI.ExpressionEdit.InferredEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (mempty)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionGui (ExpressionGui, ParentPrecedence(..))
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.Sugar.Types as Sugar

fdConfig :: Config -> FocusDelegator.Config
fdConfig config = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys =
    Config.replaceInferredValueKeys config ++
    Config.delKeys config
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Inferred value", "Replace"]
  , FocusDelegator.stopDelegatingKeys = Config.keepInferredValueKeys config
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Inferred value", "Back"]
  }

make ::
  MonadA m => ParentPrecedence ->
  Sugar.Inferred Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make parentPrecedence inferred pl myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    eventMap =
      maybe mempty
      (E.keyPresses
       (Config.acceptInferredValueKeys config)
       (E.Doc ["Edit", "Inferred value", "Accept"]) .
       fmap HoleEdit.eventResultOfPickedResult) $
      inferred ^. Sugar.iMAccept
  ExprGuiM.wrapDelegated (fdConfig config)
    FocusDelegator.NotDelegating (ExpressionGui.egWidget %~)
    (makeUnwrapped parentPrecedence pl inferred) myId
    <&> ExpressionGui.egWidget %~ Widget.weakerEvents eventMap

makeUnwrapped ::
  MonadA m => ParentPrecedence ->
  Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  Sugar.Inferred Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeUnwrapped (ParentPrecedence parentPrecedence) pl inferred myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  mInnerCursor <- ExprGuiM.widgetEnv $ WE.subCursor myId
  inactive <-
    ExpressionGui.addInferredTypes pl =<<
    ExpressionGui.egWidget
    ( ExprGuiM.widgetEnv
    . BWidgets.makeFocusableView myId
    . Widget.tint (Config.inferredValueTint config)
    . Widget.scale (realToFrac <$> Config.inferredValueScaleFactor config)
    ) =<< ExprGuiM.makeSubexpression parentPrecedence (inferred ^. Sugar.iValue)
  case (mStoredGuid, mInnerCursor, inferred ^. Sugar.iHole . Sugar.holeMActions) of
    (Just storedGuid, Just _, Just actions) ->
      HoleEdit.makeUnwrappedActive pl storedGuid actions
      (inactive ^. ExpressionGui.egWidget . Widget.wSize) myId
    _ -> return inactive
  where
    mStoredGuid = pl ^? Sugar.plActions . Lens._Just . Sugar.storedGuid
