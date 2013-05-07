{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.TagEdit(make) where

import Control.Applicative ((<$>))
import Control.MonadA (MonadA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config

fdConfig :: FocusDelegator.Config
fdConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey = E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename tag"]
  , FocusDelegator.stopDelegatingKey = E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Stop renaming tag"]
  }

make
  :: MonadA m
  => Sugar.TagG Sugar.Name
  -> Widget.Id
  -> ExprGuiM m (ExpressionGui m)
make (Sugar.TagG tag _) =
  ExpressionGui.wrapDelegated fdConfig FocusDelegator.NotDelegating $ \myId -> do
    name <- ExprGuiM.transaction $ ExprGuiM.getGuidName tag
    ExpressionGui.fromValueWidget . Widget.scale Config.fieldScale . Widget.tint Config.fieldTint <$>
      ExpressionGui.makeNameEdit name tag myId
