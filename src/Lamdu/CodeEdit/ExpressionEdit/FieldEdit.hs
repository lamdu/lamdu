{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, TemplateHaskell #-}
module Lamdu.CodeEdit.ExpressionEdit.FieldEdit
  ( make
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.WidgetIds as WidgetIds

fieldFDConfig :: FocusDelegator.Config
fieldFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKey =
    E.ModKey E.noMods E.KeyEnter
  , FocusDelegator.startDelegatingDoc =
    E.Doc ["Edit", "Record", "Field", "Rename"]
  , FocusDelegator.stopDelegatingKey =
    E.ModKey E.noMods E.KeyEsc
  , FocusDelegator.stopDelegatingDoc =
    E.Doc ["Edit", "Record", "Field", "Done renaming"]
  }

make :: MonadA m => Sugar.FieldTag m -> ExprGuiM m (ExpressionGui m)
make fieldTag =
  case fieldTag ^. Sugar.ftTag of
  Nothing -> makeFieldHole fieldTag myId
  Just fieldGuid -> do
    name@(nameSrc, _) <- ExprGuiM.getGuidName fieldGuid
    ExpressionGui.fromValueWidget
      . ExpressionGui.nameSrcTint nameSrc
      <$>
      ExprGuiM.wrapDelegated fieldFDConfig FocusDelegator.NotDelegating id
      (ExpressionGui.makeNameEdit name fieldGuid) myId
  where
    myId = WidgetIds.fromGuid $ fieldTag ^. Sugar.ftGuid

makeFieldHole :: MonadA m => Sugar.FieldTag m -> Widget.Id -> ExprGuiM m (ExpressionGui m)
makeFieldHole _fieldTag myId =
  fmap ExpressionGui.fromValueWidget . ExprGuiM.widgetEnv $ BWidgets.makeFocusableTextView "HOLE" myId
