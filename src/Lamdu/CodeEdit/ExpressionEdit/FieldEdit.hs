{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, TemplateHaskell #-}
module Lamdu.CodeEdit.ExpressionEdit.FieldEdit
  ( make
  ) where

import Control.Applicative ((<$>))
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

make :: MonadA m => Sugar.FieldTag -> Widget.Id -> ExprGuiM m (ExpressionGui m)
make (Sugar.FieldTag fieldGuid) myId = do
  name@(nameSrc, _) <- ExprGuiM.getGuidName fieldGuid
  ExpressionGui.fromValueWidget
    . ExpressionGui.nameSrcTint nameSrc
    <$>
    ExprGuiM.wrapDelegated fieldFDConfig FocusDelegator.NotDelegating id
    (ExpressionGui.makeNameEdit name fieldGuid) myId
make Sugar.FieldTagHole myId =
  fmap ExpressionGui.fromValueWidget . ExprGuiM.widgetEnv $ BWidgets.makeFocusableTextView "HOLE" myId
