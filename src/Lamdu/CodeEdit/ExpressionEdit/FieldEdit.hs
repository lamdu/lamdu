{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts #-}
module Lamdu.CodeEdit.ExpressionEdit.FieldEdit
  ( make
  ) where

import Control.Applicative ((<$>))
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.Config as Config

type T = Transaction

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

makeTagNameWidget ::
  MonadA m =>
  ((ExprGuiM.NameSource, String) -> ExprGuiM m (Widget f)) ->
  Guid -> ExprGuiM m (Widget f)
makeTagNameWidget f fieldGuid = do
  name@(nameSrc, _) <- ExprGuiM.transaction $ ExprGuiM.getGuidName fieldGuid
  ExpressionGui.nameSrcTint nameSrc .
    Widget.tint Config.fieldTint .
    Widget.scale Config.fieldScale <$> f name

makeTagNameEdit :: MonadA m => Guid -> Widget.Id -> ExprGuiM m (Widget (T m))
makeTagNameEdit fieldGuid myId =
  makeTagNameWidget makeEdit fieldGuid
  where
    makeEdit name = ExpressionGui.makeNameEdit name fieldGuid myId

make :: MonadA m => Guid -> Widget.Id -> ExprGuiM m (ExpressionGui m)
make fieldGuid myId =
  ExpressionGui.fromValueWidget <$>
  ExprGuiM.wrapDelegated fieldFDConfig FocusDelegator.NotDelegating id
  (makeTagNameEdit fieldGuid) myId
