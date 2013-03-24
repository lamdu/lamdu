{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleContexts, TemplateHaskell #-}
module Lamdu.CodeEdit.ExpressionEdit.FieldEdit
  ( make
  ) where

--import Lamdu.Data.Expression (Field(..))
import Control.Applicative ((<$>))
import Control.MonadA (MonadA)
import Data.Store.Guid (Guid)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM

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

make :: MonadA m => Guid -> Widget.Id -> ExprGuiM m (ExpressionGui m)
make fieldGuid myId = do
  name@(nameSrc, _) <- ExprGuiM.getGuidName fieldGuid
  ExpressionGui.fromValueWidget
    . ExpressionGui.nameSrcTint nameSrc
    <$>
    ExprGuiM.wrapDelegated fieldFDConfig FocusDelegator.NotDelegating id
    (ExpressionGui.makeNameEdit name fieldGuid) myId
