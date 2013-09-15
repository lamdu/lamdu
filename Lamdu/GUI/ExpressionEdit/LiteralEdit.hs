{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LiteralEdit
  ( makeInt
  ) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Lamdu.GUI.ExpressionEdit.HoleEdit.State (HoleState(..), setHoleStateAndJump)
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

setColor :: MonadA m => ExprGuiM m a -> ExprGuiM m a
setColor action = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.withFgColor (Config.literalIntColor config) action

mkEditEventMap ::
  MonadA m => Integer -> T m Guid -> Widget.EventHandlers (T m)
mkEditEventMap integer setToHole =
  Widget.keysEventMapMovesCursor [E.ModKey E.noMods E.Key'Enter]
  (E.Doc ["Edit", "Integer"]) $
  setHoleStateAndJump (HoleState (show integer)) =<< setToHole

makeInt ::
  MonadA m =>
  Integer -> Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeInt integer pl myId =
  BWidgets.makeFocusableTextView (show integer) myId
  & setColor . ExprGuiM.widgetEnv
  <&> Widget.weakerEvents editEventMap
  <&> ExpressionGui.fromValueWidget
  & ExpressionGui.stdWrap pl
  where
    editEventMap =
      maybe mempty (mkEditEventMap integer) $
      pl ^? Sugar.plActions . Lens._Just . Sugar.mSetToHole . Lens._Just
