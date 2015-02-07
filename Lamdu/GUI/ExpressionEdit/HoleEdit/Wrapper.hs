{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- | Wrapper hole

module Lamdu.GUI.ExpressionEdit.HoleEdit.Wrapper
    ( make
    ) where

import           Control.Applicative (Applicative(..), (<$>))
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Monoid ((<>))
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.EventMap as HoleEventMap
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (ExpressionN)
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

modifyWrappedEventMap ::
  (MonadA m, Applicative f) =>
  Config -> Bool -> Sugar.HoleArg m (ExpressionN m a) -> WidgetIds ->
  Widget.EventHandlers f ->
  Widget.EventHandlers f
modifyWrappedEventMap config argIsFocused arg WidgetIds{..} eventMap
  | argIsFocused =
    eventMap <>
    Widget.keysEventMapMovesCursor (Config.leaveSubexpressionKeys config)
    (E.Doc ["Navigation", "Go to parent wrapper"]) (pure hidClosed)
  | otherwise =
    Widget.keysEventMapMovesCursor (Config.enterSubexpressionKeys config)
    (E.Doc ["Navigation", "Go to wrapped expr"]) .
    -- TODO: This is ugly: Who says it's in a FocusDelegator?
    pure . WidgetIds.notDelegatingId . WidgetIds.fromExprPayload $
    arg ^. Sugar.haExpr . Sugar.rPayload

makeUnwrapEventMap ::
  (MonadA m, MonadA f) =>
  Sugar.HoleArg f (ExpressionN f a) -> WidgetIds ->
  ExprGuiM m (Widget.EventHandlers (T f))
makeUnwrapEventMap arg WidgetIds{..} = do
  config <- ExprGuiM.readConfig
  let Config.Hole{..} = Config.hole config
  pure $
    case arg ^? Sugar.haUnwrap . Sugar._UnwrapMAction . Lens._Just of
    Just unwrap ->
      Widget.keysEventMapMovesCursor
      (holeUnwrapKeys ++ Config.delKeys config)
      (E.Doc ["Edit", "Unwrap"]) $ WidgetIds.fromEntityId <$> unwrap
    Nothing -> HoleEventMap.open holeUnwrapKeys WidgetIds{..}

make ::
  MonadA m => WidgetIds ->
  Sugar.HoleArg m (ExpressionN m ExprGuiM.Payload) ->
  ExprGuiM m (ExpressionGui m)
make WidgetIds{..} arg = do
  config <- ExprGuiM.readConfig
  let
    Config.Hole{..} = Config.hole config
    bgColor =
      config &
      case arg ^. Sugar.haUnwrap of
      Sugar.UnwrapMAction {} -> Config.typeIndicatorMatchColor
      Sugar.UnwrapTypeMismatch {} -> Config.typeIndicatorErrorColor
    frameWidth = realToFrac <$> Config.typeIndicatorFrameWidth config
    padding = realToFrac <$> Config.valFramePadding config
  argGui <-
    arg ^. Sugar.haExpr
    & ExprGuiM.makeSubexpression 0
  let argIsFocused = argGui ^. ExpressionGui.egWidget . Widget.isFocused
  unwrapEventMap <- makeUnwrapEventMap arg WidgetIds{..}
  argGui
    & ExpressionGui.egWidget . Widget.eventMap %~
      modifyWrappedEventMap config argIsFocused arg WidgetIds{..}
    & ExpressionGui.pad (padding + frameWidth)
    & ExpressionGui.egWidget %~
      Widget.addInnerFrame
      (Config.layerHoleBG (Config.layers config))
      frameId bgColor frameWidth
    & ExpressionGui.egWidget %~ Widget.weakerEvents unwrapEventMap
    & return
  where
    frameId = Widget.toAnimId hidClosed <> ["hole frame"]
