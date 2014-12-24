{-# LANGUAGE OverloadedStrings #-}

module Lamdu.GUI.ExpressionEdit.HoleEdit.Closed
  ( make
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.Lens.Tuple
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import Control.MonadA (MonadA)
import Data.Maybe.Utils (maybeToMPlus)
import Data.Monoid (Monoid(..), (<>))
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Common (makeBackground, diveIntoHole)
import Lamdu.GUI.ExpressionGui (ExpressionGui(..))
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Sugar.AddNames.Types (Name(..), ExpressionN)
import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

make ::
  MonadA m =>
  Sugar.Hole (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  Widget.Id ->
  ExprGuiM m (Widget.Id, ExpressionGui m)
make hole pl myId =
  (Lens.mapped . _2 . ExpressionGui.egWidget %~ Widget.weakerEvents openEventMap) $
  runMatcherT $ do
    justToLeft $ do
      arg <- maybeToMPlus $ hole ^. Sugar.holeMArg
      gui <- lift $ makeWrapper arg myId
      return (myId, gui)
    justToLeft $ do
      guard $ not isHoleResult -- Avoid suggesting inside hole results
      guard . Lens.nullOf ExprLens.valHole $ suggested ^. Sugar.hsValue
      (destId, gui) <- lift $ makeSuggested suggested myId
      return (destId, gui)
    gui <- makeSimple myId & lift
    return (diveIntoHole myId, gui)
  where
    isHoleResult =
      Lens.nullOf (Sugar.plData . ExprGuiM.plStoredEntityIds . Lens.traversed) pl
    suggested = hole ^. Sugar.holeSuggested
    openEventMap =
      Widget.keysEventMapMovesCursor [E.ModKey E.noMods E.Key'Enter]
      (E.Doc ["Navigation", "Hole", "Open"]) . pure $
      diveIntoHole myId

makeUnwrapEventMap ::
  (MonadA m, MonadA f) =>
  Sugar.HoleArg f (ExpressionN f a) -> Widget.Id ->
  ExprGuiM m (Widget.EventHandlers (T f))
makeUnwrapEventMap arg myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  pure $
    case arg ^? Sugar.haUnwrap . Sugar._UnwrapMAction . Lens._Just of
    Just unwrap ->
      Widget.keysEventMapMovesCursor (Config.acceptKeys config ++ Config.delKeys config)
      (E.Doc ["Edit", "Unwrap"]) $ WidgetIds.fromEntityId <$> unwrap
    Nothing ->
      Widget.keysEventMapMovesCursor (Config.wrapKeys config)
      (E.Doc ["Navigation", "Hole", "Open"]) .
      pure $ diveIntoHole myId

modifyWrappedEventMap ::
  (MonadA m, Applicative f) =>
  Config -> Bool -> Sugar.HoleArg m (ExpressionN m a) -> Widget.Id ->
  Widget.EventHandlers f ->
  Widget.EventHandlers f
modifyWrappedEventMap config argIsFocused arg myId eventMap
  | argIsFocused =
    eventMap <>
    Widget.keysEventMapMovesCursor (Config.leaveSubexpressionKeys config)
    (E.Doc ["Navigation", "Go to parent wrapper"]) (pure myId)
  | otherwise =
    Widget.keysEventMapMovesCursor (Config.enterSubexpressionKeys config)
    (E.Doc ["Navigation", "Go to wrapped expr"]) .
    pure . FocusDelegator.notDelegatingId . WidgetIds.fromEntityId $
    arg ^. Sugar.haExpr . Sugar.rPayload . Sugar.plEntityId

makeWrapper ::
  MonadA m =>
  Sugar.HoleArg m (ExpressionN m ExprGuiM.Payload) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
makeWrapper arg myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    bgColor =
      config &
      case arg ^. Sugar.haUnwrap of
      Sugar.UnwrapMAction {} -> Config.typeMatchColor
      Sugar.UnwrapTypeMismatch {} -> Config.typeErrorColor
    frameWidth = realToFrac <$> Config.wrapperHoleFrameWidth config
    padding = realToFrac <$> Config.valFramePadding config
  argGui <-
    arg ^. Sugar.haExpr
    & ExprGuiM.makeSubexpression 0
  let argIsFocused = argGui ^. ExpressionGui.egWidget . Widget.wIsFocused
  gui <-
    argGui
    & ExpressionGui.egWidget . Widget.wEventMap %~
      modifyWrappedEventMap config argIsFocused arg myId
    & ExpressionGui.pad (padding + frameWidth)
    & ExpressionGui.egWidget %~
      Widget.addInnerFrame
      (Config.layerHoleBG (Config.layers config))
      frameId bgColor frameWidth
    & ExpressionGui.egWidget %%~ makeFocusable myId
  unwrapEventMap <- makeUnwrapEventMap arg myId
  return (gui & ExpressionGui.egWidget %~ Widget.weakerEvents unwrapEventMap)
  where
    frameId = Widget.toAnimId myId <> ["hole frame"]

makeSuggested ::
  MonadA m =>
  Sugar.HoleSuggested (Name m) m ->
  Widget.Id -> ExprGuiM m (Widget.Id, ExpressionGui m)
makeSuggested suggested myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  gui <-
    (suggested ^. Sugar.hsMakeConverted)
    & ExprGuiM.transaction
    <&> Lens.mapped . Lens.mapped .~ emptyPl
    >>= ExprGuiM.makeSubexpression 0
    >>= ExpressionGui.egWidget %%~
        makeFocusable myId .
        Widget.tint (Config.suggestedValueTint config) .
        Widget.scale (realToFrac <$> Config.suggestedValueScaleFactor config) .
        (Widget.wEventMap .~ mempty)
  return $
    if fullySuggested
    then (myId, gui)
    else
      ( diveIntoHole myId
      , gui
        & ExpressionGui.egWidget %~
          makeBackground myId (Config.layerHoleBG (Config.layers config))
          (Config.inactiveHoleBGColor config)
      )
  where
    fullySuggested =
      Lens.nullOf (ExprLens.subExprs . ExprLens.valHole) $
      suggested ^. Sugar.hsValue
    emptyPl =
      ExprGuiM.Payload
      { ExprGuiM._plStoredEntityIds = []
      , ExprGuiM._plInjected = []
      , ExprGuiM._plShowType = ExprGuiM.DoNotShowType
      -- filled by AddNextHoles above
      , ExprGuiM._plHoleEntityIds = ExprGuiM.emptyHoleEntityIds
      }

makeSimple :: MonadA m => Widget.Id -> ExprGuiM m (ExpressionGui m)
makeSimple myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  ExprGuiM.widgetEnv
    (BWidgets.makeTextViewWidget "  " (Widget.toAnimId myId))
    <&>
      makeBackground myId
      (Config.layerHoleBG (Config.layers config))
      (Config.inactiveHoleBGColor config)
    <&> ExpressionGui.fromValueWidget
    >>= ExpressionGui.egWidget %%~ makeFocusable myId

makeFocusable :: (MonadA m, Applicative f) => Widget.Id -> Widget f -> ExprGuiM m (Widget f)
makeFocusable wId = ExprGuiM.widgetEnv . BWidgets.makeFocusableView wId
