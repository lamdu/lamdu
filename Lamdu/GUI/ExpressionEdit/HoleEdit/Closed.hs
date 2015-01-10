{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Lamdu.GUI.ExpressionEdit.HoleEdit.Closed
  ( HoleDest(..)
  , ClosedHole(..), chClosedHole
  , make
  ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Lens (Lens')
import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Monad (guard)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Either.Utils (runMatcherT, justToLeft)
import           Control.MonadA (MonadA)
import           Data.Maybe.Utils (maybeToMPlus)
import           Data.Monoid (Monoid(..), (<>))
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Expr.Lens as ExprLens
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Common (addBackground)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleIds(..))
import           Lamdu.GUI.ExpressionGui (ExpressionGui(..))
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..), ExpressionN)
import qualified Lamdu.Sugar.NearestHoles as NearestHoles
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

data HoleDest = HoleDestClosed | HoleDestOpened

data ClosedHole m = ClosedHole
  { chDest :: HoleDest
  , _chClosedHole :: ExpressionGui m
  }

chClosedHole :: Lens' (ClosedHole m) (ExpressionGui m)
chClosedHole f ClosedHole{..} = f _chClosedHole <&> \_chClosedHole -> ClosedHole{..}

make ::
  MonadA m =>
  Sugar.Hole (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload -> HoleIds ->
  ExprGuiM m (ClosedHole m)
make hole pl hids@HoleIds{..} =
  do
    Config.Hole{..} <- ExprGuiM.widgetEnv WE.readConfig <&> Config.hole
    do
      justToLeft $ do
        arg <- maybeToMPlus $ hole ^. Sugar.holeMArg
        gui <- lift $ makeWrapper arg hids
        return ClosedHole
          { chDest = HoleDestClosed
          , _chClosedHole = gui
          }
      justToLeft $ do
        guard $ not isHoleResult -- Avoid suggesting inside hole results
        guard . Lens.nullOf ExprLens.valHole $ suggested ^. Sugar.hsValue
        (dest, gui) <- lift $ makeSuggested suggested hids
        return ClosedHole
          { chDest = dest
          , _chClosedHole = gui
          }
      gui <- makeSimple hids & lift
      return ClosedHole
        { chDest = HoleDestOpened
        , _chClosedHole = gui
        }
      & runMatcherT
      <&> chClosedHole . ExpressionGui.egWidget %~
          Widget.weakerEvents (openHoleEventMap holeOpenKeys hids)
  where
    isHoleResult =
      Lens.nullOf (Sugar.plData . ExprGuiM.plStoredEntityIds . Lens.traversed) pl
    suggested = hole ^. Sugar.holeSuggested

openHoleEventMap ::
  Applicative f => [ModKey] -> HoleIds -> Widget.EventHandlers f
openHoleEventMap keys HoleIds{..} =
  Widget.keysEventMapMovesCursor keys doc $ pure hidOpen
  where
    doc = E.Doc ["Navigation", "Hole", "Open"]

makeUnwrapEventMap ::
  (MonadA m, MonadA f) =>
  Sugar.HoleArg f (ExpressionN f a) -> HoleIds ->
  ExprGuiM m (Widget.EventHandlers (T f))
makeUnwrapEventMap arg hids = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let Config.Hole{..} = Config.hole config
  pure $
    case arg ^? Sugar.haUnwrap . Sugar._UnwrapMAction . Lens._Just of
    Just unwrap ->
      Widget.keysEventMapMovesCursor
      (holeUnwrapKeys ++ Config.delKeys config)
      (E.Doc ["Edit", "Unwrap"]) $ WidgetIds.fromEntityId <$> unwrap
    Nothing -> openHoleEventMap (Config.wrapKeys config) hids

modifyWrappedEventMap ::
  (MonadA m, Applicative f) =>
  Config -> Bool -> Sugar.HoleArg m (ExpressionN m a) -> HoleIds ->
  Widget.EventHandlers f ->
  Widget.EventHandlers f
modifyWrappedEventMap config argIsFocused arg HoleIds{..} eventMap
  | argIsFocused =
    eventMap <>
    Widget.keysEventMapMovesCursor (Config.leaveSubexpressionKeys config)
    (E.Doc ["Navigation", "Go to parent wrapper"]) (pure hidClosed)
  | otherwise =
    Widget.keysEventMapMovesCursor (Config.enterSubexpressionKeys config)
    (E.Doc ["Navigation", "Go to wrapped expr"]) .
    -- TODO: This is ugly: Who says it's in a FocusDelegator?
    pure . FocusDelegator.notDelegatingId . WidgetIds.fromEntityId $
    arg ^. Sugar.haExpr . Sugar.rPayload . Sugar.plEntityId

makeWrapper ::
  MonadA m =>
  Sugar.HoleArg m (ExpressionN m ExprGuiM.Payload) ->
  HoleIds -> ExprGuiM m (ExpressionGui m)
makeWrapper arg hids@HoleIds{..} = do
  config <- ExprGuiM.widgetEnv WE.readConfig
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
  let argIsFocused = argGui ^. ExpressionGui.egWidget . Widget.wIsFocused
  unwrapEventMap <- makeUnwrapEventMap arg hids
  argGui
    & ExpressionGui.egWidget . Widget.wEventMap %~
      modifyWrappedEventMap config argIsFocused arg hids
    & ExpressionGui.pad (padding + frameWidth)
    & ExpressionGui.egWidget %~
      Widget.addInnerFrame
      (Config.layerHoleBG (Config.layers config))
      frameId bgColor frameWidth
    & ExpressionGui.egWidget %~ Widget.weakerEvents unwrapEventMap
    & return
  where
    frameId = Widget.toAnimId hidClosed <> ["hole frame"]

makeSuggested ::
  MonadA m =>
  Sugar.HoleSuggested (Name m) m ->
  HoleIds -> ExprGuiM m (HoleDest, ExpressionGui m)
makeSuggested suggested HoleIds{..} = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let Config.Hole{..} = Config.hole config
  gui <-
    (suggested ^. Sugar.hsMakeConverted)
    & ExprGuiM.transaction
    <&> Lens.mapped . Lens.mapped .~ ExprGuiM.emptyPayload NearestHoles.none
    <&> Lens.mapped . Lens.mapped . ExprGuiM.plShowType .~ ExprGuiM.DoNotShowType
    >>= ExprGuiM.makeSubexpression 0
    <&> ExpressionGui.egWidget %~
        Widget.tint (Config.suggestedValueTint config) .
        Widget.scale (realToFrac <$> Config.suggestedValueScaleFactor config) .
        (Widget.wEventMap .~ mempty)
  return $
    if fullySuggested
    then (HoleDestClosed, gui)
    else
      ( HoleDestOpened
      , gui
        & ExpressionGui.egWidget %~
          addBackground hidClosed (Config.layers config) holeClosedBGColor
      )
  where
    fullySuggested =
      Lens.nullOf (ExprLens.subExprs . ExprLens.valHole) $
      suggested ^. Sugar.hsValue

makeSimple :: MonadA m => HoleIds -> ExprGuiM m (ExpressionGui m)
makeSimple HoleIds{..} = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let Config.Hole{..} = Config.hole config
  ExprGuiM.widgetEnv
    (BWidgets.makeTextViewWidget "  " (Widget.toAnimId hidClosed))
    <&> addBackground hidClosed (Config.layers config) holeClosedBGColor
    <&> ExpressionGui.fromValueWidget
