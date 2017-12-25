{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
-- | Wrapper hole
module Lamdu.GUI.ExpressionEdit.HoleEdit.Argument
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import qualified GUI.Momentu as Momentu
import           GUI.Momentu.EventMap (EventMap)
import qualified GUI.Momentu.EventMap as E
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widget as Widget
import           GUI.Momentu.Widgets.Menu.Picker (withPicker)
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionGui (ExpressionGui, ExpressionN)
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

makeUnwrapEventMap ::
    (MonadReader env m, Config.HasConfig env, Monad f) =>
    Sugar.HoleArg (T f) (ExpressionN f a) -> Widget.Id ->
    m (EventMap (T f GuiState.Update))
makeUnwrapEventMap arg openHoleId =
    Lens.view Config.config
    <&>
    \config ->
    let unwrapKeys = Config.hole config & Config.holeUnwrapKeys
    in
    case arg ^? Sugar.haUnwrap . Sugar._UnwrapAction of
    Just unwrap ->
        E.keysEventMapMovesCursor (unwrapKeys ++ Config.replaceParentKeys config)
        (Momentu.Doc ["Edit", "Unwrap"]) $ WidgetIds.fromEntityId <$> unwrap
    Nothing ->
        -- When pressing "space" in a wrapper hole, open its hole.
        pure openHoleId
        & E.keysEventMapMovesCursor unwrapKeys doc
        where
            doc = Momentu.Doc ["Navigation", "Hole", "Open"]

make ::
    Monad m =>
    Widget.Id ->
    Sugar.HoleArg (T m) (ExpressionN m ExprGui.Payload) ->
    ExprGuiM m (ExpressionGui m)
make openHoleId arg =
    do
        theme <- Lens.view Theme.theme
        let frameColor =
                theme &
                case arg ^. Sugar.haUnwrap of
                Sugar.UnwrapAction {} -> Theme.typeIndicatorMatchColor
                Sugar.UnwrapTypeMismatch {} -> Theme.typeIndicatorErrorColor
        let frameWidth = Theme.typeIndicatorFrameWidth theme <&> realToFrac
        (argGui, resultPicker) <-
            ExprGuiM.makeSubexpression (arg ^. Sugar.haExpr)
            & ExprGuiM.listenResultPicker
        unwrapEventMap <-
            makeUnwrapEventMap arg openHoleId
            <&> const
            >>= withPicker resultPicker
        Momentu.addInnerFrame
            ?? frameColor ?? frameWidth
            ?? Momentu.pad (frameWidth & _2 .~ 0) argGui
            <&> Momentu.weakerEvents unwrapEventMap
