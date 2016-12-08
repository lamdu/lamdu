{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}
-- | Wrapper hole
module Lamdu.GUI.ExpressionEdit.HoleEdit.Wrapper
    ( make
    ) where

import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (ExpressionN)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction.Transaction

modifyWrappedEventMap ::
    Applicative f =>
    Config -> Bool -> Sugar.HoleArg m (ExpressionN m a) -> WidgetIds ->
    Widget.EventMap (f Widget.EventResult) ->
    Widget.EventMap (f Widget.EventResult)
modifyWrappedEventMap config argIsFocused arg WidgetIds{..} eventMap
    | argIsFocused =
        eventMap <>
        Widget.keysEventMapMovesCursor (Config.leaveSubexpressionKeys config)
        (E.Doc ["Navigation", "Go to parent wrapper"]) (pure hidWrapper)
    | otherwise =
        Widget.keysEventMapMovesCursor (Config.enterSubexpressionKeys config)
        (E.Doc ["Navigation", "Go to wrapped expr"]) .
        -- TODO: This is ugly: Who says it's in a FocusDelegator?
        pure . WidgetIds.notDelegatingId . WidgetIds.fromExprPayload $
        arg ^. Sugar.haExpr . Sugar.rPayload

makeUnwrapEventMap ::
    (Monad m, Monad f) =>
    Sugar.HoleArg f (ExpressionN f a) -> WidgetIds ->
    ExprGuiM m (Widget.EventMap (T f Widget.EventResult))
makeUnwrapEventMap arg WidgetIds{..} =
    do
        config <- ExprGuiM.readConfig
        let Config.Hole{..} = Config.hole config
        pure $
            case arg ^? Sugar.haUnwrap . Sugar._UnwrapAction of
            Just unwrap ->
                Widget.keysEventMapMovesCursor
                (holeUnwrapKeys ++ Config.delKeys config)
                (E.Doc ["Edit", "Unwrap"]) $ WidgetIds.fromEntityId <$> unwrap
            Nothing ->
                Widget.keysEventMapMovesCursor holeUnwrapKeys doc $ pure hidOpenSearchTerm
                where
                        doc = E.Doc ["Navigation", "Hole", "Open"]

make ::
    Monad m => WidgetIds ->
    Sugar.HoleArg m (ExpressionN m ExprGuiT.Payload) ->
    ExprGuiM m (ExpressionGui m)
make WidgetIds{..} arg =
    do
        config <- ExprGuiM.readConfig
        let Config.Hole{..} = Config.hole config
        let frameColor =
                config &
                case arg ^. Sugar.haUnwrap of
                Sugar.UnwrapAction {} -> Config.typeIndicatorMatchColor
                Sugar.UnwrapTypeMismatch {} -> Config.typeIndicatorErrorColor
        let frameWidth = Config.typeIndicatorFrameWidth config <&> realToFrac
        argGui <-
            arg ^. Sugar.haExpr
            & ExprGuiM.makeSubexpression (const 0)
        let argIsFocused = ExpressionGui.egIsFocused argGui
        unwrapEventMap <- makeUnwrapEventMap arg WidgetIds{..}
        ExpressionGui.makeFocusableView hidWrapper
            <&> (TreeLayout.alignedWidget %~)
            ?? argGui
            <&> TreeLayout.pad (frameWidth & _2 .~ 0)
            <&> TreeLayout.widget %~
                Widget.addInnerFrame frameId frameColor frameWidth
            <&> TreeLayout.widget . Widget.eventMap %~
                modifyWrappedEventMap config argIsFocused arg WidgetIds{..}
            <&> TreeLayout.widget %~ Widget.weakerEvents unwrapEventMap
    where
        frameId = Widget.toAnimId hidWrapper <> ["hole frame"]
