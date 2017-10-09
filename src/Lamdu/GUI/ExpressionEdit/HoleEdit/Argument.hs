{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
-- | Wrapper hole
module Lamdu.GUI.ExpressionEdit.HoleEdit.Argument
    ( make
    ) where

import qualified Control.Lens as Lens
import           Data.Store.Transaction (Transaction)
import qualified GUI.Momentu as Momentu
import qualified GUI.Momentu.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.GUI.ExpressionGui.Types (ExpressionN)
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

makeUnwrapEventMap ::
    (MonadReader env m, Config.HasConfig env, Monad f) =>
    Sugar.HoleArg (T f) (ExpressionN f a) -> Widget.Id ->
    m (Widget.EventMap (T f Widget.EventResult))
makeUnwrapEventMap arg openHoleId =
    Lens.view Config.config
    <&>
    \config ->
    case arg ^? Sugar.haUnwrap . Sugar._UnwrapAction of
    Just unwrap ->
        unwrap <&> WidgetIds.fromEntityId
        & Widget.keysEventMapMovesCursor
            (Config.holeUnwrapKeys (Config.hole config) <> Config.replaceParentKeys config)
            (Momentu.Doc ["Edit", "Unwrap"])
    Nothing ->
        -- When pressing "space" in a wrapper hole, open its hole.
        pure openHoleId
        & Widget.keysEventMapMovesCursor (Config.wrapKeys config) doc
        where
            doc = Momentu.Doc ["Navigation", "Hole", "Open"]

make ::
    Monad m =>
    Widget.Id ->
    Sugar.HoleArg (T m) (ExpressionN m ExprGuiT.Payload) ->
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
        argGui <- ExprGuiM.makeSubexpression (arg ^. Sugar.haExpr)
        unwrapEventMap <- makeUnwrapEventMap arg openHoleId
        Momentu.addInnerFrame
            ?? frameColor ?? frameWidth
            ?? Momentu.pad (frameWidth & _2 .~ 0) argGui
            <&> Momentu.weakerEvents unwrapEventMap
