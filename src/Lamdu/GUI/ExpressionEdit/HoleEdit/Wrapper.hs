{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
-- | Wrapper hole
module Lamdu.GUI.ExpressionEdit.HoleEdit.Wrapper
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.View as View
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.Config.Theme as Theme
import           Lamdu.GUI.ExpressionEdit.HoleEdit.WidgetIds (WidgetIds(..))
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (ExpressionN)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction.Transaction

makeUnwrapEventMap ::
    (MonadReader env m, Config.HasConfig env, Monad f) =>
    Sugar.HoleArg f (ExpressionN f a) -> WidgetIds ->
    m (Widget.EventMap (T f Widget.EventResult))
makeUnwrapEventMap arg widgetIds =
    Lens.view Config.config <&> Config.hole <&> Config.holeUnwrapKeys
    <&>
    \unwrapKeys ->
    case arg ^? Sugar.haUnwrap . Sugar._UnwrapAction of
    Just unwrap ->
        Widget.keysEventMapMovesCursor unwrapKeys
        (E.Doc ["Edit", "Unwrap"]) $ WidgetIds.fromEntityId <$> unwrap
    Nothing ->
        hidOpenSearchTerm widgetIds & pure
        & Widget.keysEventMapMovesCursor unwrapKeys doc
        where
            doc = E.Doc ["Navigation", "Hole", "Open"]

make ::
    Monad m => WidgetIds ->
    Sugar.HoleArg m (ExpressionN m ExprGuiT.Payload) ->
    ExprGuiM m (ExpressionGui m)
make widgetIds arg =
    do
        theme <- Lens.view Theme.theme
        let frameColor =
                theme &
                case arg ^. Sugar.haUnwrap of
                Sugar.UnwrapAction {} -> Theme.typeIndicatorMatchColor
                Sugar.UnwrapTypeMismatch {} -> Theme.typeIndicatorErrorColor
        let frameWidth = Theme.typeIndicatorFrameWidth theme <&> realToFrac
        argGui <- ExprGuiM.makeSubexpression (arg ^. Sugar.haExpr)
        unwrapEventMap <- makeUnwrapEventMap arg widgetIds
        View.addInnerFrame ?? frameColor ?? frameWidth ?? View.pad (frameWidth & _2 .~ 0) argGui
            <&> E.weakerEvents unwrapEventMap
