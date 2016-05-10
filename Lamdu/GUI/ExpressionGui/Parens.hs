{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionGui.Parens
    ( addHighlightedTextParens
    ) where

import           Control.Lens.Operators
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Widget as Widget
import           Graphics.UI.Bottle.Widgets.Layout (Layout)
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Graphics.UI.Bottle.WidgetsEnvT (WidgetEnvT)
import qualified Graphics.UI.Bottle.WidgetsEnvT as WE
import           Lamdu.GUI.WidgetIds (parensPrefix)
import qualified Lamdu.GUI.WidgetIds as WidgetIds

import           Prelude.Compat

addTextParensI ::
    (Monad m, Applicative f) =>
    AnimId -> Widget.Id ->
    WidgetEnvT m
    (Layout (f Widget.EventResult) -> Layout (f Widget.EventResult))
addTextParensI parenId rParenId =
    do
        beforeParen <- label "("
        afterParen <- BWidgets.makeFocusableView rParenId <*> label ")"
        return $ \widget ->
            Layout.hbox 0.5
            [ beforeParen & Layout.fromCenteredWidget
            , widget
            , afterParen & Layout.fromCenteredWidget
            ]
    where
        label str = BWidgets.makeLabel str $ parensPrefix parenId

highlightExpression :: Config -> Widget.Widget a -> Widget.Widget a
highlightExpression config =
    Widget.backgroundColor (Config.layerParensHighlightBG (Config.layers config))
    WidgetIds.parenHighlightId $
    Config.parenHighlightColor config

addHighlightedTextParens ::
    (Monad m, Applicative f) =>
    Widget.Id ->
    ExprGuiM m
    (Layout (f Widget.EventResult) -> Layout (f Widget.EventResult))
addHighlightedTextParens myId =
    do
        mInsideParenId <- WE.subCursor rParenId & ExprGuiM.widgetEnv
        config <- ExprGuiM.readConfig
        (.)
            <$> addTextParensI animId rParenId
            ?? case mInsideParenId of
                Nothing -> id
                Just _ -> Layout.widget %~ highlightExpression config
            & ExprGuiM.widgetEnv
    where
        rParenId = Widget.joinId myId [")"]
        animId = Widget.toAnimId myId
