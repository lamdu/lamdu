{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets
    ( makeChoiceWidget
    ) where

import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Choice as Choice
import           Lamdu.GUI.WidgetsEnvT (WidgetEnvT)
import           Lamdu.Prelude

makeChoiceWidget ::
    (Eq a, Monad m, Applicative f) =>
    (a -> f ()) -> [(a, Widget (f Widget.EventResult))] -> a ->
    Choice.Config -> Widget.Id -> WidgetEnvT m (Widget (f Widget.EventResult))
makeChoiceWidget choose children curChild choiceConfig =
    Choice.make choiceConfig (children <&> annotate)
    where
        annotate (item, widget) =
            ( if item == curChild then Choice.Selected else Choice.NotSelected
            , choose item
            , widget
            )
