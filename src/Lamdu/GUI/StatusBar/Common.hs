-- | Common utilities for status bar widgets
module Lamdu.GUI.StatusBar.Common
    ( hspacer
    ) where

import qualified Control.Lens as Lens
import           GUI.Momentu.View (View)
import qualified GUI.Momentu.Widgets.Spacer as Spacer
import qualified Lamdu.Config.Theme as Theme

import           Lamdu.Prelude

hspacer ::
    (MonadReader env m, Spacer.HasStdSpacing env, Theme.HasTheme env) => m View
hspacer = do
    hSpaceCount <-
        Lens.view Theme.theme <&> Theme.statusBar <&> Theme.statusBarHSpaces
    Spacer.getSpaceSize <&> (^. _1) <&> (* hSpaceCount) <&> Spacer.makeHorizontal
