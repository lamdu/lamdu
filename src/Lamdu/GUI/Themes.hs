-- | GUI choice of themes
module Lamdu.GUI.Themes
    ( makeStatusWidget
    , Themes.Selection
    ) where

import           Data.Property (Property(..))
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.Hover as Hover
import qualified GUI.Momentu.State as GuiState
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Config (HasConfig)
import qualified Lamdu.Config as Config
import           Lamdu.Config.Theme (HasTheme)
import qualified Lamdu.GUI.StatusBar.Common as StatusBar
import qualified Lamdu.Themes as Themes

import           Lamdu.Prelude

makeStatusWidget ::
    ( MonadReader env m, Applicative f
    , HasConfig env, HasTheme env
    , TextView.HasStyle env, Element.HasAnimIdPrefix env, GuiState.HasCursor env
    , Hover.HasStyle env
    ) =>
    [Themes.Selection] -> Property f Text -> m (StatusBar.StatusWidget f)
makeStatusWidget themeNames prop =
    StatusBar.makeSwitchStatusWidget "Theme" Config.changeThemeKeys prop
    (map (join (,)) themeNames)
