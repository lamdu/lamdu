module Lamdu.GUI.LightLambda
    ( underline
    ) where

import           GUI.Momentu.Widgets.TextView (Underline(..))
import           Lamdu.Config.Theme (Theme)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.Config.Theme.TextColors as TextColors

underline :: Theme -> Underline
underline theme =
    Underline
    { _underlineColor = TextColors.lightLambdaUnderlineColor (Theme.textColors theme)
    , _underlineWidth = Theme.wideUnderlineWidth theme
    }
