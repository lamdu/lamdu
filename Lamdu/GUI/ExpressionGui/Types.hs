module Lamdu.GUI.ExpressionGui.Types
    ( ExpressionGui
    ) where

import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widgets.Layout (Layout)

type T = Transaction

type ExpressionGui m = Layout (T m)
