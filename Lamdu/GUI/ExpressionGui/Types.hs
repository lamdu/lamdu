{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.ExpressionGui.Types
  ( ExpressionGui(..), egWidget, egAlignment
  ) where

import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widget (Widget, R)
import qualified Control.Lens as Lens

type T = Transaction

data ExpressionGui m = ExpressionGui
  { _egWidget :: Widget (T m)
  , _egAlignment :: R
  }
Lens.makeLenses ''ExpressionGui
