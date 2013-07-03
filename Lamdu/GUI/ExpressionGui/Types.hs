{-# LANGUAGE TemplateHaskell #-}
module Lamdu.GUI.ExpressionGui.Types
  ( WidgetT, ExpressionGui(..), egWidget, egAlignment
  , Precedence, MyPrecedence(..), ParentPrecedence(..)
  ) where

import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widget (Widget, R)
import qualified Control.Lens as Lens

type WidgetT m = Widget (Transaction m)

data ExpressionGui m = ExpressionGui
  { _egWidget :: WidgetT m
  , _egAlignment :: R
  }
Lens.makeLenses ''ExpressionGui

type Precedence = Int
newtype MyPrecedence = MyPrecedence Precedence
newtype ParentPrecedence = ParentPrecedence Precedence
