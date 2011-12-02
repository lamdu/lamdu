{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, TypeSynonymInstances,
             TupleSections #-}
module Graphics.UI.GLFWWidgets.Widgetable(Widgetable(..), Theme(..)) where

import           Control.Arrow (second)
import           Data.List.Utils (enumerate2d, nth)
import           Graphics.UI.GLFWWidgets.Widget(Widget)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.GLFWWidgets.FocusDelegator as FocusDelegator
import qualified Graphics.UI.GLFWWidgets.Grid as Grid
import qualified Graphics.UI.GLFWWidgets.TextEdit as TextEdit

data Theme = Theme {
  themeFont :: Draw.Font,
  themeFontSize :: Int,
  themeEmptyString :: String
  }

class Widgetable a where
  toWidget :: Theme -> a -> Widget a

instance Widgetable TextEdit.Model where
  toWidget (Theme font ptSize emptyStr) = TextEdit.make font ptSize emptyStr

instance Widgetable a => Widgetable (Grid.Cursor, [[a]]) where
  toWidget theme (cursor, childrenModels) =
    Grid.make (, childrenModels) cursor .
      (map . map) (
        liftInnerWidget .
        second (toWidget theme)
      ) .
      enumerate2d $
      childrenModels
    where
      setChildModel (rowIndex, colIndex) x =
        (nth rowIndex . nth colIndex . const) x childrenModels
      liftInnerWidget (index, childWidget) =
        fmap ((cursor,) . setChildModel index) childWidget

instance Widgetable a => Widgetable (FocusDelegator.Cursor, a) where
  toWidget theme (cursor, childModel) =
    FocusDelegator.make (, childModel) cursor .
    fmap (cursor,) . toWidget theme $
    childModel
