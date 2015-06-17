module Lamdu.GUI.EvalView
    ( make
    ) where

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Graphics.UI.Bottle.Animation (AnimId)
import           Graphics.UI.Bottle.View (View)
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import           Lamdu.Eval.Results (ComputedVal(..))
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

make :: MonadA m => AnimId -> ComputedVal () -> ExprGuiM m View
make animId val =
    BWidgets.makeTextView text animId & ExprGuiM.widgetEnv
    where
        text = show val & truncateStr 20

truncateStr :: Int -> String -> String
truncateStr n s
    | l > n = take (n `div` 3) s ++ ".." ++ drop (l - (2 * n `div` 3)) s
    | otherwise = s
    where
        l = length s
