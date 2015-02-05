{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Lamdu.GUI.ExpressionEdit.HoleEdit.Closed
  ( HoleDest(..)
  , make
  ) where

import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.Monad.Trans.Either.Utils (runMatcher, justToLeft)
import           Control.MonadA (MonadA)
import           Data.Maybe.Utils (maybeToMPlus)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Common (openHoleEventMap)
import           Lamdu.GUI.ExpressionEdit.HoleEdit.Info (EditableHoleInfo(..), HoleInfo(..), HoleIds(..))
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.SearchTerm as SearchTerm
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Wrapper as Wrapper
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM

data HoleDest = HoleDestClosed | HoleDestOpened

make ::
    MonadA m =>
    HoleInfo m -> Maybe (EditableHoleInfo m) ->
    (HoleDest, ExprGuiM m (ExpressionGui m))
make holeInfo mEditableHoleInfo =
  do
    justToLeft $ do
      arg <- maybeToMPlus (hiMArgument holeInfo)
      return (HoleDestClosed, Wrapper.make arg holeIds)
    return (HoleDestOpened, SearchTerm.make holeInfo mEditableHoleInfo)
    & runMatcher
    & _2 %~ (>>= onGui)
  where
    holeIds@HoleIds{..} = hiIds holeInfo
    onGui gui =
      do
        Config.Hole{..} <- ExprGuiM.readConfig <&> Config.hole
        gui
          & ExpressionGui.egWidget %~
            Widget.weakerEvents (openHoleEventMap holeOpenKeys holeIds)
          & ExpressionGui.egWidget %%~
            ExprGuiM.widgetEnv . BWidgets.respondToCursorPrefix hidClosed
