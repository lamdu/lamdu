{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.HoleEdit.Common
  ( makeBackground, diveIntoHole
  , searchTermWIdOfHoleGuid
  , inferredValue
  ) where

import Control.Lens.Operators
import Control.Monad (void)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Data.Expression.IRef (ExpressionM)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

searchTermWIdOfHoleGuid :: Guid -> Widget.Id
searchTermWIdOfHoleGuid = WidgetIds.searchTermId . FocusDelegator.delegatingId . WidgetIds.fromGuid

makeBackground :: Widget.Id -> Int -> Draw.Color -> Widget f -> Widget f
makeBackground myId level =
  Widget.backgroundColor level $
  mappend (Widget.toAnimId myId) ["hole background"]

-- TODO: We no longer use a FocusDelegator, use a different id
-- manipulation function
diveIntoHole :: Widget.Id -> Widget.Id
diveIntoHole = FocusDelegator.delegatingId

inferredValue :: Sugar.HoleInferred m -> ExpressionM m ()
inferredValue =
  (ExprLens.lambdaParamTypes .~ ExprUtil.pureHole) .
  void . Infer.iValue . (^. Sugar.hiInferred)
