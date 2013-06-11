module Lamdu.CodeEdit.ExpressionEdit.Wrap
  ( eventMap
  ) where

import Data.Monoid (mconcat)

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widget (EventHandlers)
import Lamdu.CodeEdit.ExpressionEdit.HoleEdit (HoleState(..))
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetIds as WidgetIds

eventMap :: MonadA m => Sugar.Actions m -> EventHandlers (Transaction m)
eventMap actions =
  mconcat
  [ Widget.keysEventMapMovesCursor
    Config.wrapKeys (E.Doc ["Edit", "Wrap"]) .
    fmap WidgetIds.fromGuid $ actions ^. Sugar.wrap
  , (fmap . fmap) Widget.eventResultFromCursor .
    E.charGroup "Operator" (E.Doc ["Edit", "Apply operator"])
    Config.operatorChars $ \c _isShifted -> do
      targetGuid <- actions ^. Sugar.wrap
      HoleEdit.setHoleStateAndJump (HoleState [c]) targetGuid
  ]
