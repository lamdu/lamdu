module Lamdu.CodeEdit.ExpressionEdit.Wrap
  ( eventMap
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widget (EventHandlers)
import Lamdu.CharClassification (operatorChars)
import Lamdu.CodeEdit.ExpressionEdit.HoleEdit (HoleState(..))
import Lamdu.Config.Default (defaultConfig)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetIds as WidgetIds

eventMap :: MonadA m => Sugar.Actions m -> EventHandlers (Transaction m)
eventMap actions =
  mconcat
  [ (fmap . fmap) Widget.eventResultFromCursor .
    E.charGroup "Operator" (E.Doc ["Edit", "Apply operator"]) operatorChars $
    \c _isShifted -> HoleEdit.setHoleStateAndJump (HoleState [c]) =<< wrap
  , Widget.keysEventMapMovesCursor
    (Config.wrapKeys defaultConfig) (E.Doc ["Edit", doc])
    (WidgetIds.fromGuid <$> wrap)
  ]
  where
    (doc, wrap) =
      case actions ^. Sugar.wrap of
      Sugar.AlreadyWrapped guid -> ("Jump to wrapper", return guid)
      Sugar.WrapAction action -> ("Wrap", action)
