module Lamdu.GUI.ExpressionEdit.Modify
  ( eventMap, wrapEventMap, replaceEventMap, applyOperatorEventMap
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widget (EventHandlers)
import Lamdu.CharClassification (operatorChars)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleState(..))
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

applyOperatorEventMap ::
  MonadA m => Transaction m Guid -> EventHandlers (Transaction m)
applyOperatorEventMap wrap =
  E.charGroup "Operator" (E.Doc ["Edit", "Apply operator"]) operatorChars action
  where
    action c _isShifted =
      Widget.eventResultFromCursor <$>
      (HoleInfo.setHoleStateAndJump (HoleState [c]) =<< wrap)

wrapEventMap :: MonadA m => Config -> Sugar.Actions m -> EventHandlers (Transaction m)
wrapEventMap config actions =
  fromMaybe mempty $ do
    wrap <- actions ^. Sugar.wrap
    let
      wrapEv =
        Widget.keysEventMapMovesCursor
        (Config.wrapKeys config) (E.Doc ["Edit", "Wrap"])
        (FocusDelegator.delegatingId . WidgetIds.fromGuid <$> wrap)
    Just $ mappend (applyOperatorEventMap wrap) wrapEv

replaceEventMap :: MonadA m => Config -> Sugar.Actions m -> EventHandlers (Transaction m)
replaceEventMap config actions =
  mconcat
  [ actionEventMap Sugar.mSetToInnerExpr
    "Replace with inner expression" $ Config.delKeys config
  , actionEventMap Sugar.mSetToHole
    "Replace expression" delKeys
  ]
  where
    actionEventMap lens doc keys =
      maybe mempty
      (Widget.keysEventMapMovesCursor keys
       (E.Doc ["Edit", doc]) .
       fmap WidgetIds.fromGuid) $ actions ^. lens
    delKeys = Config.replaceKeys config ++ Config.delKeys config

eventMap :: MonadA m => Config -> Sugar.Actions m -> EventHandlers (Transaction m)
eventMap config actions =
  mconcat
  [ wrapEventMap config actions
  , replaceEventMap config actions
  ]
