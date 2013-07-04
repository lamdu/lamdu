module Lamdu.GUI.ExpressionEdit.Modify
  ( removeConflicts, eventMap, wrapEventMap, replaceEventMap
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Graphics.UI.Bottle.Widget (EventHandlers)
import Lamdu.CharClassification (operatorChars)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleState(..))
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

removeConflicts ::
  MonadA m => Config -> EventHandlers (T m) -> EventHandlers (T m)
removeConflicts config = E.deleteKeys $ map (E.KeyEvent E.Press) keys
  where
    keys =
      mconcat
      [ Config.wrapKeys
      , Config.delKeys
      , Config.replaceKeys
      ] config

applyOperatorEventMap ::
  MonadA m => T m Guid -> EventHandlers (T m)
applyOperatorEventMap wrap =
  E.charGroup "Operator" (E.Doc ["Edit", "Apply operator"]) operatorChars action
  where
    action c _isShifted =
      Widget.eventResultFromCursor <$>
      (HoleInfo.setHoleStateAndJump (HoleState [c]) =<< wrap)

wrapEventMap :: MonadA m => Config -> Sugar.Actions m -> EventHandlers (T m)
wrapEventMap config actions =
  case actions ^. Sugar.wrap of
  Sugar.WrapAction wrap ->
    mconcat
    [ applyOperatorEventMap wrap
    , Widget.keysEventMapMovesCursor
      (Config.wrapKeys config) (E.Doc ["Edit", "Wrap"])
      (FocusDelegator.delegatingId . WidgetIds.fromGuid <$> wrap)
    ]
  Sugar.WrapperAlready ->
    applyOperatorEventMap . return $ actions ^. Sugar.storedGuid
  Sugar.WrapNotAllowed -> mempty

replaceEventMap :: MonadA m => Config -> Sugar.Actions m -> EventHandlers (T m)
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

eventMap :: MonadA m => Config -> Sugar.Actions m -> EventHandlers (T m)
eventMap config actions =
  mconcat
  [ wrapEventMap config actions
  , replaceEventMap config actions
  ]
