module Lamdu.GUI.ExpressionEdit.Modify
  ( eventMap, wrapEventMap, replaceEventMap
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Graphics.UI.Bottle.Widget (EventHandlers)
import Lamdu.CharClassification (operatorChars)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleState(..))
import Lamdu.GUI.ExpressionGui.Monad (HolePickers, holePickersAddDocPrefix, holePickersAction)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

applyOperatorEventMap ::
  MonadA m => HolePickers m -> T m Guid -> EventHandlers (T m)
applyOperatorEventMap holePickers wrap =
  E.charGroup "Operator" doc operatorChars action
  where
    doc = E.Doc ["Edit", holePickersAddDocPrefix holePickers "Apply operator"]
    action c _isShifted = do
      holePickersAction holePickers
      Widget.eventResultFromCursor <$>
        (HoleInfo.setHoleStateAndJump (HoleState [c]) =<< wrap)

wrapEventMap :: MonadA m => HolePickers m -> Config -> Sugar.Actions m -> EventHandlers (T m)
wrapEventMap holePickers config actions =
  case actions ^. Sugar.wrap of
  Sugar.WrapAction wrap ->
    mconcat
    [ applyOperatorEventMap holePickers wrap
    , Widget.keysEventMapMovesCursor
      (Config.wrapKeys config) (E.Doc ["Edit", holePickersAddDocPrefix holePickers "Wrap"]) $
      holePickersAction holePickers *>
      (FocusDelegator.delegatingId . WidgetIds.fromGuid <$> wrap)
    ]
  Sugar.WrapperAlready ->
    applyOperatorEventMap holePickers . return $ actions ^. Sugar.storedGuid
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

eventMap :: MonadA m => HolePickers m -> Config -> Sugar.Actions m -> EventHandlers (T m)
eventMap holePickers config actions =
  mconcat
  [ wrapEventMap holePickers config actions
  , replaceEventMap config actions
  ]
