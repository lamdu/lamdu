module Lamdu.CodeEdit.ExpressionEdit.Modify
  ( eventMap, wrapEventMap, replaceEventMap
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widget (EventHandlers)
import Lamdu.CharClassification (operatorChars)
import Lamdu.CodeEdit.ExpressionEdit.HoleEdit (HoleState(..))
import Lamdu.Config (Config)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

wrapEventMap :: MonadA m => Config -> Sugar.Actions m -> EventHandlers (Transaction m)
wrapEventMap config actions =
  mconcat
  [ (fmap . fmap) Widget.eventResultFromCursor .
    E.charGroup "Operator" (E.Doc ["Edit", "Apply operator"]) operatorChars $
    \c _isShifted -> HoleEdit.setHoleStateAndJump (HoleState [c]) =<< wrap
  , Widget.keysEventMapMovesCursor
    (Config.wrapKeys config) (E.Doc ["Edit", doc])
    (FocusDelegator.delegatingId . WidgetIds.fromGuid <$> wrap)
  ]
  where
    (doc, wrap) =
      case actions ^. Sugar.wrap of
      Sugar.AlreadyWrapped guid -> ("Jump to wrapper", return guid)
      Sugar.WrapAction action -> ("Wrap", action)

replaceEventMap :: MonadA m => Config -> Sugar.Actions m -> EventHandlers (Transaction m)
replaceEventMap config actions =
  concat
  [ actions ^.. Sugar.mSetToInnerExpr . Lens._Just
    <&> fmap WidgetIds.fromGuid
    <&>
      Widget.keysEventMapMovesCursor delKeys
      (E.Doc ["Edit", "Replace with inner expression"])
  , actions ^.. Sugar.mSetToHole . Lens._Just
    <&> fmap (FocusDelegator.delegatingId . WidgetIds.fromGuid)
    <&>
      Widget.keysEventMapMovesCursor delKeys
      (E.Doc ["Edit", "Replace expression"])
  ] ^? Lens.traversed
  & fromMaybe mempty
  where
    delKeys = Config.replaceKeys config ++ Config.delKeys config

eventMap :: MonadA m => Config -> Sugar.Actions m -> EventHandlers (Transaction m)
eventMap config actions =
  mconcat
  [ wrapEventMap config actions
  , replaceEventMap config actions
  ]
