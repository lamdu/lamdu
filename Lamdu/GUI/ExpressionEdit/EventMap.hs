module Lamdu.GUI.ExpressionEdit.EventMap (make, modifyEventMap) where

import Control.Applicative ((<$>), (<$), Applicative(..))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Graphics.UI.Bottle.Widget (EventHandlers)
import Lamdu.CharClassification (operatorChars)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionEdit.HoleEdit.Info (HoleState(..))
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, HolePickers, holePickersAddDocPrefix, holePickersAction)
import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.HoleEdit.Info as HoleInfo
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Expression as SugarExpr
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

make ::
  MonadA m => HolePickers m -> Sugar.Expression name m ExprGuiM.Payload ->
  ExprGuiM m (EventHandlers (T m))
make holePickers sExpr =
  mconcat <$> sequenceA
  [ maybe (return mempty) (actionsEventMap holePickers sExpr) $
    pl ^. Sugar.plActions
  , jumpHolesEventMap holePickers pl
  ]
  where
    pl = sExpr ^. Sugar.rPayload

jumpHolesEventMap ::
  MonadA m => HolePickers m ->
  Sugar.Payload name m ExprGuiM.Payload ->
  ExprGuiM m (EventHandlers (T m))
jumpHolesEventMap holePickers pl = do
  isSelected <- ExprGuiM.widgetEnv . WE.isSubCursor $ WidgetIds.fromGuid exprGuid
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    doc dirStr =
      E.Doc
      [ "Navigation"
      , holePickersAddDocPrefix holePickers $ "Jump to " ++ dirStr ++ " hole"
      ]
    jumpEventMap keys dirStr lens =
      maybe mempty
      (Widget.keysEventMapMovesCursor (keys config) (doc dirStr) .
       (<$ holePickersAction holePickers) . WidgetIds.fromGuid) $ hg ^. lens
  pure $
    if isSelected
    then
      mconcat
      [ jumpEventMap Config.jumpToNextHoleKeys "next" ExprGuiM.hgMNextHole
      , jumpEventMap Config.jumpToPrevHoleKeys "previous" ExprGuiM.hgMPrevHole
      ]
    else mempty
  where
    exprGuid = pl ^. Sugar.plGuid
    hg = pl ^. Sugar.plData . ExprGuiM.plHoleGuids

actionsEventMap ::
  MonadA m => HolePickers m ->
  Sugar.Expression name m a -> Sugar.Actions m ->
  ExprGuiM m (EventHandlers (T m))
actionsEventMap holePickers sExpr actions = do
  isSelected <- ExprGuiM.widgetEnv . WE.isSubCursor $ WidgetIds.fromGuid exprGuid
  config <- ExprGuiM.widgetEnv WE.readConfig
  clipboard <-
    case sExpr ^? Sugar.rBody . SugarExpr.bodyHole of
    Just hole -> pasteEventMap hole
    Nothing ->
      return .
      mkEventMap (Config.cutKeys config) (E.Doc ["Edit", "Cut"]) id $
      actions ^. Sugar.cut
  let
    delKeys = Config.replaceKeys config ++ Config.delKeys config
    replace
      | isSelected = replaceEventMap config actions
      | otherwise =
        mkEventMap delKeys (E.Doc ["Navigation", "Select parent"])
        FocusDelegator.notDelegatingId $ return exprGuid
    wrapAndOp = wrapAndOpEventMap enableWrap holePickers config actions
  return $ mconcat
    [ wrapAndOp
    , replace
    , clipboard
    ]
  where
    enableWrap =
      Lens.nullOf (Sugar.rBody . SugarExpr.bodyHole) sExpr
      || not (null holePickers)
    exprGuid = sExpr ^. Sugar.rPayload . Sugar.plGuid
    mkEventMap keys doc f =
      Widget.keysEventMapMovesCursor keys doc .
      fmap (f . WidgetIds.fromGuid)

pasteEventMap ::
  MonadA m =>
  Sugar.Hole name m expr ->
  ExprGuiM m (Widget.EventHandlers (T m))
pasteEventMap hole = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  return .
    maybe mempty
    (Widget.keysEventMapMovesCursor
     (Config.pasteKeys config) (E.Doc ["Edit", "Paste"]) .
     fmap WidgetIds.fromGuid) $
    hole ^? Sugar.holeMActions . Lens._Just . Sugar.holePaste . Lens._Just

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

wrapAndOpEventMap ::
  MonadA m =>
  Bool -> HolePickers m -> Config ->
  Sugar.Actions m -> EventHandlers (T m)
wrapAndOpEventMap enableWrap holePickers config actions =
  case actions ^. Sugar.wrap of
  Sugar.WrapAction wrap ->
    mconcat $ concat
    [ [ applyOperatorEventMap holePickers wrap ]
    , [ Widget.keysEventMapMovesCursor
        (Config.wrapKeys config) (E.Doc ["Edit", holePickersAddDocPrefix holePickers "Wrap"]) $
        holePickersAction holePickers *>
        (FocusDelegator.delegatingId . WidgetIds.fromGuid <$> wrap)
      | enableWrap
      ]
    ]
  Sugar.WrapperAlready -> applyOperatorEventMap holePickers . return $ actions ^. Sugar.storedGuid
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

modifyEventMap ::
  MonadA m => HolePickers m ->
  Config -> Sugar.Actions m -> EventHandlers (T m)
modifyEventMap holePickers config actions =
  mconcat
  [ wrapAndOpEventMap True holePickers config actions
  , replaceEventMap config actions
  ]
