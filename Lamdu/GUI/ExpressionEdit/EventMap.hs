module Lamdu.GUI.ExpressionEdit.EventMap (make) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widget (EventHandlers)
import Lamdu.GUI.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionEdit.Modify as Modify
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

make ::
  MonadA m => Sugar.Expression name m a ->
  ExprGuiM m (EventHandlers (Transaction m))
make sExpr =
  maybe (return mempty) (actionsEventMap sExpr) $
  sExpr ^. Sugar.rPayload . Sugar.plActions

actionsEventMap ::
  MonadA m =>
  Sugar.Expression name m a -> Sugar.Actions m ->
  ExprGuiM m (EventHandlers (Transaction m))
actionsEventMap sExpr actions = do
  isSelected <- ExprGuiM.widgetEnv . WE.isSubCursor $ WidgetIds.fromGuid exprGuid
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    delKeys = Config.replaceKeys config ++ Config.delKeys config
    replace
      | isSelected = Modify.replaceEventMap config actions
      | otherwise =
        mkEventMap delKeys (E.Doc ["Navigation", "Select parent"])
        FocusDelegator.notDelegatingId $ return exprGuid
  clipboard <-
    case sExpr ^. Sugar.rBody of
    Sugar.BodyHole hole -> pasteEventMap hole
    Sugar.BodyInferred inferred -> pasteEventMap (inferred ^. Sugar.iHole)
    _ ->
      return .
      mkEventMap (Config.cutKeys config) (E.Doc ["Edit", "Cut"]) id $
      actions ^. Sugar.cut
  return $ mconcat
    [ Modify.wrapEventMap config actions
    , replace
    , clipboard
    ]
  where
    exprGuid = sExpr ^. Sugar.rPayload . Sugar.plGuid
    mkEventMap keys doc f =
      Widget.keysEventMapMovesCursor keys doc .
      fmap (f . WidgetIds.fromGuid)

pasteEventMap ::
  MonadA m =>
  Sugar.Hole name m expr ->
  ExprGuiM m (Widget.EventHandlers (Transaction m))
pasteEventMap hole = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  return .
    maybe mempty
    (Widget.keysEventMapMovesCursor
     (Config.pasteKeys config) (E.Doc ["Edit", "Paste"]) .
     fmap WidgetIds.fromGuid) $
    hole ^? Sugar.holeMActions . Lens._Just . Sugar.holePaste . Lens._Just
