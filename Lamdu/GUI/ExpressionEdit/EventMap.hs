module Lamdu.GUI.ExpressionEdit.EventMap
  ( make
  , modifyEventMap
  , applyOperatorEventMap
  , cutEventMap
  , jumpHolesEventMap
  , replaceOrComeToParentEventMap
  ) where

import Control.Applicative ((<$>), Applicative(..))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (sequenceA)
import Graphics.UI.Bottle.Widget (EventHandlers)
import Lamdu.CharClassification (operatorChars)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionEdit.HoleEdit.State (HoleState(..), setHoleStateAndJump)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, HolePickers, holePickersAddDocPrefix, holePickersAction)
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction.Transaction

make ::
  MonadA m => HolePickers m -> Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  ExprGuiM m (EventHandlers (T m))
make holePickers pl =
  mconcat <$> sequenceA
  [ maybe (return mempty) (actionsEventMap holePickers) $
    pl ^. Sugar.plActions
  , replaceOrComeToParentEventMap pl
  , jumpHolesEventMap holePickers pl
  ]

mkEventMap ::
  (Functor f, Functor g) =>
  [E.ModKey] -> E.Doc ->
  (f Widget.Id -> g Widget.Id) ->
  f Guid -> EventHandlers g
mkEventMap keys doc f =
  Widget.keysEventMapMovesCursor keys doc .
  f . fmap WidgetIds.fromGuid

mkEventMapWithPickers ::
  (Functor f, MonadA m) =>
  HolePickers m ->
  [E.ModKey] -> E.Doc ->
  (f Widget.Id -> T m Widget.Id) ->
  f Guid -> EventHandlers (T m)
mkEventMapWithPickers holePickers keys doc f =
  mkEventMap keys doc ((holePickersAction holePickers *>) . f)

isExprSelected :: MonadA m => Sugar.Payload name f a -> ExprGuiM m Bool
isExprSelected pl =
  ExprGuiM.widgetEnv . WE.isSubCursor . WidgetIds.fromGuid $
  pl ^. Sugar.plGuid

jumpHolesEventMap ::
  MonadA m => HolePickers m ->
  Sugar.Payload name m ExprGuiM.Payload ->
  ExprGuiM m (EventHandlers (T m))
jumpHolesEventMap holePickers pl = do
  isSelected <- isExprSelected pl
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    jumpEventMap keys dirStr lens =
      maybe mempty
      (mkEventMapWithPickers holePickers (keys config)
       (E.Doc ["Navigation", jumpDoc dirStr]) id . pure) $
      hg ^. lens
  pure $
    if isSelected
    then
      mconcat
      [ jumpEventMap Config.jumpToNextHoleKeys "next" ExprGuiM.hgMNextHole
      , jumpEventMap Config.jumpToPrevHoleKeys "previous" ExprGuiM.hgMPrevHole
      ]
    else mempty
  where
    jumpDoc dirStr =
      holePickersAddDocPrefix holePickers $ "Jump to " ++ dirStr ++ " hole"
    hg = pl ^. Sugar.plData . ExprGuiM.plHoleGuids

cutEventMap :: Functor m => Config -> Sugar.Actions m -> EventHandlers (T m)
cutEventMap config actions =
  mkEventMap (Config.cutKeys config) (E.Doc ["Edit", "Cut"]) id $
  actions ^. Sugar.cut

replaceOrComeToParentEventMap ::
  MonadA m =>
  Sugar.Payload Sugar.Name m ExprGuiM.Payload ->
  ExprGuiM m (EventHandlers (T m))
replaceOrComeToParentEventMap pl =
  case pl ^. Sugar.plActions of
  Nothing -> return mempty
  Just actions -> do
    isSelected <- isExprSelected pl
    config <- ExprGuiM.widgetEnv WE.readConfig
    let delKeys = Config.replaceKeys config ++ Config.delKeys config
    return $ if isSelected
      then replaceEventMap config actions
      else
        mkEventMap delKeys (E.Doc ["Navigation", "Select parent"])
        (fmap FocusDelegator.notDelegatingId) . return $
        actions ^. Sugar.storedGuid

actionsEventMap ::
  MonadA m =>
  HolePickers m ->
  Sugar.Actions m ->
  ExprGuiM m (EventHandlers (T m))
actionsEventMap holePickers actions = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  return $ mconcat
    [ wrapEventMap holePickers config actions
    , applyOperatorEventMap holePickers actions
    , cutEventMap config actions
    ]

applyOperatorEventMap ::
  MonadA m => HolePickers m -> Sugar.Actions m -> EventHandlers (T m)
applyOperatorEventMap holePickers actions =
  case actions ^. Sugar.wrap of
  Sugar.WrapAction wrap -> action wrap
  Sugar.WrapperAlready -> action . return $ actions ^. Sugar.storedGuid
  Sugar.WrapNotAllowed -> mempty
  where
    doc = E.Doc ["Edit", holePickersAddDocPrefix holePickers "Apply operator"]
    action wrap = E.charGroup "Operator" doc operatorChars $ \c _isShifted -> do
      holePickersAction holePickers
      Widget.eventResultFromCursor <$>
        (setHoleStateAndJump (HoleState [c]) =<< wrap)

wrapEventMap ::
  MonadA m =>
  HolePickers m -> Config ->
  Sugar.Actions m -> EventHandlers (T m)
wrapEventMap holePickers config actions =
  case actions ^. Sugar.wrap of
  Sugar.WrapAction wrap ->
    mkEventMapWithPickers holePickers
    (Config.wrapKeys config)
    (E.Doc ["Edit", holePickersAddDocPrefix holePickers "Wrap"])
    (fmap FocusDelegator.delegatingId) wrap
  Sugar.WrapperAlready -> mempty
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
      maybe mempty (mkEventMap keys (E.Doc ["Edit", doc]) id) $
      actions ^. lens
    delKeys = Config.replaceKeys config ++ Config.delKeys config

modifyEventMap ::
  MonadA m => HolePickers m ->
  Config -> Sugar.Actions m -> EventHandlers (T m)
modifyEventMap holePickers config actions =
  mconcat
  [ wrapEventMap holePickers config actions
  , applyOperatorEventMap holePickers actions
  , replaceEventMap config actions
  ]
