{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Lamdu.GUI.ExpressionEdit.BinderEdit
  ( make, diveToNameEdit, makeParamsEdit, makeResultEdit, makeWheres
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.List.Utils (nonEmptyAll)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..), (<>))
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CharClassification (operatorChars)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.Sugar.AddNames.Types (Name(..), NameSource(..))
import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ParamEdit as ParamEdit
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

nonOperatorName :: Name m -> Bool
nonOperatorName (Name NameSourceStored _ _ x) =
  nonEmptyAll (`notElem` operatorChars) x
nonOperatorName _ = False

makeBinderNameEdit ::
  MonadA m =>
  Maybe (Sugar.BinderActions m) ->
  Widget.EventHandlers (Transaction m) ->
  (String, ExprGuiM.SugarExpr m) ->
  Name m -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeBinderNameEdit mBinderActions rhsJumperEquals rhs name myId =
  do
    config <- ExprGuiM.widgetEnv WE.readConfig
    rhsJumper <- jumpToRHS (Config.jumpLHStoRHSKeys config) rhs
    let nameEditEventMap = mappend addFirstParamEventMap rhsJumper
        addFirstParamEventMap =
          maybe mempty
          ( Widget.keysEventMapMovesCursor (Config.addNextParamKeys config)
            (E.Doc ["Edit", "Add parameter"]) .
            fmap toEventMapAction ) $
          mBinderActions ^? Lens._Just . Sugar.baAddFirstParam
    ExpressionGui.makeNameOriginEdit name myId
      <&> Widget.weakerEvents nameEditEventMap . jumpToRHSViaEquals name
      <&> ExpressionGui.fromValueWidget
  where
    jumpToRHSViaEquals n widget
      | nonOperatorName n =
        widget
        & Widget.wEventMap %~ E.filterSChars (curry (/= ('=', E.NotShifted)))
        & Widget.weakerEvents rhsJumperEquals
      | otherwise = widget

makeWheres ::
  MonadA m =>
  [Sugar.WhereItem (Name m) m (ExprGuiM.SugarExpr m)] -> Widget.Id ->
  ExprGuiM m (Maybe (WidgetT m))
makeWheres [] _ = return Nothing
makeWheres whereItems myId =
  do
    config <- ExprGuiM.widgetEnv WE.readConfig
    whereLabel <-
      BWidgets.makeLabel "where" (Widget.toAnimId myId)
      & ExprGuiM.widgetEnv
      <&> ExpressionGui.fromValueWidget
      <&> ExpressionGui.scaleFromTop (realToFrac <$> Config.whereLabelScaleFactor config)
    itemEdits <-
      whereItems & reverse
      & ExpressionGui.listWithDelDests myId myId wiCursor
      & traverse makeWhereItemEdit
    ExpressionGui.hboxSpaced
      [ whereLabel
      , itemEdits
        <&> (,) 0
        & ExpressionGui.vboxDownwards
        & ExpressionGui.scaleFromTop (realToFrac <$> Config.whereScaleFactor config)
      ]
      ^. ExpressionGui.egWidget
      & Just & return
  where
    wiCursor = WidgetIds.fromEntityId . (^. Sugar.wiEntityId)

presentationModeChoiceConfig :: Config -> BWidgets.ChoiceWidgetConfig
presentationModeChoiceConfig config = BWidgets.ChoiceWidgetConfig
  { BWidgets.cwcFDConfig = FocusDelegator.Config
    { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.Key'Enter]
    , FocusDelegator.startDelegatingDoc = E.Doc ["Presentation Mode", "Select"]
    , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.Key'Enter]
    , FocusDelegator.stopDelegatingDoc = E.Doc ["Presentation Mode", "Choose selected"]
    }
  , BWidgets.cwcOrientation = Box.vertical
  , BWidgets.cwcExpandMode = BWidgets.ExplicitEntry
  , BWidgets.cwcBgLayer = Config.layerChoiceBG $ Config.layers config
  }

mkPresentationModeEdit ::
    MonadA m => Widget.Id ->
    Transaction.MkProperty m Sugar.PresentationMode ->
    ExprGuiM m (Widget (T m))
mkPresentationModeEdit myId prop = do
  cur <- ExprGuiM.transaction $ Transaction.getP prop
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    mkPair presentationMode = do
      widget <-
        ExprGuiM.withFgColor (Config.presentationChoiceColor config) .
        ExprGuiM.widgetEnv $
        BWidgets.makeFocusableLabel (show presentationMode) myId
      return (presentationMode, widget)
  pairs <- traverse mkPair [minBound..maxBound]
  fmap (Widget.scale (realToFrac <$> Config.presentationChoiceScaleFactor config)) .
    ExprGuiM.widgetEnv $
    BWidgets.makeChoiceWidget (Transaction.setP prop) pairs cur
    (presentationModeChoiceConfig config) myId

layout ::
  MonadA m =>
  ExpressionGui m -> [ExpressionGui m] ->
  ExpressionGui m -> Maybe (WidgetT m) ->
  Widget.Id ->
  ExprGuiM m (ExpressionGui m)
layout defNameEdit paramEdits bodyEdit mWheresEdit myId =
  do
    equals <-
      BWidgets.makeLabel "=" (Widget.toAnimId myId)
      & ExprGuiM.widgetEnv <&> ExpressionGui.fromValueWidget
    paramsEdit <-
      case paramEdits of
      [] -> return []
      xs -> xs <&> (,) 0 & ExpressionGui.vboxDownwardsSpaced <&> (:[])
    let
      topRow =
        ExpressionGui.hboxSpaced $
        defNameEdit : paramsEdit ++ [ equals, bodyEdit ]
    topRow
      & ExpressionGui.addBelow 0 (mWheresEdit ^.. Lens._Just <&> (,) 0)
      & return

make ::
  MonadA m =>
  Name m ->
  Sugar.Binder (Name m) m (ExprGuiM.SugarExpr m) ->
  Widget.Id ->
  ExprGuiM m (ExpressionGui m)
make name binder myId = do
  rhsJumperEquals <- jumpToRHS [E.ModKey E.noMods E.Key'Equal] rhs
  bodyEdit <- makeResultEdit (binder ^. Sugar.dMActions) params body myId
  presentationEdits <-
    traverse (mkPresentationModeEdit presentationChoiceId) $
    binder ^.. Sugar.dSetPresentationMode . Lens._Just
  defNameEdit <-
    makeBinderNameEdit (binder ^. Sugar.dMActions) rhsJumperEquals rhs name myId
    <&> ExpressionGui.addBelow 0 (map ((,) 0) presentationEdits)
  paramEdits <-
    makeParamsEdit ExprGuiM.ShowType myId params
    <&> Lens.mapped . ExpressionGui.egWidget
        %~ Widget.weakerEvents rhsJumperEquals
  mWheresEdit <- makeWheres (binder ^. Sugar.dWhereItems) myId
  layout defNameEdit paramEdits bodyEdit mWheresEdit myId
  where
    presentationChoiceId = Widget.joinId myId ["presentation"]
    rhs = ("Def Body", body)
    params = binder ^. Sugar.dParams
    body = binder ^. Sugar.dBody

toEventMapAction :: Sugar.EntityId -> Widget.Id
toEventMapAction = FocusDelegator.delegatingId . WidgetIds.fromEntityId

makeWhereItemEdit ::
  MonadA m =>
  (Widget.Id, Widget.Id, Sugar.WhereItem (Name m) m (ExprGuiM.SugarExpr m)) ->
  ExprGuiM m (ExpressionGui m)
makeWhereItemEdit (_prevId, nextId, item) = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    eventMap
      | Just wiActions <- item ^. Sugar.wiActions =
        mconcat
        [ Widget.keysEventMapMovesCursor (Config.delKeys config)
          (E.Doc ["Edit", "Where clause", "Delete"]) $
          nextId <$ wiActions ^. Sugar.itemDelete
        , Widget.keysEventMapMovesCursor (Config.whereAddItemKeys config)
          (E.Doc ["Edit", "Where clause", "Add next"]) .
          fmap WidgetIds.fromEntityId $ wiActions ^. Sugar.itemAddNext
        ]
      | otherwise = mempty
  make
    (item ^. Sugar.wiName)
    (item ^. Sugar.wiValue)
    (WidgetIds.fromEntityId (item ^. Sugar.wiEntityId))
    <&> ExpressionGui.egWidget %~ Widget.weakerEvents eventMap

jumpToRHS ::
  (MonadA m, MonadA f) =>
  [E.ModKey] -> (String, ExprGuiM.SugarExpr m) ->
  ExprGuiM f (Widget.EventHandlers (T f))
jumpToRHS keys (rhsDoc, rhs) = do
  savePos <- ExprGuiM.mkPrejumpPosSaver
  return $
    Widget.keysEventMapMovesCursor keys (E.Doc ["Navigation", "Jump to " ++ rhsDoc]) $
      rhsId <$ savePos
  where
    rhsId = WidgetIds.fromEntityId $ rhs ^. Sugar.rPayload . Sugar.plEntityId

makeResultEdit ::
  MonadA m =>
  Maybe (Sugar.BinderActions m) ->
  [Sugar.FuncParam name m] ->
  ExprGuiM.SugarExpr m ->
  Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeResultEdit mActions params result myId = do
  savePos <- ExprGuiM.mkPrejumpPosSaver
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    jumpToLhsEventMap =
      Widget.keysEventMapMovesCursor
      (Config.jumpRHStoLHSKeys config) (E.Doc ["Navigation", "Jump to last param"]) $
      lastParam <$ savePos
    addWhereItemEventMap actions =
      Widget.keysEventMapMovesCursor (Config.whereAddItemKeys config)
      (E.Doc ["Edit", "Where clause", "Add first"]) .
      fmap toEventMapAction $ savePos >> actions ^. Sugar.baAddInnermostWhereItem
  ExprGuiM.makeSubexpression 0 result
    <&> ExpressionGui.egWidget %~
        Widget.weakerEvents
        (jumpToLhsEventMap <> maybe mempty addWhereItemEventMap mActions)
  where
    lastParam =
      params
      <&> WidgetIds.fromEntityId . (^. Sugar.fpId)
      & reverse
      & (^? traverse)
      & fromMaybe myId

makeParamsEdit ::
  MonadA m =>
  ExprGuiM.ShowType ->
  Widget.Id ->
  [Sugar.FuncParam (Name m) m] ->
  ExprGuiM m [ExpressionGui m]
makeParamsEdit showType lhsId params =
  ExpressionGui.listWithDelDests lhsId lhsId (WidgetIds.fromEntityId . (^. Sugar.fpId)) params
  & traverse mkParam
  where
    mkParam (prevId, nextId, param) = ParamEdit.make showType prevId nextId param

diveToNameEdit :: Widget.Id -> Widget.Id
diveToNameEdit = FocusDelegator.delegatingId -- Name editor
