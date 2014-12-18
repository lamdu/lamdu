{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Lamdu.GUI.ExpressionEdit.BinderEdit (make, diveToNameEdit, makeNameEdit) where

import Control.Applicative ((<$>), (<$))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.List.Utils (nonEmptyAll)
import Data.Monoid (Monoid(..))
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

defFDConfig :: FocusDelegator.Config
defFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.Key'Enter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename definition"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.Key'Escape]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Done renaming"]
  }

makeNameEdit ::
  MonadA m => Name m ->
  Widget.Id -> ExprGuiM m (WidgetT m)
makeNameEdit nameProperty =
  ExprGuiM.wrapDelegated defFDConfig FocusDelegator.NotDelegating id
  (ExpressionGui.makeNameEdit nameProperty)

nonOperatorName :: Name m -> Bool
nonOperatorName (Name NameSourceStored _ _ x) =
  nonEmptyAll (`notElem` operatorChars) x
nonOperatorName _ = False

makeDefNameEdit ::
  MonadA m =>
  Name m -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makeDefNameEdit nameProp myId =
  do
    config <- ExprGuiM.widgetEnv WE.readConfig
    ExprGuiM.withFgColor (Config.defOriginFGColor config) $
      ExpressionGui.fromValueWidget <$> makeNameEdit nameProp myId

makeWheres ::
  MonadA m =>
  [Sugar.WhereItem (Name m) m (ExprGuiM.SugarExpr m)] -> Widget.Id ->
  ExprGuiM m [Widget (T m)]
makeWheres [] _ = return []
makeWheres whereItems myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  whereLabel <-
    (fmap . Widget.scale) (realToFrac <$> Config.whereLabelScaleFactor config) .
    ExprGuiM.widgetEnv . BWidgets.makeLabel "where" $ Widget.toAnimId myId
  itemEdits <- traverse makeWhereItemEdit $ reverse whereItems
  return
    [ BWidgets.hboxSpaced
      [ (0, whereLabel)
      , (0, Widget.scale (realToFrac <$> Config.whereScaleFactor config) $ Box.vboxAlign 0 itemEdits)
      ]
    ]

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
    MonadA m => Transaction.MkProperty m Sugar.PresentationMode ->
    Widget.Id -> ExprGuiM m (Widget (T m))
mkPresentationModeEdit prop myId = do
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

make ::
  MonadA m =>
  Name m ->
  Sugar.Binder (Name m) m (ExprGuiM.SugarExpr m) ->
  Widget.Id ->
  ExprGuiM m (WidgetT m)
make nameProp content myId = do
  equals <- ExprGuiM.widgetEnv . BWidgets.makeLabel "=" $ Widget.toAnimId myId
  rhsJumperEquals <- jumpToRHS [E.ModKey E.noMods E.Key'Equal] rhs
  let
    jumpToRHSViaEquals n widget
      | nonOperatorName n =
        widget
        & Widget.wEventMap %~ E.filterSChars (curry (/= ('=', E.NotShifted)))
        & Widget.weakerEvents rhsJumperEquals
      | otherwise = widget
  paramsEdits <- makeNestedParams jumpToRHSViaEquals rhs myId params
  config <- ExprGuiM.widgetEnv WE.readConfig
  bodyEdit <- makeResultEdit lhs body
  rhsJumper <- jumpToRHS (Config.jumpLHStoRHSKeys config) rhs
  let
    addFirstParamEventMap =
      Widget.keysEventMapMovesCursor (Config.addNextParamKeys config)
      (E.Doc ["Edit", "Add parameter"]) .
      toEventMapAction $ content ^. Sugar.dAddFirstParam
    nameEditEventMap = mappend addFirstParamEventMap rhsJumper
  defNameEdit <-
    makeDefNameEdit nameProp myId
    & Lens.mapped . ExpressionGui.egWidget %~
      Widget.weakerEvents nameEditEventMap . jumpToRHSViaEquals nameProp
  savePos <- ExprGuiM.mkPrejumpPosSaver
  presentationEdits <-
      case content ^. Sugar.dSetPresentationMode of
      Nothing -> return []
      Just setPresentationMode ->
        (: []) <$>
        mkPresentationModeEdit setPresentationMode presentationChoiceId
  let
    addWhereItemEventMap =
      Widget.keysEventMapMovesCursor (Config.addWhereItemKeys config)
      (E.Doc ["Edit", "Add where item"]) .
      toEventMapAction $ savePos >> content ^. Sugar.dAddInnermostWhereItem
    assignment =
      ExpressionGui.hboxSpaced $
      ExpressionGui.addBelow 0 (map ((,) 0) presentationEdits)
      defNameEdit :
      paramsEdits ++
      [ ExpressionGui.fromValueWidget equals
      , bodyEdit
        & ExpressionGui.egWidget %~
          Widget.weakerEvents addWhereItemEventMap
      ]
  wheres <- makeWheres (content ^. Sugar.dWhereItems) myId
  return . Box.vboxAlign 0 $ assignment ^. ExpressionGui.egWidget : wheres
  where
    presentationChoiceId = Widget.joinId myId ["presentation"]
    lhs = myId : map (WidgetIds.fromEntityId . (^. Sugar.fpId)) params
    rhs = ("Def Body", body)
    params = content ^. Sugar.dParams
    body = content ^. Sugar.dBody
    toEventMapAction =
      fmap (FocusDelegator.delegatingId . WidgetIds.fromEntityId)

makeWhereItemEdit ::
  MonadA m =>
  Sugar.WhereItem (Name m) m (ExprGuiM.SugarExpr m) ->
  ExprGuiM m (WidgetT m)
makeWhereItemEdit item = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    eventMap
      | Just wiActions <- item ^. Sugar.wiActions =
        mconcat
        [ Widget.keysEventMapMovesCursor (Config.delKeys config)
          (E.Doc ["Edit", "Where item", "Delete"]) .
          fmap WidgetIds.fromEntityId $ wiActions ^. Sugar.itemDelete
        , Widget.keysEventMapMovesCursor (Config.addWhereItemKeys config)
          (E.Doc ["Edit", "Where item", "Add"]) .
          fmap WidgetIds.fromEntityId $ wiActions ^. Sugar.itemAddNext
        ]
      | otherwise = mempty
  Widget.weakerEvents eventMap <$>
    make
    (item ^. Sugar.wiName)
    (item ^. Sugar.wiValue)
    (WidgetIds.fromEntityId (item ^. Sugar.wiEntityId))

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

makeResultEdit
  :: MonadA m
  => [Widget.Id]
  -> ExprGuiM.SugarExpr m
  -> ExprGuiM m (ExpressionGui m)
makeResultEdit lhs result = do
  savePos <- ExprGuiM.mkPrejumpPosSaver
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    jumpToLhsEventMap =
      Widget.keysEventMapMovesCursor
      (Config.jumpRHStoLHSKeys config) (E.Doc ["Navigation", "Jump to last param"]) $
      lastParam <$ savePos
  ExprGuiM.makeSubexpression 0 result
    & Lens.mapped . ExpressionGui.egWidget %~
      Widget.weakerEvents jumpToLhsEventMap
  where
    lastParam = case lhs of
      [] -> error "makeResultEdit given empty LHS"
      xs -> last xs

addPrevIds ::
  Widget.Id ->
  [Sugar.FuncParam name m] ->
  [(Widget.Id, Sugar.FuncParam name m)]
addPrevIds = go
  where
    fpId param = WidgetIds.fromEntityId $ param ^. Sugar.fpId
    go _      [] = []
    go prevId (fp:fps) = (prevId, fp) : go (fpId fp) fps

makeNestedParams ::
  MonadA m =>
  (Name m -> Widget (T m) -> Widget (T m)) ->
  (String, ExprGuiM.SugarExpr m) ->
  Widget.Id ->
  [Sugar.FuncParam (Name m) m] ->
  ExprGuiM m [ExpressionGui m]
makeNestedParams atParamWidgets rhs lhsId params = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  rhsJumper <- jumpToRHS (Config.jumpLHStoRHSKeys config) rhs
  let
    mkParam (prevId, param) =
      (ExpressionGui.egWidget %~
       (atParamWidgets (param ^. Sugar.fpName) .
        Widget.weakerEvents rhsJumper)) <$>
      ParamEdit.make ExprGuiM.ShowType prevId param
  traverse mkParam $ addPrevIds lhsId params

diveToNameEdit :: Widget.Id -> Widget.Id
diveToNameEdit = FocusDelegator.delegatingId -- Name editor
