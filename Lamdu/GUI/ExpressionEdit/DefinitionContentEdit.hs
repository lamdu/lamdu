{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Lamdu.GUI.ExpressionEdit.DefinitionContentEdit (make, diveToNameEdit, makeNameEdit) where

import Control.Applicative ((<$>), (<*>), (<$))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.List.Utils (nonEmptyAll, isLengthAtLeast)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CharClassification (operatorChars)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionGui (ExpressionGui, Collapser(..))
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, WidgetT)
import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

defFDConfig :: FocusDelegator.Config
defFDConfig = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
  , FocusDelegator.startDelegatingDoc = E.Doc ["Edit", "Rename definition"]
  , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.KeyEsc]
  , FocusDelegator.stopDelegatingDoc = E.Doc ["Edit", "Done renaming"]
  }

makeNameEdit ::
  MonadA m => Sugar.Name ->
  Widget.Id -> Guid -> ExprGuiM m (WidgetT m)
makeNameEdit name myId ident =
  ExprGuiM.wrapDelegated defFDConfig FocusDelegator.NotDelegating id
  (ExpressionGui.makeNameEdit name ident)
  myId

nonOperatorName :: Sugar.Name -> Bool
nonOperatorName (Sugar.Name Sugar.StoredName _ x) =
  nonEmptyAll (`notElem` operatorChars) x
nonOperatorName _ = False

polyNameFDConfig :: Config -> FocusDelegator.Config
polyNameFDConfig config = FocusDelegator.Config
  { FocusDelegator.startDelegatingKeys = Config.collapsedExpandKeys config
  , FocusDelegator.startDelegatingDoc = E.Doc ["View", "Expand polymorphic"]
  , FocusDelegator.stopDelegatingKeys = Config.collapsedCollapseKeys config
  , FocusDelegator.stopDelegatingDoc = E.Doc ["View", "Collapse polymorphic"]
  }

makePolyNameEdit ::
  MonadA m =>
  Sugar.Name -> Guid -> [ExpressionGui m] -> Widget.Id ->
  ExprGuiM m (ExpressionGui m)
makePolyNameEdit name guid depParamsEdits myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    f wId =
      Collapser
      { cMakeExpanded =
        ExpressionGui.withBgColor
        (Config.layerCollapsedExpandedBG (Config.layers config))
        (Config.collapsedExpandedBGColor config) bgId .
        ExpressionGui.hboxSpaced . (: depParamsEdits) <$>
        nameGui (Config.monomorphicDefOriginForegroundColor config)
      , cMakeFocusedCompact =
        nameGui $ Config.polymorphicDefOriginForegroundColor config
      }
      where
        nameGui color = makeNameGui color wId
        bgId = Widget.toAnimId wId ++ ["bg"]
  case depParamsEdits of
    [] -> makeNameGui (Config.monomorphicDefOriginForegroundColor config) myId
    _ -> ExpressionGui.makeCollapser (polyNameFDConfig config) f myId
  where
    makeNameGui color wId =
      ExprGuiM.withFgColor color $
      ExpressionGui.fromValueWidget <$> makeNameEdit name wId guid

makeWheres ::
  MonadA m =>
  [Sugar.WhereItem Sugar.Name m (ExprGuiM.SugarExpr m)] -> Widget.Id ->
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
    { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
    , FocusDelegator.startDelegatingDoc = E.Doc ["Presentation Mode", "Select"]
    , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
    , FocusDelegator.stopDelegatingDoc = E.Doc ["Presentation Mode", "Choose selected"]
    }
  , BWidgets.cwcOrientation = Box.vertical
  , BWidgets.cwcExpandMode = BWidgets.ExplicitEntry
  , BWidgets.cwcBgLayer = Config.layerChoiceBG $ Config.layers config
  }

mkPresentationEdits :: MonadA m => Guid -> Widget.Id -> ExprGuiM m (Widget (T m))
mkPresentationEdits guid myId = do
  cur <- ExprGuiM.transaction $ Transaction.getP mkProp
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
    BWidgets.makeChoiceWidget (Transaction.setP mkProp) pairs cur
    (presentationModeChoiceConfig config) myId
  where
    mkProp = Anchors.assocPresentationMode guid

make ::
  MonadA m => Guid -> Sugar.Name ->
  Sugar.DefinitionContent Sugar.Name m (ExprGuiM.SugarExpr m) ->
  ExprGuiM m (WidgetT m)
make guid name content = do
  equals <- ExprGuiM.widgetEnv . BWidgets.makeLabel "=" $ Widget.toAnimId myId
  rhsJumperEquals <- jumpToRHS [E.ModKey E.noMods (E.charKey '=')] rhs
  let
    jumpToRHSViaEquals n widget
      | nonOperatorName n =
        widget
        & Widget.wEventMap %~ E.filterSChars (curry (/= ('=', E.NotShifted)))
        & Widget.weakerEvents rhsJumperEquals
      | otherwise = widget
  (depParamsEdits, paramsEdits) <-
    makeNestedParams jumpToRHSViaEquals rhs myId depParams params
  config <- ExprGuiM.widgetEnv WE.readConfig
  bodyEdit <- makeResultEdit lhs body
  rhsJumper <- jumpToRHS (Config.jumpLHStoRHSKeys config) rhs
  let
    addFirstParamEventMap =
      Widget.keysEventMapMovesCursor (Config.addNextParamKeys config)
      (E.Doc ["Edit", "Add parameter"]) .
      toEventMapAction $ content ^. Sugar.dAddFirstParam
    nameEditEventMap = mappend addFirstParamEventMap rhsJumper
  polyNameEdit <-
    makePolyNameEdit name guid depParamsEdits myId
    & Lens.mapped . ExpressionGui.egWidget %~
      Widget.weakerEvents nameEditEventMap . jumpToRHSViaEquals name
  savePos <- ExprGuiM.mkPrejumpPosSaver
  presentationEdits <-
    if isLengthAtLeast 2 params
    then (: []) <$> mkPresentationEdits guid presentationChoiceId
    else return []
  let
    addWhereItemEventMap =
      Widget.keysEventMapMovesCursor (Config.addWhereItemKeys config)
      (E.Doc ["Edit", "Add where item"]) .
      toEventMapAction $ savePos >> content ^. Sugar.dAddInnermostWhereItem
    assignment =
      ExpressionGui.hboxSpaced $
      ExpressionGui.addBelow 0 (map ((,) 0) presentationEdits)
      polyNameEdit :
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
    lhs = myId : map (WidgetIds.fromGuid . (^. Sugar.fpId)) allParams
    rhs = ("Def Body", body)
    allParams = depParams ++ params
    depParams = content ^. Sugar.dDepParams
    params = content ^. Sugar.dParams
    body = content ^. Sugar.dBody
    toEventMapAction =
      fmap (FocusDelegator.delegatingId . WidgetIds.fromGuid)
    myId = WidgetIds.fromGuid guid

makeWhereItemEdit ::
  MonadA m =>
  Sugar.WhereItem Sugar.Name m (ExprGuiM.SugarExpr m) ->
  ExprGuiM m (WidgetT m)
makeWhereItemEdit item = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    eventMap
      | Just wiActions <- item ^. Sugar.wiActions =
        mconcat
        [ Widget.keysEventMapMovesCursor (Config.delKeys config)
          (E.Doc ["Edit", "Where item", "Delete"]) .
          fmap WidgetIds.fromGuid $ wiActions ^. Sugar.itemDelete
        , Widget.keysEventMapMovesCursor (Config.addWhereItemKeys config)
          (E.Doc ["Edit", "Where item", "Add"]) .
          fmap WidgetIds.fromGuid $ wiActions ^. Sugar.itemAddNext
        ]
      | otherwise = mempty
  Widget.weakerEvents eventMap <$>
    make
    (item ^. Sugar.wiGuid)
    (item ^. Sugar.wiName)
    (item ^. Sugar.wiValue)

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
    rhsId = WidgetIds.fromGuid $ rhs ^. Sugar.rPayload . Sugar.plGuid

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
  [Sugar.FuncParam name m expr] ->
  [Sugar.FuncParam name m expr] ->
  ( [(Widget.Id, Sugar.FuncParam name m expr)]
  , [(Widget.Id, Sugar.FuncParam name m expr)]
  )
addPrevIds lhsId depParams params =
  (depParamIds, paramIds)
  where
    depParamIds = go lhsId depParams
    paramIds = go lhsId params
    fpId param = WidgetIds.fromGuid $ param ^. Sugar.fpId
    go _ [] = []
    go i (fp:fps) = (i, fp) : go (fpId fp) fps

makeNestedParams ::
  MonadA m =>
  (Sugar.Name -> Widget (T m) -> Widget (T m))
 -> (String, ExprGuiM.SugarExpr m)
 -> Widget.Id
 -> [Sugar.FuncParam Sugar.Name m (ExprGuiM.SugarExpr m)]
 -> [Sugar.FuncParam Sugar.Name m (ExprGuiM.SugarExpr m)]
 -> ExprGuiM m ([ExpressionGui m], [ExpressionGui m])
makeNestedParams atParamWidgets rhs lhsId depParams params = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  rhsJumper <- jumpToRHS (Config.jumpLHStoRHSKeys config) rhs
  let
    (depParamIds, paramIds) = addPrevIds lhsId depParams params
    mkParam (prevId, param) =
      (ExpressionGui.egWidget %~
       (atParamWidgets (param ^. Sugar.fpName) .
        Widget.weakerEvents rhsJumper)) <$>
      LambdaEdit.makeParamEdit prevId param
  (,)
    <$> traverse mkParam depParamIds
    <*> traverse mkParam paramIds

diveToNameEdit :: Widget.Id -> Widget.Id
diveToNameEdit =
  -- If we delegate too deep (e.g: No polymorphic params) that's
  -- handled OK. So we may as well assume we're always wrapped by a
  -- polymorphic wrapper:
  FocusDelegator.delegatingId . -- Collapsed wrapper
  FocusDelegator.delegatingId -- Name editor
