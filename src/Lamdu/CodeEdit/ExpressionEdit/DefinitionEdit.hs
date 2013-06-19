{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Lamdu.CodeEdit.ExpressionEdit.DefinitionEdit (make, diveToNameEdit) where

import Control.Applicative ((<$>), (<*>), (<$))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.List.Utils (nonEmptyAll, isLengthAtLeast)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse, sequenceA)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CharClassification (operatorChars)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui, Collapser(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.Config (Config)
import qualified Control.Lens as Lens
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.BuiltinEdit as BuiltinEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

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

makeEquals :: MonadA m => Widget.Id -> ExprGuiM m (Widget f)
makeEquals = ExprGuiM.widgetEnv . BWidgets.makeLabel "=" . Widget.toAnimId

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
        ExpressionGui.withBgColor Layers.collapsedExpandedBG
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
  [Sugar.WhereItem Sugar.Name m (Sugar.ExpressionN m ())] -> Widget.Id ->
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

presentationModeChoiceConfig :: BWidgets.ChoiceWidgetConfig
presentationModeChoiceConfig = BWidgets.ChoiceWidgetConfig
  { BWidgets.cwcFDConfig = FocusDelegator.Config
    { FocusDelegator.startDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
    , FocusDelegator.startDelegatingDoc = E.Doc ["Presentation Mode", "Select"]
    , FocusDelegator.stopDelegatingKeys = [E.ModKey E.noMods E.KeyEnter]
    , FocusDelegator.stopDelegatingDoc = E.Doc ["Presentation Mode", "Choose selected"]
    }
  , BWidgets.cwcOrientation = Box.vertical
  , BWidgets.cwcExpandMode = BWidgets.ExplicitEntry
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
    presentationModeChoiceConfig myId
  where
    mkProp = Anchors.assocPresentationMode guid

makeDefContentEdit ::
  MonadA m => Guid -> Sugar.Name ->
  Sugar.DefinitionContent Sugar.Name m (Sugar.ExpressionN m ()) ->
  ExprGuiM m (WidgetT m)
makeDefContentEdit guid name content = do
  equals <- makeEquals myId
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

make ::
  MonadA m =>
  Sugar.Definition Sugar.Name m (Sugar.ExpressionN m ()) ->
  ExprGuiM m (WidgetT m)
make def =
  case def ^. Sugar.drBody of
  Sugar.DefinitionBodyExpression bodyExpr ->
    makeExprDefinition def bodyExpr
  Sugar.DefinitionBodyBuiltin builtin ->
    makeBuiltinDefinition def builtin

makeBuiltinDefinition ::
  MonadA m =>
  Sugar.Definition Sugar.Name m (Sugar.ExpressionN m ()) ->
  Sugar.DefinitionBuiltin m (Sugar.ExpressionN m ()) ->
  ExprGuiM m (WidgetT m)
makeBuiltinDefinition def builtin = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  Box.vboxAlign 0 <$> sequenceA
    [ defTypeScale config . (^. ExpressionGui.egWidget) <$>
      ExprGuiM.makeSubexpresion 0 (Sugar.biType builtin)
    , BWidgets.hboxCenteredSpaced <$> sequenceA
      [ ExprGuiM.withFgColor (Config.builtinOriginNameColor config) $
        makeNameEdit name (Widget.joinId myId ["name"]) guid
      , makeEquals myId
      , BuiltinEdit.make builtin myId
      ]
    ]
  where
    Sugar.Definition guid name _ = def
    myId = WidgetIds.fromGuid guid

defTypeScale :: Config -> Widget f -> Widget f
defTypeScale config = Widget.scale $ realToFrac <$> Config.defTypeBoxScaleFactor config

makeWhereItemEdit ::
  MonadA m =>
  Sugar.WhereItem Sugar.Name m (Sugar.ExpressionN m ()) ->
  ExprGuiM m (WidgetT m)
makeWhereItemEdit item = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    eventMap
      | Just wiActions <- Sugar.wiActions item =
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
    makeDefContentEdit (Sugar.wiGuid item) (Sugar.wiName item) (Sugar.wiValue item)

makeExprDefinition ::
  MonadA m =>
  Sugar.Definition Sugar.Name m (Sugar.ExpressionN m ()) ->
  Sugar.DefinitionExpression Sugar.Name m (Sugar.ExpressionN m ()) ->
  ExprGuiM m (WidgetT m)
makeExprDefinition def bodyExpr = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    makeGrid = (:[]) . defTypeScale config . BWidgets.gridHSpaced
    addAcceptanceArrow acceptInferredType label = do
      acceptanceLabel <-
        (fmap . Widget.weakerEvents)
        (Widget.keysEventMapMovesCursor (Config.acceptInferredTypeKeys config)
         (E.Doc ["Edit", "Accept inferred type"]) (acceptInferredType >> return myId)) .
        ExprGuiM.widgetEnv .
        BWidgets.makeFocusableTextView "↱" $ Widget.joinId myId ["accept type"]
      return $ BWidgets.hboxCenteredSpaced [acceptanceLabel, label]
    labelStyle =
      ExprGuiM.localEnv $ WE.setTextSizeColor
      (Config.defTypeLabelTextSize config)
      (Config.defTypeLabelColor config)
    mkTypeRow labelText onLabel typeExpr = do
      label <-
        onLabel . labelStyle . ExprGuiM.widgetEnv .
        BWidgets.makeLabel labelText $ Widget.toAnimId myId
      typeGui <- ExprGuiM.makeSubexpresion 0 typeExpr
      return
        [ (right, label)
        , (center, Widget.doesntTakeFocus (typeGui ^. ExpressionGui.egWidget))
        ]
  typeWidgets <-
    case bodyExpr ^. Sugar.deTypeInfo of
    Sugar.DefinitionExportedTypeInfo x ->
      makeGrid <$> sequenceA
      [ mkTypeRow "Exported type:" id x ]
    Sugar.DefinitionIncompleteType x ->
      makeGrid <$> sequenceA
      [ mkTypeRow "Exported type:" id $ Sugar.sitOldType x
      , mkTypeRow "Inferred type:" id $ Sugar.sitNewIncompleteType x
      ]
    Sugar.DefinitionNewType x ->
      makeGrid <$> sequenceA
      [ mkTypeRow "Exported type:" (>>= addAcceptanceArrow (Sugar.antAccept x)) $
        Sugar.antOldType x
      , mkTypeRow "Inferred type:" id $ Sugar.antNewType x
      ]
  bodyWidget <-
    makeDefContentEdit guid name $ bodyExpr ^. Sugar.deContent
  return . Box.vboxAlign 0 $ typeWidgets ++ [bodyWidget]
  where
    right = Vector2 1 0.5
    center = 0.5
    Sugar.Definition guid name _ = def
    myId = WidgetIds.fromGuid guid

diveToNameEdit :: Widget.Id -> Widget.Id
diveToNameEdit =
  -- If we delegate too deep (e.g: No polymorphic params) that's
  -- handled OK. So we may as well assume we're always wrapped by a
  -- polymorphic wrapper:
  FocusDelegator.delegatingId . -- Collapsed wrapper
  FocusDelegator.delegatingId -- Name editor

jumpToRHS ::
  (MonadA m, MonadA f) =>
  [E.ModKey] -> (String, Sugar.ExpressionN m ()) ->
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
  -> Sugar.ExpressionN m ()
  -> ExprGuiM m (ExpressionGui m)
makeResultEdit lhs result = do
  savePos <- ExprGuiM.mkPrejumpPosSaver
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    jumpToLhsEventMap =
      Widget.keysEventMapMovesCursor
      (Config.jumpRHStoLHSKeys config) (E.Doc ["Navigation", "Jump to last param"]) $
      lastParam <$ savePos
  ExprGuiM.makeSubexpresion 0 result
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
addPrevIds firstParId depParams params =
  (depParamIds, paramIds)
  where
    (lastDepParamId, depParamIds) = go firstParId depParams
    (_, paramIds) = go lastDepParamId params
    fpId param = WidgetIds.fromGuid $ param ^. Sugar.fpId
    go i [] = (i, [])
    go i (fp:fps) = Lens._2 %~ ((i, fp) :) $ go (fpId fp) fps

makeNestedParams ::
  MonadA m =>
  (Sugar.Name -> Widget (T m) -> Widget (T m))
 -> (String, Sugar.ExpressionN m ())
 -> Widget.Id
 -> [Sugar.FuncParam Sugar.Name m (Sugar.ExpressionN m ())]
 -> [Sugar.FuncParam Sugar.Name m (Sugar.ExpressionN m ())]
 -> ExprGuiM m ([ExpressionGui m], [ExpressionGui m])
makeNestedParams atParamWidgets rhs firstParId depParams params = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  rhsJumper <- jumpToRHS (Config.jumpLHStoRHSKeys config) rhs
  let
    (depParamIds, paramIds) = addPrevIds firstParId depParams params
    mkParam (prevId, param) =
      (ExpressionGui.egWidget %~
       (atParamWidgets (param ^. Sugar.fpName) .
        Widget.weakerEvents rhsJumper)) <$>
      LambdaEdit.makeParamEdit prevId param
  (,)
    <$> traverse mkParam depParamIds
    <*> traverse mkParam paramIds
