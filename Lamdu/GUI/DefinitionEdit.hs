{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.DefinitionEdit (make, makeNewDefinition) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT)
import Control.MonadA (MonadA)
import Data.Cache (Cache)
import Data.Store.Transaction (Transaction)
import Data.Traversable (sequenceA)
import Data.Typeable (Typeable1)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Config (Config)
import Lamdu.Data.Expr.IRef (DefIM)
import Lamdu.GUI.CodeEdit.Settings (Settings)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, WidgetT)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expr.Load as Load
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.CodeEdit.Settings as Settings
import qualified Lamdu.GUI.ExpressionEdit as ExpressionEdit
import qualified Lamdu.GUI.ExpressionEdit.BuiltinEdit as BuiltinEdit
import qualified Lamdu.GUI.ExpressionEdit.DefinitionContentEdit as DefinitionContentEdit
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.AddNextHoles as AddNextHoles
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.AddNames as AddNames
import qualified Lamdu.Sugar.Convert as SugarConvert
import qualified Lamdu.Sugar.RemoveTypes as SugarRemoveTypes
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction
type CT m = StateT Cache (WE.WidgetEnvT (T m))

make ::
  (Typeable1 m, MonadA m) =>
  Anchors.CodeProps m -> Settings ->
  DefIM m -> CT m (WidgetT m)
make cp settings defI = ExprGuiM.run ExpressionEdit.make cp settings $ do
  infoMode <- (^. Settings.sInfoMode) <$> ExprGuiM.readSettings
  let
    maybeRemoveTypes =
      case infoMode of
      Settings.Types -> id
      _ -> fmap SugarRemoveTypes.nonHoleTypes
  defS <- ExprGuiM.liftMemoT $ maybeRemoveTypes <$> loadConvertDefI cp defI
  case defS ^. Sugar.drBody of
    Sugar.DefinitionBodyExpression bodyExpr ->
      makeExprDefinition defS bodyExpr
    Sugar.DefinitionBodyBuiltin builtin ->
      makeBuiltinDefinition defS builtin

makeBuiltinDefinition ::
  MonadA m =>
  Sugar.Definition Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Sugar.DefinitionBuiltin m (ExprGuiM.SugarExpr m) ->
  ExprGuiM m (WidgetT m)
makeBuiltinDefinition def builtin = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  Box.vboxAlign 0 <$> sequenceA
    [ defTypeScale config . (^. ExpressionGui.egWidget) <$>
      ExprGuiM.makeSubexpression 0 (Sugar.biType builtin)
    , BWidgets.hboxCenteredSpaced <$> sequenceA
      [ ExprGuiM.withFgColor (Config.builtinOriginNameColor config) $
        DefinitionContentEdit.makeNameEdit name (Widget.joinId myId ["name"]) guid
      , ExprGuiM.widgetEnv . BWidgets.makeLabel "=" $ Widget.toAnimId myId
      , BuiltinEdit.make builtin myId
      ]
    ]
  where
    Sugar.Definition guid name _ = def
    myId = WidgetIds.fromGuid guid

defTypeScale :: Config -> Widget f -> Widget f
defTypeScale config = Widget.scale $ realToFrac <$> Config.defTypeBoxScaleFactor config

makeExprDefinition ::
  MonadA m =>
  Sugar.Definition Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Sugar.DefinitionExpression Sugar.Name m (ExprGuiM.SugarExpr m) ->
  ExprGuiM m (WidgetT m)
makeExprDefinition def bodyExpr = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    makeGrid = (:[]) . defTypeScale config . BWidgets.gridHSpaced
    addAcceptanceArrow acceptInferredType label = do
      acceptanceLabel <-
        (fmap . Widget.weakerEvents)
        (Widget.keysEventMapMovesCursor (Config.acceptKeys config)
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
      typeGui <- ExprGuiM.makeSubexpression 0 typeExpr
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
    DefinitionContentEdit.make ExprGuiM.emptyHoleGuids guid name $ bodyExpr ^. Sugar.deContent
  return . Box.vboxAlign 0 $ typeWidgets ++ [bodyWidget]
  where
    right = Vector2 1 0.5
    center = 0.5
    Sugar.Definition guid name _ = def
    myId = WidgetIds.fromGuid guid

loadConvertDefI ::
  (MonadA m, Typeable1 m) =>
  Anchors.CodeProps m -> DefIM m ->
  StateT Cache (T m) (Sugar.DefinitionN m ExprGuiM.Payload)
loadConvertDefI cp defI =
  lift (Load.loadDefinitionClosure defI) >>=
  SugarConvert.convertDefI cp
  <&> AddNames.addToDef
  <&> Lens.mapped . Lens.mapped . Lens.mapped %~ mkPayload
  <&> AddNextHoles.addToDef
  where
    mkPayload guids = ExprGuiM.Payload
      { ExprGuiM._plStoredGuids = guids
      , ExprGuiM._plInjected = [False]
      -- Filled by AddNextHoles above:
      , ExprGuiM._plHoleGuids = ExprGuiM.emptyHoleGuids
      }

makeNewDefinition ::
  MonadA m => Anchors.CodeProps m ->
  CT m (T m Widget.Id)
makeNewDefinition cp = do
  curCursor <- lift WE.readCursor
  return $ do
    newDefI <- DataOps.newPublicDefinition cp ""
    DataOps.newPane cp newDefI
    DataOps.savePreJumpPosition cp curCursor
    return . DefinitionContentEdit.diveToNameEdit $ WidgetIds.fromIRef newDefI
