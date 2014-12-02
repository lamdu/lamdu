{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.DefinitionEdit (make, makeNewDefinition) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Store.Transaction (Transaction)
import Data.Traversable (sequenceA)
import Lamdu.Expr.IRef (DefIM)
import Lamdu.GUI.CodeEdit.Settings (Settings)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.GUI.WidgetEnvT (WidgetEnvT)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.Expr.Load as Load
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit as ExpressionEdit
import qualified Lamdu.GUI.ExpressionEdit.BuiltinEdit as BuiltinEdit
import qualified Lamdu.GUI.ExpressionEdit.DefinitionContentEdit as DefinitionContentEdit
import qualified Lamdu.GUI.ExpressionGui.AddNextHoles as AddNextHoles
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.AddNames as AddNames
import qualified Lamdu.Sugar.Convert as SugarConvert
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

make ::
  MonadA m => Anchors.CodeProps m -> Settings ->
  DefIM m -> WidgetEnvT (T m) (WidgetT m)
make cp settings defI = ExprGuiM.run ExpressionEdit.make cp settings $ do
  -- infoMode <- (^. Settings.sInfoMode) <$> ExprGuiM.readSettings
  defS <- ExprGuiM.transaction $ loadConvertDefI cp defI
  case defS ^. Sugar.drBody of
    Sugar.DefinitionBodyExpression bodyExpr ->
      makeExprDefinition defS bodyExpr
    Sugar.DefinitionBodyBuiltin builtin ->
      makeBuiltinDefinition defS builtin

makeBuiltinDefinition ::
  MonadA m =>
  Sugar.Definition Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Sugar.DefinitionBuiltin m -> ExprGuiM m (WidgetT m)
makeBuiltinDefinition def builtin = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  Box.vboxAlign 0 <$> sequenceA
    [ -- defTypeScale config . (^. ExpressionGui.egWidget) <$>
    --   ExprGuiM.makeSubexpression 0 (Sugar.biType builtin)
    -- ,
      BWidgets.hboxCenteredSpaced <$> sequenceA
      [ ExprGuiM.withFgColor (Config.builtinOriginNameColor config) $
        DefinitionContentEdit.makeNameEdit name (Widget.joinId myId ["name"])
      , ExprGuiM.widgetEnv . BWidgets.makeLabel "=" $ Widget.toAnimId myId
      , BuiltinEdit.make builtin myId
      ]
    ]
  where
    name = def ^. Sugar.drName
    myId = WidgetIds.fromEntityId $ def ^. Sugar.drEntityId

-- defTypeScale :: Config -> Widget f -> Widget f
-- defTypeScale config = Widget.scale $ realToFrac <$> Config.defTypeBoxScaleFactor config

makeExprDefinition ::
  MonadA m =>
  Sugar.Definition Sugar.Name m (ExprGuiM.SugarExpr m) ->
  Sugar.DefinitionExpression Sugar.Name m (ExprGuiM.SugarExpr m) ->
  ExprGuiM m (WidgetT m)
makeExprDefinition def bodyExpr = do
  -- config <- ExprGuiM.widgetEnv WE.readConfig
  -- let
    -- makeGrid = (:[]) . defTypeScale config . BWidgets.gridHSpaced
    -- addAcceptanceArrow acceptInferredType label = do
    --   acceptanceLabel <-
    --     (fmap . Widget.weakerEvents)
    --     (Widget.keysEventMapMovesCursor (Config.acceptKeys config)
    --      (E.Doc ["Edit", "Accept inferred type"]) (acceptInferredType >> return myId)) .
    --     ExprGuiM.widgetEnv .
    --     BWidgets.makeFocusableTextView "↱" $ Widget.joinId myId ["accept type"]
    --   return $ BWidgets.hboxCenteredSpaced [acceptanceLabel, label]
    -- labelStyle =
    --   ExprGuiM.localEnv $ WE.setTextSizeColor
    --   (Config.defTypeLabelTextSize config)
    --   (Config.defTypeLabelColor config)
    -- mkTypeRow labelText onLabel typeExpr = do
    --   label <-
    --     onLabel . labelStyle . ExprGuiM.widgetEnv .
    --     BWidgets.makeLabel labelText $ Widget.toAnimId myId
    --   typeGui <- ExprGuiM.makeSubexpression 0 typeExpr
    --   return
    --     [ (right, label)
    --     , (center, Widget.doesntTakeFocus (typeGui ^. ExpressionGui.egWidget))
    --     ]
  -- typeWidgets <-
  --   case bodyExpr ^. Sugar.deTypeInfo of
  --   Sugar.DefinitionExportedTypeInfo x ->
  --     makeGrid <$> sequenceA
  --     [ mkTypeRow "Exported type:" id x ]
  --   Sugar.DefinitionNewType x ->
  --     makeGrid <$> sequenceA
  --     [ mkTypeRow "Exported type:" (>>= addAcceptanceArrow (Sugar.antAccept x)) $
  --       Sugar.antOldType x
  --     , mkTypeRow "Inferred type:" id $ Sugar.antNewType x
  --     ]
  bodyWidget <-
    DefinitionContentEdit.make name (bodyExpr ^. Sugar.deContent) $
    WidgetIds.fromEntityId (def ^. Sugar.drEntityId)
  return . Box.vboxAlign 0 $ -- typeWidgets ++
    [bodyWidget]
  where
    -- right = Vector2 1 0.5
    -- center = 0.5
    -- myId = WidgetIds.fromGuid guid
    name = def ^. Sugar.drName

loadConvertDefI ::
  MonadA m => Anchors.CodeProps m -> DefIM m ->
  T m (Sugar.DefinitionN m ExprGuiM.Payload)
loadConvertDefI cp defI =
  Load.loadDefinitionClosure defI >>=
  SugarConvert.convertDefI cp
  <&> AddNames.addToDef
  <&> Lens.mapped . Lens.mapped . Lens.mapped %~ mkPayload
  <&> AddNextHoles.addToDef
  where
    mkPayload entityIds = ExprGuiM.Payload
      { ExprGuiM._plStoredEntityIds = entityIds
      , ExprGuiM._plInjected = [False]
      -- Filled by AddNextHoles above:
      , ExprGuiM._plHoleEntityIds = ExprGuiM.emptyHoleEntityIds
      }

makeNewDefinition ::
  MonadA m => Anchors.CodeProps m ->
  WidgetEnvT (T m) (T m Widget.Id)
makeNewDefinition cp = do
  curCursor <- WE.readCursor
  return $ do
    newDefI <- DataOps.newPublicDefinition cp ""
    DataOps.newPane cp newDefI
    DataOps.savePreJumpPosition cp curCursor
    return . DefinitionContentEdit.diveToNameEdit $ WidgetIds.fromIRef newDefI
