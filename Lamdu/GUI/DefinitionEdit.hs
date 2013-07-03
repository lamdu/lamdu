{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.DefinitionEdit (make, DefinitionContentEdit.diveToNameEdit) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Traversable (sequenceA)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Config (Config)
import Lamdu.GUI.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.BuiltinEdit as BuiltinEdit
import qualified Lamdu.GUI.ExpressionEdit.DefinitionContentEdit as DefinitionContentEdit
import qualified Lamdu.GUI.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

make ::
  MonadA m =>
  Sugar.Definition Sugar.Name m (ExprGuiM.SugarExpr m) ->
  ExprGuiM m (WidgetT m)
make def =
  case def ^. Sugar.drBody of
  Sugar.DefinitionBodyExpression bodyExpr ->
    makeExprDefinition def bodyExpr
  Sugar.DefinitionBodyBuiltin builtin ->
    makeBuiltinDefinition def builtin

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
    DefinitionContentEdit.make guid name $ bodyExpr ^. Sugar.deContent
  return . Box.vboxAlign 0 $ typeWidgets ++ [bodyWidget]
  where
    right = Vector2 1 0.5
    center = 0.5
    Sugar.Definition guid name _ = def
    myId = WidgetIds.fromGuid guid
