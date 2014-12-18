{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.DefinitionEdit (make, makeNewDefinition) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Data.Traversable (sequenceA)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Expr.IRef (DefI)
import Lamdu.Expr.Load (loadDef)
import Lamdu.Expr.Scheme (Scheme(..))
import Lamdu.GUI.CodeEdit.Settings (Settings)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.GUI.WidgetEnvT (WidgetEnvT)
import Lamdu.Sugar.AddNames.Types (Name(..), DefinitionN)
import Lamdu.Sugar.RedundantTypes (redundantTypes)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit as ExpressionEdit
import qualified Lamdu.GUI.ExpressionEdit.BuiltinEdit as BuiltinEdit
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import qualified Lamdu.GUI.ExpressionGui.AddNextHoles as AddNextHoles
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.AddNames as AddNames
import qualified Lamdu.Sugar.Convert as SugarConvert
import qualified Lamdu.Sugar.Lens as SugarLens
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

make ::
  MonadA m => Anchors.CodeProps m -> Settings ->
  DefI m -> WidgetEnvT (T m) (WidgetT m)
make cp settings defI = ExprGuiM.run ExpressionEdit.make cp settings $ do
  defS <- ExprGuiM.transaction $ loadConvertDefI cp defI
  case defS ^. Sugar.drBody of
    Sugar.DefinitionBodyExpression bodyExpr ->
      makeExprDefinition defS bodyExpr
    Sugar.DefinitionBodyBuiltin builtin ->
      makeBuiltinDefinition defS builtin

topLevelSchemeTypeView :: MonadA m => Widget.R -> AnimId -> Scheme -> ExprGuiM m (Widget f)
topLevelSchemeTypeView minWidth animId scheme =
  do
    config <- ExprGuiM.widgetEnv WE.readConfig
    -- At the definition-level, Schemes can be shown as ordinary
    -- types to avoid confusing forall's:
    schemeType scheme
      & TypeView.make animId
      <&> ExprGui.addTypeBackground config animId minWidth
      <&> uncurry Widget.liftView
      & ExprGuiM.widgetEnv

makeBuiltinDefinition ::
  MonadA m =>
  Sugar.Definition (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.DefinitionBuiltin m -> ExprGuiM m (WidgetT m)
makeBuiltinDefinition def builtin = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  Box.vboxAlign 0 <$> sequenceA
    [ BWidgets.hboxCenteredSpaced <$> sequenceA
      [ ExprGuiM.withFgColor (Config.builtinOriginNameColor config) $
        BinderEdit.makeNameEdit name (Widget.joinId myId ["name"])
      , ExprGuiM.widgetEnv . BWidgets.makeLabel "=" $ Widget.toAnimId myId
      , BuiltinEdit.make builtin myId
      ]
    , topLevelSchemeTypeView 0 (mappend (Widget.toAnimId myId) ["type"])
      (Sugar.biType builtin)
    ]
  where
    name = def ^. Sugar.drName
    entityId = def ^. Sugar.drEntityId
    myId = WidgetIds.fromEntityId entityId

makeExprDefinition ::
  MonadA m =>
  Sugar.Definition (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.DefinitionExpression (Name m) m (ExprGuiM.SugarExpr m) ->
  ExprGuiM m (WidgetT m)
makeExprDefinition def bodyExpr = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  bodyWidget <-
    BinderEdit.make (def ^. Sugar.drName)
    (bodyExpr ^. Sugar.deContent) myId
  let width = bodyWidget ^. Widget.wSize . Lens._1
  let separatorHeight = realToFrac $ Config.wrapperHoleFrameWidth config ^. Lens._2
  let vspace = BWidgets.vspaceWidget . realToFrac $ Config.valFramePadding config ^. Lens._2
  let
    typeSeparatorId = Widget.joinId myId ["type indicator"]
    typeSeparator =
      Anim.unitSquare (Widget.toAnimId typeSeparatorId)
      & Widget.liftView 1
      & Widget.scale (Vector2 width separatorHeight)
  typeWidget <-
    fmap (Grid.toWidget . Grid.make . map ((:[]) . (,) 0.5)) $
    case bodyExpr ^. Sugar.deTypeInfo of
    Sugar.DefinitionExportedTypeInfo scheme ->
      sequence $
      [ return vspace
      , return $ Widget.tint (Config.typeMatchColor config) typeSeparator
      ] ++
      case bodyExpr ^. Sugar.deContent . Sugar.dParams of
      [] -> []
      _ ->
        [ return vspace
        , topLevelSchemeTypeView width exportedTypeAnimId scheme
        ]
    Sugar.DefinitionNewType (Sugar.AcceptNewType oldScheme _ accept) ->
      sequence $
      return vspace :
      case oldScheme of
      Definition.NoExportedType ->
        [ return vspace
        , typeSeparator
          & Widget.tint (Config.acceptFirstTypeColor config)
          & Widget.weakerEvents acceptKeyMap
          & BWidgets.makeFocusableView typeSeparatorId & ExprGuiM.widgetEnv
        ]
      Definition.ExportedType scheme ->
        [ return vspace
        , typeSeparator
          & Widget.tint (Config.typeErrorColor config)
          & Widget.weakerEvents acceptKeyMap
          & BWidgets.makeFocusableView typeSeparatorId & ExprGuiM.widgetEnv
        , return vspace
        , topLevelSchemeTypeView width exportedTypeAnimId scheme
        ]
      where
        acceptKeyMap =
          Widget.keysEventMapMovesCursor (Config.acceptKeys config)
          (E.Doc ["Edit", "Accept inferred type"]) (accept >> return myId)
  return $ Box.vboxAlign 0 [bodyWidget, typeWidget]
  where
    entityId = def ^. Sugar.drEntityId
    myId = WidgetIds.fromEntityId entityId
    exportedTypeAnimId = mappend (Widget.toAnimId myId) ["Exported"]

loadConvertDefI ::
  MonadA m => Anchors.CodeProps m -> DefI m ->
  T m (DefinitionN m ExprGuiM.Payload)
loadConvertDefI cp defI =
  loadDef defI
  >>= SugarConvert.convertDefI cp
  >>= AddNames.addToDef
  <&> fmap onVal
  <&> AddNextHoles.addToDef
  where
    onVal v =
      v
      & Lens.mapped . Sugar.plData %~ mkPayload
      & redundantTypes . showType .~ ExprGuiM.DoNotShowType
      & SugarLens.holePayloads . showType .~ ExprGuiM.ShowType
      & SugarLens.holeArgs . showType .~ ExprGuiM.ShowType
      & Sugar.rPayload . showType .~ ExprGuiM.ShowType
    showType = Sugar.plData . ExprGuiM.plShowType
    mkPayload entityIds =
      ExprGuiM.emptyPayload
      & ExprGuiM.plStoredEntityIds .~ entityIds

makeNewDefinition ::
  MonadA m => Anchors.CodeProps m ->
  WidgetEnvT (T m) (T m Widget.Id)
makeNewDefinition cp = do
  curCursor <- WE.readCursor
  return $ do
    newDefI <- DataOps.newPublicDefinition cp ""
    DataOps.newPane cp newDefI
    DataOps.savePreJumpPosition cp curCursor
    return . BinderEdit.diveToNameEdit $ WidgetIds.fromIRef newDefI
