{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.DefinitionEdit
  ( make
  , diveToNameEdit
  ) where

import Control.Applicative ((<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Data.Traversable (sequenceA)
import Data.Vector.Vector2 (Vector2(..))
import Graphics.UI.Bottle.Animation (AnimId)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.Expr.Scheme (Scheme(..))
import Lamdu.GUI.CodeEdit.Settings (Settings)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.GUI.WidgetEnvT (WidgetEnvT)
import Lamdu.Sugar.AddNames.Types (Name(..), DefinitionN)
import Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Control.Lens as Lens
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit as ExpressionEdit
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import qualified Lamdu.GUI.ExpressionEdit.BuiltinEdit as BuiltinEdit
import qualified Lamdu.GUI.ExpressionGui as ExprGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

toExprGuiMPayload :: ([Sugar.EntityId], NearestHoles) -> ExprGuiM.Payload
toExprGuiMPayload (entityIds, nearestHoles) =
  ExprGuiM.emptyPayload nearestHoles & ExprGuiM.plStoredEntityIds .~ entityIds

make ::
  MonadA m => Anchors.CodeProps m -> Settings ->
  DefinitionN m ([Sugar.EntityId], NearestHoles) ->
  WidgetEnvT (T m) (WidgetT m)
make cp settings defS =
  ExprGuiM.run ExpressionEdit.make cp settings $
  case exprGuiDefS ^. Sugar.drBody of
    Sugar.DefinitionBodyExpression bodyExpr ->
      makeExprDefinition exprGuiDefS bodyExpr
    Sugar.DefinitionBodyBuiltin builtin ->
      makeBuiltinDefinition exprGuiDefS builtin
  where
    exprGuiDefS =
      defS
      <&> Lens.mapped . Sugar.plData %~ toExprGuiMPayload
      <&> ExprGuiM.markRedundantTypes

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
makeBuiltinDefinition def builtin =
  Box.vboxAlign 0 <$> sequenceA
  [ BWidgets.hboxCenteredSpaced <$> sequenceA
    [ ExprGui.makeNameOriginEdit name (Widget.joinId myId ["name"])
    , ExprGuiM.makeLabel "=" $ Widget.toAnimId myId
    , BuiltinEdit.make builtin myId
    ]
  , topLevelSchemeTypeView 0 (mappend (Widget.toAnimId myId) ["type"])
    (Sugar.biType builtin)
  ]
  where
    name = def ^. Sugar.drName
    entityId = def ^. Sugar.drEntityId
    myId = WidgetIds.fromEntityId entityId

typeIndicatorId :: Widget.Id -> Widget.Id
typeIndicatorId myId = Widget.joinId myId ["type indicator"]

typeIndicator ::
  MonadA m => Widget.R -> Draw.Color -> Widget.Id -> ExprGuiM m (Widget f)
typeIndicator width color myId =
  do
    config <- ExprGuiM.widgetEnv WE.readConfig
    let
      typeIndicatorHeight =
        realToFrac $ Config.typeIndicatorFrameWidth config ^. Lens._2
    Anim.unitSquare (Widget.toAnimId (typeIndicatorId myId))
      & Widget.liftView 1
      & Widget.scale (Vector2 width typeIndicatorHeight)
      & Widget.tint color
      & return

acceptableTypeIndicator ::
  (MonadA m, MonadA f) =>
  Widget.R -> f a -> Draw.Color -> Widget.Id ->
  ExprGuiM m (Widget f)
acceptableTypeIndicator width accept color myId =
  do
    config <- ExprGuiM.widgetEnv WE.readConfig
    let
      acceptKeyMap =
        Widget.keysEventMapMovesCursor (Config.acceptDefinitionTypeKeys config)
        (E.Doc ["Edit", "Accept inferred type"]) (accept >> return myId)
    typeIndicator width color myId
      <&> Widget.weakerEvents acceptKeyMap
      >>= ExprGuiM.widgetEnv . BWidgets.makeFocusableView (typeIndicatorId myId)

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
    <&> (^. ExprGui.egWidget)
  let width = bodyWidget ^. Widget.wSize . Lens._1
  vspace <- BWidgets.verticalSpace & ExprGuiM.widgetEnv
  typeWidget <-
    fmap (Box.vboxAlign 0.5 . concatMap (\w -> [vspace, w])) $
    case bodyExpr ^. Sugar.deTypeInfo of
    Sugar.DefinitionExportedTypeInfo scheme ->
      sequence $
      typeIndicator width (Config.typeIndicatorMatchColor config) myId :
      [ topLevelSchemeTypeView width exportedTypeAnimId scheme
      | not $ null $ bodyExpr ^. Sugar.deContent . Sugar.dParams
      ]
    Sugar.DefinitionNewType (Sugar.AcceptNewType oldScheme _ accept) ->
      sequence $
      case oldScheme of
      Definition.NoExportedType ->
        [ acceptableTypeIndicator width accept (Config.acceptDefinitionTypeForFirstTimeColor config) myId
        ]
      Definition.ExportedType scheme ->
        [ acceptableTypeIndicator width accept (Config.typeIndicatorErrorColor config) myId
        , topLevelSchemeTypeView width exportedTypeAnimId scheme
        ]
  return $ Box.vboxAlign 0 [bodyWidget, typeWidget]
  where
    entityId = def ^. Sugar.drEntityId
    myId = WidgetIds.fromEntityId entityId
    exportedTypeAnimId = mappend (Widget.toAnimId myId) ["Exported"]

diveToNameEdit :: Widget.Id -> Widget.Id
diveToNameEdit = BinderEdit.diveToNameEdit
