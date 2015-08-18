{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.DefinitionEdit
    ( make
    , diveToNameEdit
    ) where

import           Prelude.Compat

import qualified Control.Lens as Lens
import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import           Graphics.UI.Bottle.WidgetsEnvT (WidgetEnvT)
import           Lamdu.Config (Config)
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Definition as Definition
import           Lamdu.Expr.Scheme (Scheme(..), schemeType)
import           Lamdu.GUI.CodeEdit.Settings (Settings)
import qualified Lamdu.GUI.ExpressionEdit as ExpressionEdit
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import qualified Lamdu.GUI.ExpressionEdit.BuiltinEdit as BuiltinEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..), DefinitionN)
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

toExprGuiMPayload :: ([Sugar.EntityId], NearestHoles) -> ExprGuiT.Payload
toExprGuiMPayload (entityIds, nearestHoles) =
    ExprGuiT.emptyPayload nearestHoles & ExprGuiT.plStoredEntityIds .~ entityIds

make ::
    MonadA m => Anchors.CodeProps m -> Config -> Settings ->
    DefinitionN m ([Sugar.EntityId], NearestHoles) ->
    WidgetEnvT (T m) (Widget (T m))
make cp config settings defS =
    case exprGuiDefS ^. Sugar.drBody of
        Sugar.DefinitionBodyExpression bodyExpr ->
            makeExprDefinition exprGuiDefS bodyExpr
        Sugar.DefinitionBodyBuiltin builtin ->
            makeBuiltinDefinition exprGuiDefS builtin
    & ExprGuiM.run ExpressionEdit.make cp config settings
    <&> (^. ExpressionGui.egWidget)
    where
        exprGuiDefS =
            defS
            <&> Lens.mapped %~ toExprGuiMPayload
            <&> ExprGuiT.markAnnotationsToDisplay

expandTo :: Widget.R -> ExpressionGui m -> ExpressionGui m
expandTo width eg
    | padding <= 0 = eg
    | otherwise = eg & ExpressionGui.pad (Vector2 (padding / 2) 0)
    where
        padding = width - eg ^. ExpressionGui.egWidget . Widget.width

topLevelSchemeTypeView ::
    MonadA m => Widget.R -> Scheme -> Sugar.EntityId -> ExprGuiM m (ExpressionGui m)
topLevelSchemeTypeView width scheme entityId =
    -- At the definition-level, Schemes can be shown as ordinary
    -- types to avoid confusing forall's:
    WidgetIds.fromEntityId entityId
    & Widget.toAnimId
    & ExpressionGui.makeTypeView (scheme ^. schemeType)
    <&> expandTo width

makeBuiltinDefinition ::
    MonadA m =>
    Sugar.Definition (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.DefinitionBuiltin m -> ExprGuiM m (ExpressionGui m)
makeBuiltinDefinition def builtin =
    do
        assignment <-
            [ ExpressionGui.makeNameOriginEdit name (Widget.joinId myId ["name"])
            , ExprGuiM.makeLabel "=" $ Widget.toAnimId myId
            , BuiltinEdit.make builtin myId
            ]
            & sequenceA
            >>= ExprGuiM.widgetEnv . BWidgets.hboxCenteredSpaced
            <&> ExpressionGui.fromValueWidget
        let width = assignment ^. ExpressionGui.egWidget . Widget.width
        typeView <-
            topLevelSchemeTypeView width (builtin ^. Sugar.biType) entityId
        [assignment, typeView]
            & ExpressionGui.vboxTopFocalAlignedTo 0
            & return
    where
        name = def ^. Sugar.drName
        entityId = def ^. Sugar.drEntityId
        myId = WidgetIds.fromEntityId entityId

typeIndicatorId :: Widget.Id -> Widget.Id
typeIndicatorId myId = Widget.joinId myId ["type indicator"]

typeIndicator ::
    MonadA m => Widget.R -> Draw.Color -> Widget.Id -> ExprGuiM m (ExpressionGui m)
typeIndicator width color myId =
    do
        config <- ExprGuiM.readConfig
        let typeIndicatorHeight =
                realToFrac $ Config.typeIndicatorFrameWidth config ^. _2
        Anim.unitSquare (Widget.toAnimId (typeIndicatorId myId))
            & View 1
            & Widget.fromView
            & Widget.scale (Vector2 width typeIndicatorHeight)
            & Widget.tint color
            & ExpressionGui.fromValueWidget
            & return

acceptableTypeIndicator ::
    MonadA m =>
    Widget.R -> T m a -> Draw.Color -> Widget.Id ->
    ExprGuiM m (ExpressionGui m)
acceptableTypeIndicator width accept color myId =
    do
        config <- ExprGuiM.readConfig
        let acceptKeyMap =
                Widget.keysEventMapMovesCursor (Config.acceptDefinitionTypeKeys config)
                (E.Doc ["Edit", "Accept inferred type"]) (accept >> return myId)
        typeIndicator width color myId
            >>= ExpressionGui.egWidget %%~
                ExprGuiM.widgetEnv .
                BWidgets.makeFocusableView (typeIndicatorId myId) .
                Widget.weakerEvents acceptKeyMap

makeExprDefinition ::
    MonadA m =>
    Sugar.Definition (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.DefinitionExpression (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeExprDefinition def bodyExpr =
    do
        config <- ExprGuiM.readConfig
        bodyGui <-
            BinderEdit.make (def ^. Sugar.drName)
            (bodyExpr ^. Sugar.deContent) myId
        let width = bodyGui ^. ExpressionGui.egWidget . Widget.width
        vspace <- ExpressionGui.verticalSpace
        typeGui <-
            case bodyExpr ^. Sugar.deTypeInfo of
            Sugar.DefinitionExportedTypeInfo scheme ->
                typeIndicator width (Config.typeIndicatorMatchColor config) myId :
                [ topLevelSchemeTypeView width scheme entityId
                | Lens.hasn't (Sugar.deContent . Sugar.bParams . Sugar._DefintionWithoutParams) bodyExpr
                ] & sequence
            Sugar.DefinitionNewType (Sugar.AcceptNewType oldScheme _ accept) ->
                case oldScheme of
                Definition.NoExportedType ->
                    [ acceptableTypeIndicator width accept (Config.typeIndicatorFirstTimeColor config) myId
                    ]
                Definition.ExportedType scheme ->
                    [ acceptableTypeIndicator width accept (Config.typeIndicatorErrorColor config) myId
                    , topLevelSchemeTypeView width scheme entityId
                    ]
                & sequence
            <&> ExpressionGui.vboxTopFocalAlignedTo 0 . concatMap (\w -> [vspace, w])
        ExpressionGui.vboxTopFocalAlignedTo 0 [bodyGui, typeGui]
            & return
    where
        entityId = def ^. Sugar.drEntityId
        myId = WidgetIds.fromEntityId entityId

diveToNameEdit :: Widget.Id -> Widget.Id
diveToNameEdit = BinderEdit.diveToNameEdit
