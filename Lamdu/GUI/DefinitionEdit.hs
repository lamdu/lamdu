{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.DefinitionEdit
    ( make
    ) where

import           Control.Lens.Operators
import           Control.Lens.Tuple
import qualified Data.List as List
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.View (View(..))
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.Layout as Layout
import           Lamdu.Calc.Type.Scheme (Scheme(..), schemeType)
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import qualified Lamdu.GUI.ExpressionEdit.BuiltinEdit as BuiltinEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..), DefinitionN)
import qualified Lamdu.Sugar.Types as Sugar

import           Prelude.Compat

type T = Transaction

make ::
    Monad m => DefinitionN m ExprGuiT.Payload -> ExprGuiM m (ExpressionGui m)
make exprGuiDefS =
    case exprGuiDefS ^. Sugar.drBody of
    Sugar.DefinitionBodyExpression bodyExpr ->
        makeExprDefinition exprGuiDefS bodyExpr
    Sugar.DefinitionBodyBuiltin builtin ->
        makeBuiltinDefinition exprGuiDefS builtin <&> ExpressionGui.fromValueWidget

expandTo :: Widget.R -> Widget a -> Widget a
expandTo width eg
    | padding <= 0 = eg
    | otherwise = eg & Widget.pad (Vector2 (padding / 2) 0)
    where
        padding = width - eg ^. Widget.width

topLevelSchemeTypeView ::
    Monad m =>
    Scheme -> Sugar.EntityId -> AnimId ->
    ExprGuiM m (Widget.R -> Widget a)
topLevelSchemeTypeView scheme entityId suffix =
    -- At the definition-level, Schemes can be shown as ordinary
    -- types to avoid confusing forall's:
    WidgetIds.fromEntityId entityId
    & (`Widget.joinId` suffix)
    & Widget.toAnimId
    & TypeView.make (scheme ^. schemeType)
    <&> Widget.fromView
    <&> flip expandTo

makeBuiltinDefinition ::
    Monad m =>
    Sugar.Definition (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.DefinitionBuiltin m -> ExprGuiM m (Widget (T m Widget.EventResult))
makeBuiltinDefinition def builtin =
    do
        assignment <-
            [ ExpressionGui.makeNameOriginEdit name (Widget.joinId myId ["name"])
            , ExprGuiM.makeLabel "=" $ Widget.toAnimId myId
            , BuiltinEdit.make builtin myId
            ]
            & sequenceA
            >>= ExprGuiM.widgetEnv . BWidgets.hboxCenteredSpaced
        let width = assignment ^. Widget.width
        typeView <-
            topLevelSchemeTypeView (builtin ^. Sugar.biType) entityId ["builtinType"]
            ?? width
        Box.vboxCentered [assignment, typeView] & return
    where
        name = def ^. Sugar.drName
        entityId = def ^. Sugar.drEntityId
        myId = WidgetIds.fromEntityId entityId

typeIndicatorId :: Widget.Id -> Widget.Id
typeIndicatorId myId = Widget.joinId myId ["type indicator"]

typeIndicator ::
    Monad m =>
    Draw.Color -> Widget.Id -> ExprGuiM m (Widget.R -> Widget a)
typeIndicator color myId =
    ExprGuiM.readConfig
    <&>
    \config width ->
    Anim.unitSquare (Widget.toAnimId (typeIndicatorId myId))
    & View 1
    & Widget.fromView
    & Widget.scale (Vector2 width (realToFrac (Config.typeIndicatorFrameWidth config ^. _2)))
    & Widget.tint color

acceptableTypeIndicator ::
    Monad m =>
    T m a -> Draw.Color -> Widget.Id ->
    ExprGuiM m (Widget.R -> Widget (T m Widget.EventResult))
acceptableTypeIndicator accept color myId =
    do
        config <- ExprGuiM.readConfig
        let acceptKeyMap =
                Widget.keysEventMapMovesCursor (Config.acceptDefinitionTypeKeys config)
                (E.Doc ["Edit", "Accept inferred type"]) (accept >> return myId)
        makeFocusable <-
            BWidgets.makeFocusableView (typeIndicatorId myId)
            & ExprGuiM.widgetEnv
        makeIndicator <- typeIndicator color myId
        return $
            \width ->
            makeIndicator width
            & makeFocusable
            & Widget.weakerEvents acceptKeyMap

makeExprDefinition ::
    Monad m =>
    Sugar.Definition (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.DefinitionExpression (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeExprDefinition def bodyExpr =
    do
        config <- ExprGuiM.readConfig
        bodyGui <-
            BinderEdit.make (def ^. Sugar.drName)
            (bodyExpr ^. Sugar.deContent) myId
        vspace <- ExpressionGui.stdVSpace
        mkTypeWidgets <-
            case bodyExpr ^. Sugar.deTypeInfo of
            Sugar.DefinitionExportedTypeInfo scheme ->
                [ typeIndicator (Config.typeIndicatorMatchColor config) myId
                , topLevelSchemeTypeView scheme entityId ["exportedType"]
                ]
            Sugar.DefinitionNewType (Sugar.AcceptNewType oldMExported newInferred accept) ->
                case oldMExported of
                Definition.NoExportedType ->
                    [ topLevelSchemeTypeView newInferred entityId ["inferredType"]
                    , acceptableTypeIndicator accept (Config.typeIndicatorFirstTimeColor config) myId
                    ]
                Definition.ExportedType oldExported ->
                    [ topLevelSchemeTypeView newInferred entityId ["inferredType"]
                    , acceptableTypeIndicator accept (Config.typeIndicatorErrorColor config) myId
                    , topLevelSchemeTypeView oldExported entityId ["exportedType"]
                    ]
            & sequence <&> sequence
        let f widget =
                widget : mkTypeWidgets (widget ^. Widget.width)
                & List.intersperse vspace
                & Box.vboxCentered
        bodyGui & ExpressionGui.egLayout . Layout.absAlignedWidget . _2 %~ f
            & return
    where
        entityId = def ^. Sugar.drEntityId
        myId = WidgetIds.fromEntityId entityId
