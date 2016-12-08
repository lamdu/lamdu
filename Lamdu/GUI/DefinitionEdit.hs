{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.DefinitionEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.DrawingCombinators as Draw
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.ModKey (ModKey(..))
import           Graphics.UI.Bottle.View (View(..))
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.Aligned as AlignedWidget
import qualified Graphics.UI.Bottle.Widgets as BWidgets
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.Calc.Type.Scheme (Scheme(..), schemeType)
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import qualified Lamdu.GUI.ExpressionEdit.BuiltinEdit as BuiltinEdit
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..), DefinitionN)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

addUndeleteButton ::
    Monad m => Widget.Id -> T m Widget.Id ->
    (Widget.R -> Widget (T m Widget.EventResult)) ->
    ExprGuiM m (Widget.R -> Widget (T m Widget.EventResult))
addUndeleteButton myId undelete mkWidget =
    do
        undelButton <-
            BWidgets.makeFocusableTextView "Undelete..." undelButtonId
            & ExprGuiM.widgetEnv
            <&> Widget.weakerEvents eventMap
        return $ \width -> Box.vboxAlign 0 [mkWidget width, undelButton]
    where
        eventMap =
            Widget.keysEventMapMovesCursor [ModKey mempty GLFW.Key'Enter]
            (E.Doc ["Edit", "Undelete definition"]) undelete
        undelButtonId = Widget.joinId myId ["Undelete"]

make ::
    Monad m =>
    DefinitionN m ExprGuiT.Payload ->
    ExprGuiM m (Widget.R -> Widget (T m Widget.EventResult))
make def =
    do
        defStateProp <-
            def ^. Sugar.drDefinitionState . Transaction.mkProperty
            & ExprGuiM.transaction
        let defState = Property.value defStateProp
        let mUndelete =
                case defState of
                Sugar.LiveDefinition -> Nothing
                Sugar.DeletedDefinition ->
                    myId <$ Property.set defStateProp Sugar.LiveDefinition & Just
        case def ^. Sugar.drBody of
            Sugar.DefinitionBodyExpression bodyExpr ->
                makeExprDefinition def bodyExpr
            Sugar.DefinitionBodyBuiltin builtin ->
                makeBuiltinDefinition def builtin <&> const
            <&> Lens.mapped . Widget.view %~
                ExpressionGui.deletionDiagonal 0.02 (Widget.toAnimId myId) defState
            >>= maybe return (addUndeleteButton myId) mUndelete
    where
        myId = def ^. Sugar.drEntityId & WidgetIds.fromEntityId

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
            , ExprGuiM.makeLabel "=" (Widget.toAnimId myId) <&> Widget.fromView
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
    Monad m => Draw.Color -> Widget.Id -> ExprGuiM m (Widget.R -> View)
typeIndicator color myId =
    ExprGuiM.readConfig
    <&>
    \config width ->
    Anim.unitSquare (Widget.toAnimId (typeIndicatorId myId))
    & View.make 1
    & View.scale (Vector2 width (realToFrac (Config.typeIndicatorFrameWidth config ^. _2)))
    & View.tint color

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
            & Widget.fromView
            & makeFocusable
            & Widget.weakerEvents acceptKeyMap

makeExprDefinition ::
    Monad m =>
    Sugar.Definition (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.DefinitionExpression (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (Widget.R -> Widget (T m Widget.EventResult))
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
                  <&> fmap Widget.fromView
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
        return $ \width ->
            let bodyWidget = ExpressionGui.render width bodyGui ^. AlignedWidget.widget
            in
            bodyWidget : mkTypeWidgets (bodyWidget ^. Widget.width)
            & List.intersperse vspace
            & Box.vboxCentered
    where
        entityId = def ^. Sugar.drEntityId
        myId = WidgetIds.fromEntityId entityId
