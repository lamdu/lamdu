{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.DefinitionEdit
    ( make
    ) where

import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (transaction)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import qualified Data.Store.Transaction as Transaction
import           Graphics.UI.Bottle.Animation (AnimId)
import qualified Graphics.UI.Bottle.EventMap as E
import           Graphics.UI.Bottle.MetaKey (MetaKey(..), noMods)
import           Graphics.UI.Bottle.View (View)
import qualified Graphics.UI.Bottle.View as View
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Graphics.UI.Bottle.Widgets.Box as Box
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView
import qualified Graphics.UI.GLFW as GLFW
import           Lamdu.Calc.Type.Scheme (Scheme(..), schemeType)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import qualified Lamdu.GUI.ExpressionEdit.BuiltinEdit as BuiltinEdit
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import           Lamdu.GUI.ExpressionGui.Types (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.Spacing as Spacing
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..), DefinitionN)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

undeleteButton ::
    Monad m => T m Widget.Id -> ExprGuiM m (Widget (T m Widget.EventResult))
undeleteButton undelete =
    TextView.makeFocusableLabel "Undelete..." <&> E.weakerEvents eventMap
    where
        eventMap =
            Widget.keysEventMapMovesCursor [MetaKey noMods GLFW.Key'Enter]
            (E.Doc ["Edit", "Undelete definition"]) undelete

makeExprDefinition ::
    Monad m =>
    Sugar.Definition (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.DefinitionExpression (Name m) m (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeExprDefinition def bodyExpr =
    do
        theme <- ExprGuiM.readTheme
        let defColor = Theme.definitionColor (Theme.name theme)
        BinderEdit.make (def ^. Sugar.drName) defColor
            (bodyExpr ^. Sugar.deContent) myId
    where
        entityId = def ^. Sugar.drEntityId
        myId = WidgetIds.fromEntityId entityId

makeBuiltinDefinition ::
    Monad m =>
    Sugar.Definition (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.DefinitionBuiltin m -> ExprGuiM m (Widget (T m Widget.EventResult))
makeBuiltinDefinition def builtin =
    do
        defColor <- ExprGuiM.readTheme <&> Theme.name <&> Theme.definitionColor
        assignment <-
            [ ExpressionGui.makeNameOriginEdit name defColor (Widget.joinId myId ["name"])
            , TextView.makeLabel "=" <&> Widget.fromView
            , BuiltinEdit.make builtin myId
            ]
            & sequenceA
            >>= Spacing.hboxCenteredSpaced
        typeView <- topLevelSchemeTypeView (builtin ^. Sugar.biType) entityId ["builtinType"]
        Box.vboxAlign 0 [assignment, Widget.fromView typeView] & return
    where
        name = def ^. Sugar.drName
        entityId = def ^. Sugar.drEntityId
        myId = WidgetIds.fromEntityId entityId

make :: Monad m => DefinitionN m ExprGuiT.Payload -> ExprGuiM m (ExpressionGui m)
make def =
    do
        defStateProp <-
            def ^. Sugar.drDefinitionState . Transaction.mkProperty
            & transaction
        let defState = Property.value defStateProp
        addDeletionDiagonal <-
            case defState of
            Sugar.DeletedDefinition -> ExpressionGui.addDeletionDiagonal ?? 0.02
            Sugar.LiveDefinition -> return id
        defGui <-
            case def ^. Sugar.drBody of
            Sugar.DefinitionBodyExpression bodyExpr ->
                makeExprDefinition def bodyExpr
            Sugar.DefinitionBodyBuiltin builtin ->
                makeBuiltinDefinition def builtin <&> TreeLayout.fromWidget
            <&> addDeletionDiagonal
        case defState of
            Sugar.LiveDefinition -> return defGui
            Sugar.DeletedDefinition ->
                do
                    buttonGui <-
                        myId <$ Property.set defStateProp Sugar.LiveDefinition
                        & undeleteButton <&> TreeLayout.fromWidget
                    TreeLayout.vbox [defGui, buttonGui] & return
    & Reader.local (View.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = def ^. Sugar.drEntityId & WidgetIds.fromEntityId

topLevelSchemeTypeView ::
    Monad m => Scheme -> Sugar.EntityId -> AnimId -> ExprGuiM m View
topLevelSchemeTypeView scheme entityId suffix =
    -- At the definition-level, Schemes can be shown as ordinary
    -- types to avoid confusing forall's:
    WidgetIds.fromEntityId entityId
    & (`Widget.joinId` suffix)
    & Widget.toAnimId
    & TypeView.make (scheme ^. schemeType)
