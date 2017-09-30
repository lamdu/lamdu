{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.DefinitionEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import           Control.Monad.Transaction (transaction)
import qualified Data.Store.Property as Property
import           Data.Store.Transaction (Transaction)
import           GUI.Momentu.Align (WithTextPos)
import qualified GUI.Momentu.Align as Align
import           GUI.Momentu.Animation (AnimId)
import qualified GUI.Momentu.Element as Element
import qualified GUI.Momentu.EventMap as E
import           GUI.Momentu.Glue ((/-/), (/|/))
import           GUI.Momentu.MetaKey (MetaKey(..), noMods)
import qualified GUI.Momentu.MetaKey as MetaKey
import qualified GUI.Momentu.Responsive as Responsive
import           GUI.Momentu.View (View)
import           GUI.Momentu.Widget (Widget)
import qualified GUI.Momentu.Widget as Widget
import qualified GUI.Momentu.Widgets.TextView as TextView
import           Lamdu.Calc.Type.Scheme (Scheme(..), schemeType)
import qualified Lamdu.Config.Theme as Theme
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import qualified Lamdu.GUI.ExpressionEdit.BuiltinEdit as BuiltinEdit
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import           Lamdu.GUI.ExpressionGui.Types (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.NameEdit as NameEdit
import qualified Lamdu.GUI.TypeView as TypeView
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..), DefinitionN)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

type T = Transaction

undeleteButton ::
    Monad m =>
    T m Widget.Id -> ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
undeleteButton undelete =
    TextView.makeFocusableLabel "Undelete..."
    <&> Align.tValue %~ E.weakerEvents eventMap
    where
        eventMap =
            Widget.keysEventMapMovesCursor [MetaKey noMods MetaKey.Key'Enter]
            (E.Doc ["Edit", "Undelete definition"]) undelete

makeExprDefinition ::
    Monad m =>
    Sugar.Definition (Name (T m)) (T m) (ExprGuiT.SugarExpr m) ->
    Sugar.DefinitionExpression (Name (T m)) (T m) (ExprGuiT.SugarExpr m) ->
    ExprGuiM m (ExpressionGui m)
makeExprDefinition def bodyExpr =
    do
        theme <- Lens.view Theme.theme
        let defColor = Theme.definitionColor (Theme.name theme)
        BinderEdit.make (def ^. Sugar.drName) defColor
            (bodyExpr ^. Sugar.deContent) myId
    where
        entityId = def ^. Sugar.drEntityId
        myId = WidgetIds.fromEntityId entityId

makeBuiltinDefinition ::
    Monad m =>
    Sugar.Definition (Name (T m)) (T m) (ExprGuiT.SugarExpr m) ->
    Sugar.DefinitionBuiltin (T m) ->
    ExprGuiM m (WithTextPos (Widget (T m Widget.EventResult)))
makeBuiltinDefinition def builtin =
    do
        defColor <- Lens.view Theme.theme <&> Theme.name <&> Theme.definitionColor
        nameEdit <- NameEdit.makeAtBinder name defColor (Widget.joinId myId ["name"])
        equals <- TextView.makeLabel " = "
        builtinEdit <- BuiltinEdit.make builtin myId
        typeView <- topLevelSchemeTypeView (builtin ^. Sugar.biType) entityId ["builtinType"]
        (nameEdit /|/ equals /|/ builtinEdit)
            /-/
            typeView
            & return
    where
        name = def ^. Sugar.drName
        entityId = def ^. Sugar.drEntityId
        myId = WidgetIds.fromEntityId entityId

make :: Monad m => DefinitionN m ExprGuiT.Payload -> ExprGuiM m (ExpressionGui m)
make def =
    do
        defStateProp <- def ^. Sugar.drDefinitionState & transaction
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
                makeBuiltinDefinition def builtin <&> Responsive.fromWithTextPos
            <&> addDeletionDiagonal
        case defState of
            Sugar.LiveDefinition -> return defGui
            Sugar.DeletedDefinition ->
                do
                    buttonGui <-
                        myId <$ Property.set defStateProp Sugar.LiveDefinition
                        & undeleteButton <&> Responsive.fromWithTextPos
                    Responsive.vbox [defGui, buttonGui] & return
    & Reader.local (Element.animIdPrefix .~ Widget.toAnimId myId)
    where
        myId = def ^. Sugar.drEntityId & WidgetIds.fromEntityId

topLevelSchemeTypeView ::
    Monad m => Scheme -> Sugar.EntityId -> AnimId -> ExprGuiM m (WithTextPos View)
topLevelSchemeTypeView scheme entityId suffix =
    -- At the definition-level, Schemes can be shown as ordinary
    -- types to avoid confusing forall's:
    WidgetIds.fromEntityId entityId
    & (`Widget.joinId` suffix)
    & Widget.toAnimId
    & TypeView.make (scheme ^. schemeType)
