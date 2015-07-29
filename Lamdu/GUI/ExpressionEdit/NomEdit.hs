{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.NomEdit
    ( makeFromNom, makeToNom
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Store.Transaction (Transaction)
import           Data.Vector.Vector2 (Vector2(..))
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar
import qualified Lamdu.GUI.WidgetIds as WidgetIds

makeToNom ::
    MonadA m =>
    Sugar.Nominal (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeToNom (Sugar.Nominal tidg val mDel) pl =
    ExpressionGui.stdWrapParentExpr pl $ \myId ->
    do
        delEventMap <- mkDelEventMap "Wrapper" mDel
        valEdit <- ExprGuiM.makeSubexpression 0 val
        let valWidth = valEdit ^. ExpressionGui.egWidget . Widget.width
        nameEdit <-
            ExpressionGui.makeNameEdit (tidg ^. Sugar.tidgName) nameId
            <&> Widget.weakerEvents delEventMap
            <&> Widget.padToSizeAlign (Vector2 valWidth 0) 0.5
            <&> ExpressionGui.fromValueWidget
            >>= ExpressionGui.addValFrame nameId
        ExpressionGui.vboxTopFocalSpaced [valEdit, nameEdit]
    & ExprGuiM.assignCursor myId nameId
    where
        nameId = Widget.joinId (WidgetIds.fromEntityId (pl ^. Sugar.plEntityId)) ["name"]

makeFromNom ::
    MonadA m =>
    Sugar.Nominal (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeFromNom (Sugar.Nominal tidg val mDel) pl =
    ExpressionGui.stdWrapParentExpr pl $ \myId ->
    do
        delEventMap <- mkDelEventMap "Unwrapper" mDel
        nameEdit <-
            ExpressionGui.makeNameEdit (tidg ^. Sugar.tidgName) nameId
            <&> Widget.weakerEvents delEventMap
            <&> ExpressionGui.fromValueWidget

        valEdit <- ExprGuiM.makeSubexpression 11 val

        symLabel <- ExpressionGui.grammarLabel "⇈" (Widget.toAnimId myId)
        ExpressionGui.hboxSpaced [nameEdit, symLabel, valEdit]
    & ExprGuiM.assignCursor myId nameId
    where
        nameId = Widget.joinId (WidgetIds.fromEntityId (pl ^. Sugar.plEntityId)) ["name"]

mkDelEventMap ::
    MonadA m =>
    String -> Maybe (Transaction m Sugar.EntityId) ->
    ExprGuiM m (Widget.EventHandlers (Transaction m))
mkDelEventMap docName mDel =
    do
        config <- ExprGuiM.readConfig
        mDel
            <&> fmap WidgetIds.fromEntityId
            & maybe mempty
            (Widget.keysEventMapMovesCursor (Config.delKeys config) doc)
            & return
    where
        doc = E.Doc ["Edit", "Delete", docName]
