{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.NomEdit
    ( makeFromNom, makeToNom
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
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

makeFromNom ::
    MonadA m =>
    Sugar.Nominal (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeFromNom = makeNom "Unwrapper" "⇈"

makeToNom ::
    MonadA m =>
    Sugar.Nominal (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeToNom = makeNom "Wrapper" "⇊"

makeNom ::
    MonadA m =>
    String -> String ->
    Sugar.Nominal (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
makeNom docName sym (Sugar.Nominal tidg val mDel) pl =
    ExpressionGui.stdWrapParentExpr pl $ \myId ->
    do
        config <- ExprGuiM.readConfig
        let delEventMap =
                mDel
                <&> fmap WidgetIds.fromEntityId
                & maybe mempty
                (Widget.keysEventMapMovesCursor (Config.delKeys config) delDoc)
        nameEdit <-
            ExpressionGui.makeNameEdit name nameId
            <&> Widget.weakerEvents delEventMap
            <&> ExpressionGui.fromValueWidget

        valEdit <- ExprGuiM.makeSubexpression 11 val

        symLabel <- ExpressionGui.grammarLabel sym (Widget.toAnimId myId)
        ExpressionGui.hboxSpaced [nameEdit, symLabel, valEdit]
    & ExprGuiM.assignCursor myId nameId
    where
        name = tidg ^. Sugar.tidgName
        nameId = Widget.joinId (WidgetIds.fromEntityId (pl ^. Sugar.plEntityId)) ["name"]
        delDoc = E.Doc ["Edit", "Delete", docName]
