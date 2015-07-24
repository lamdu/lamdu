{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.InjectEdit
    ( make
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar
import qualified Lamdu.GUI.WidgetIds as WidgetIds

make ::
    MonadA m =>
    Sugar.Inject (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.Inject tagG val mDelInject) pl =
    ExpressionGui.stdWrapParentExpr pl $ \myId ->
    do
        config <- ExprGuiM.readConfig
        let delEventMap =
                mDelInject
                <&> fmap WidgetIds.fromEntityId
                & maybe mempty
                (Widget.keysEventMapMovesCursor (Config.delKeys config) delDoc)
        tagEdit <-
            TagEdit.makeRecordTag
            (pl ^. Sugar.plData . ExprGuiT.plNearestHoles) tagG
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents delEventMap

        valEdit <- ExprGuiM.makeSubexpression 11 val
        ExpressionGui.hboxSpaced [tagEdit, valEdit]
    & ExprGuiM.assignCursor myId tagId
    where
        tagId = WidgetIds.fromEntityId (tagG ^. Sugar.tagInstance)
        delDoc = E.Doc ["Edit", "Delete Inject"]
