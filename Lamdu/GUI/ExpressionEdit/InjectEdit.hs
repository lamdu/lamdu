{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.InjectEdit
    ( make
    ) where

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import           Lamdu.Sugar.Names.Types (Name(..))
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.Types as Sugar
import qualified Lamdu.GUI.WidgetIds as WidgetIds

import           Prelude.Compat

makeCommon ::
    MonadA m =>
    Sugar.TagG (Name m) ->
    NearestHoles -> [ExpressionGui m] ->
    ExprGuiM m (ExpressionGui m)
makeCommon tagG nearestHoles valEdits =
    do
        tagEdit <- TagEdit.makeRecordTag nearestHoles tagG
        ExpressionGui.hboxSpaced $ tagEdit : valEdits

make ::
    MonadA m =>
    Sugar.Inject (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.Inject tagG mVal) pl =
    case mVal of
    Nothing ->
        ExpressionGui.stdWrap pl $
        makeCommon tagG nearestHoles []
        & ExprGuiM.assignCursor
            (WidgetIds.fromEntityId (pl ^. Sugar.plEntityId)) tagId
    Just val ->
        ExpressionGui.stdWrapParentExpr pl $ \myId ->
        ExprGuiM.makeSubexpression (ExpressionGui.precLeft .~ 11) val <&> (:[])
        >>= makeCommon tagG nearestHoles
        & ExprGuiM.assignCursor myId tagId
    where
        tagId = WidgetIds.fromEntityId (tagG ^. Sugar.tagInstance)
        nearestHoles =
            maybe (pl ^. Sugar.plData . ExprGuiT.plNearestHoles)
            ExprGuiT.nextHolesBefore mVal
