{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.InjectEdit
    ( make
    ) where

import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.Names.Types (Name(..))
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.Types as Sugar

import           Lamdu.Prelude

makeCommon ::
    Monad m =>
    Text -> Sugar.TagG (Name m) ->
    NearestHoles -> [ExpressionGui m] ->
    ExprGuiM m (ExpressionGui m)
makeCommon tagSuffix tagG nearestHoles valEdits =
    do
        tag <-
            TagEdit.makeCaseTag nearestHoles tagG
            <&> TreeLayout.fromAlignedWidget
        suffixLabel <-
            ExpressionGui.grammarLabel tagSuffix (Widget.toAnimId tagId)
            <&> TreeLayout.fromAlignedWidget
        tag : suffixLabel : valEdits & ExpressionGui.combine
            & return
    where
        tagId = WidgetIds.fromEntityId (tagG ^. Sugar.tagInstance)

make ::
    Monad m =>
    Sugar.Inject (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.Inject tagG mVal) pl =
    case mVal of
    Nothing ->
        ExpressionGui.stdWrap pl
        <*>
        makeCommon "◦"
        -- Give the tag widget the identity of the whole inject
        (tagG & Sugar.tagInstance .~ (pl ^. Sugar.plEntityId))
        (pl ^. Sugar.plData . ExprGuiT.plNearestHoles) []
    Just val ->
        ExpressionGui.stdWrapParentExpr pl $ \myId ->
        ExprGuiM.makeSubexpressionWith (ExpressionGui.before .~ 11) val <&> (:[])
        >>= makeCommon "•" tagG (ExprGuiT.nextHolesBefore val)
        & ExprGuiM.assignCursor myId tagId
    where
        tagId = WidgetIds.fromEntityId (tagG ^. Sugar.tagInstance)
