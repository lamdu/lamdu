{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.InjectEdit
    ( make
    ) where

import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widget.TreeLayout as TreeLayout
import qualified Lamdu.GUI.ExpressionEdit.ApplyEdit as ApplyEdit
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
    Sugar.TagG (Name m) ->
    NearestHoles -> [ExpressionGui m] ->
    ExprGuiM m (ExpressionGui m)
makeCommon tagG nearestHoles valEdits =
    ExpressionGui.combineSpaced
    <*> ( TagEdit.makeCaseTag nearestHoles tagG
          <&> TreeLayout.fromWithTextPos <&> (: valEdits)
        )

make ::
    Monad m =>
    Sugar.Inject (Name m) (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.Inject tagG mVal) pl =
    case mVal of
    Nothing ->
        makeCommon
        -- Give the tag widget the identity of the whole inject
        (tagG & Sugar.tagInstance .~ (pl ^. Sugar.plEntityId))
        (pl ^. Sugar.plData . ExprGuiT.plNearestHoles) []
        & ExpressionGui.stdWrap pl
    Just val ->
        ExprGuiM.makeSubexpressionWith ApplyEdit.prefixPrecedence
        (ExpressionGui.before .~ ApplyEdit.prefixPrecedence) val <&> (:[])
        >>= makeCommon tagG (ExprGuiT.nextHolesBefore val)
        & Widget.assignCursor myId tagId
        & ExpressionGui.stdWrapParentExpr pl
    where
        myId = WidgetIds.fromExprPayload pl
        tagId = WidgetIds.fromEntityId (tagG ^. Sugar.tagInstance)
