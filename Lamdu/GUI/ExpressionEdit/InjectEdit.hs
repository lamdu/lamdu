{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.InjectEdit
    ( make
    ) where

import           Prelude.Compat

import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Store.Transaction (Transaction)
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
import           Lamdu.Sugar.NearestHoles (NearestHoles)
import qualified Lamdu.Sugar.Types as Sugar
import qualified Lamdu.GUI.WidgetIds as WidgetIds

makeCommon ::
    MonadA m =>
    Sugar.TagG (Name m) -> Maybe (Transaction m Sugar.EntityId) ->
    NearestHoles -> [ExpressionGui m] ->
    ExprGuiM m (ExpressionGui m)
makeCommon tagG mDelInject nearestHoles valEdits =
    do
        config <- ExprGuiM.readConfig
        let delEventMap =
                mDelInject
                <&> fmap WidgetIds.fromEntityId
                & maybe mempty
                (Widget.keysEventMapMovesCursor (Config.delKeys config) delDoc)
        tagEdit <-
            TagEdit.makeRecordTag nearestHoles tagG
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents delEventMap
        ExpressionGui.hboxSpaced $ tagEdit : valEdits
    where
        delDoc = E.Doc ["Edit", "Delete Inject"]

make ::
    MonadA m =>
    Sugar.Inject (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make (Sugar.Inject tagG mVal mDelInject) pl =
    case mVal of
    Nothing ->
        ExpressionGui.stdWrap pl $
        makeCommon tagG mDelInject nearestHoles []
        & ExprGuiM.assignCursor
            (WidgetIds.fromEntityId (pl ^. Sugar.plEntityId)) tagId
    Just val ->
        ExpressionGui.stdWrapParentExpr pl $ \myId ->
        ExprGuiM.makeSubexpression 11 val <&> (:[])
        >>= makeCommon tagG mDelInject nearestHoles
        & ExprGuiM.assignCursor myId tagId
    where
        tagId = WidgetIds.fromEntityId (tagG ^. Sugar.tagInstance)
        nearestHoles =
            maybe (pl ^. Sugar.plData . ExprGuiT.plNearestHoles)
            ExprGuiT.nextHolesBefore mVal
