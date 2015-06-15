{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LambdaEdit
    ( make
    ) where

import           Control.Lens.Operators
import           Control.Lens.Tuple
import           Control.MonadA (MonadA)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui)
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionGui.Types as ExprGuiT
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

make ::
    MonadA m =>
    ExpressionGui.ParentPrecedence ->
    Sugar.Binder (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make parentPrecedence binder pl =
    ExpressionGui.stdWrapParenify plNoType parentPrecedence
    (ExpressionGui.MyPrecedence 0) $ \myId ->
    ExprGuiM.assignCursor myId bodyId $
    do
        paramsEdit <-
            BinderEdit.makeParamsEdit showParamType
            (ExprGuiT.nextHolesBefore body) bodyId params
            <&> map (ExpressionGui.egAlignment . _1 .~ 0.5)
            >>= ExpressionGui.vboxTopFocalSpaced
            >>= case params of
                    Sugar.FieldParams _ -> ExpressionGui.addValFrame myId
                    _ -> return
        arrowLabel <- ExpressionGui.grammarLabel "→" $ Widget.toAnimId myId
        bodyEdit <- BinderEdit.makeResultEdit (binder ^. Sugar.bMActions) params body
        mWheresEdit <-
            BinderEdit.makeWheres (binder ^. Sugar.bWhereItems) myId
        ExpressionGui.hboxSpaced [paramsEdit, arrowLabel, bodyEdit]
            <&> maybe id (ExpressionGui.addBelow 0 . (:[]) . (,) 0) mWheresEdit
    where
        params = binder ^. Sugar.bParams
        body = binder ^. Sugar.bBody
        -- We show the param type instead of the lambda type
        showParamType = pl ^. Sugar.plData . ExprGuiT.plShowAnnotation
        plNoType =
            pl
            & Sugar.plData . ExprGuiT.plShowAnnotation
            .~ ExprGuiT.DoNotShowAnnotation
        bodyId = WidgetIds.fromExprPayload $ body ^. Sugar.rPayload
