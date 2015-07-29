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
import           Lamdu.Sugar.Names.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

make ::
    MonadA m =>
    ExpressionGui.ParentPrecedence ->
    Sugar.Binder (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make parentPrecedence binder pl =
    ExpressionGui.stdWrapParenify plNoType parentPrecedence
    (ExpressionGui.MyPrecedence (ExpressionGui.Precedence 20 0)) $ \myId ->
    ExprGuiM.assignCursor myId bodyId $
    do
        BinderEdit.Parts paramEdits bodyEdit mWheresEdit eventMap <-
            BinderEdit.makeParts showParamType binder myId
        let animId = Widget.toAnimId myId
        paramsEdit <-
            map (ExpressionGui.egAlignment . _1 .~ 0.5) paramEdits
            & ExpressionGui.vboxTopFocalSpaced
            >>= case params of
                Sugar.FieldParams{} -> ExpressionGui.addValFrame myId
                _ -> return
            >>= case params of
                Sugar.NullParam{} -> return
                _ ->
                    \e ->
                    do
                        arrowLabel <- ExpressionGui.grammarLabel "→" animId
                        ExpressionGui.hboxSpaced [e, arrowLabel]
        ExpressionGui.hboxSpaced [paramsEdit, bodyEdit]
            <&> maybe id (ExpressionGui.addBelow 0 . (:[]) . (,) 0) mWheresEdit
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents eventMap
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
