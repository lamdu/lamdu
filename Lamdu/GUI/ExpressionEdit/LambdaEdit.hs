{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LambdaEdit
    ( make
    ) where

import qualified Control.Lens as Lens
import           Control.Lens.Operators
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
    Sugar.Lambda (Name m) m (ExprGuiT.SugarExpr m) ->
    Sugar.Payload m ExprGuiT.Payload ->
    ExprGuiM m (ExpressionGui m)
make lam pl =
    ExprGuiM.withLocalPrecedence (ExpressionGui.precLeft .~ 0) $
    ExpressionGui.stdWrapParenify pl (ExpressionGui.MyPrecedence 0) $ \myId ->
    ExprGuiM.assignCursor myId bodyId $
    do
        BinderEdit.Parts mParamsEdit bodyEdit eventMap <-
            BinderEdit.makeParts binder bodyId myId
        let animId = Widget.toAnimId myId
        labelEdits <-
            case params of
            Sugar.NullParam{} -> return []
            _ -> ExpressionGui.grammarLabel "→" animId <&> (:[])
        (mParamsEdit ^.. Lens._Just) ++ labelEdits ++ [bodyEdit]
            & ExpressionGui.hboxSpaced
            <&> ExpressionGui.egWidget %~ Widget.weakerEvents eventMap
    where
        binder = lam ^. Sugar.lamBinder
        params = binder ^. Sugar.bParams
        body = binder ^. Sugar.bBody
        bodyId = WidgetIds.fromExprPayload $ body ^. Sugar.rPayload
