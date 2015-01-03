{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.LambdaEdit
  ( make
  ) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Lamdu.GUI.ExpressionGui (ExpressionGui)
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.BinderEdit as BinderEdit
import qualified Lamdu.GUI.ExpressionEdit.Parens as Parens
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

make ::
  MonadA m =>
  ExpressionGui.ParentPrecedence ->
  Sugar.Binder (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make parentPrecedence binder pl =
  ExpressionGui.stdWrapParenify plNoType parentPrecedence (ExpressionGui.MyPrecedence 0)
  Parens.addHighlightedTextParens $ \myId ->
  ExprGuiM.assignCursor myId bodyId $
    do
      lambdaLabel <- ExpressionGui.grammarLabel "λ" $ Widget.toAnimId myId
      paramsEdit <-
        BinderEdit.makeParamsEdit showParamType bodyId params
        <&> map ((,) 0)
        >>= ExpressionGui.vboxDownwardsSpaced
      dotLabel <- ExpressionGui.grammarLabel ". " $ Widget.toAnimId myId
      bodyEdit <- BinderEdit.makeResultEdit (binder ^. Sugar.dMActions) params body myId
      mWheresEdit <-
        BinderEdit.makeWheres (binder ^. Sugar.dWhereItems) myId
      lambdaLabel :
        [ paramsEdit
        , dotLabel
        , bodyEdit
        ]
        & ExpressionGui.hbox
        & maybe id (ExpressionGui.addBelow 0 . (:[]) . (,) 0) mWheresEdit
        & return
  where
    params = binder ^. Sugar.dParams
    body = binder ^. Sugar.dBody
    -- We show the param type instead of the lambda type
    showParamType = pl ^. Sugar.plData . ExprGuiM.plShowType
    plNoType = pl & Sugar.plData . ExprGuiM.plShowType .~ ExprGuiM.DoNotShowType
    bodyId = WidgetIds.fromEntityId $ body ^. Sugar.rPayload . Sugar.plEntityId
