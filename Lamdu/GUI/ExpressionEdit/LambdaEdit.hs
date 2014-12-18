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
import qualified Graphics.UI.Bottle.Widgets.Box as Box
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
      lambdaLabel <- ExprGuiM.widgetEnv $ BWidgets.grammarLabel "λ" myId
      paramEdits <-
        BinderEdit.makeParamsEdit showParamType bodyId (binder ^. Sugar.dParams)
      dotLabel <- ExprGuiM.widgetEnv $ BWidgets.grammarLabel ". " myId
      bodyEdit <- ExprGuiM.makeSubexpression 0 body
      wheres <-
        BinderEdit.makeWheres (binder ^. Sugar.dWhereItems) myId
        <&> Box.vboxAlign 0
      ExpressionGui.fromValueWidget lambdaLabel :
        paramEdits ++
        [ ExpressionGui.fromValueWidget dotLabel
        , bodyEdit
        ]
        & ExpressionGui.hbox
        & ExpressionGui.addBelow 0 [(0, wheres)]
        & return
  where
    body = binder ^. Sugar.dBody
    -- We show the param type instead of the lambda type
    showParamType = pl ^. Sugar.plData . ExprGuiM.plShowType
    plNoType = pl & Sugar.plData . ExprGuiM.plShowType .~ ExprGuiM.DoNotShowType
    bodyId = WidgetIds.fromEntityId $ body ^. Sugar.rPayload . Sugar.plEntityId
