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
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.ExpressionEdit.Parens as Parens
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ParamEdit as ParamEdit
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

make ::
  MonadA m =>
  ExpressionGui.ParentPrecedence ->
  Sugar.Lam (Name m) m (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make parentPrecedence (Sugar.Lam param body) pl =
  ExpressionGui.stdWrapParenify pl parentPrecedence (ExpressionGui.MyPrecedence 0)
  Parens.addHighlightedTextParens $ \myId ->
  ExprGuiM.assignCursor myId bodyId $ do
    config <- ExprGuiM.widgetEnv WE.readConfig
    lambdaLabel <-
      ExpressionGui.makeColoredLabel
      (Config.lambdaTextSize config)
      (Config.lambdaColor config) "λ" myId
    paramEdit <- ParamEdit.make bodyId param
    dotLabel <-
      ExpressionGui.makeColoredLabel
      (Config.rightArrowTextSize config)
      (Config.rightArrowColor config) ". " myId
    bodyEdit <- ExprGuiM.makeSubexpression 0 body
    return $ ExpressionGui.hbox
      [ ExpressionGui.fromValueWidget lambdaLabel
      , paramEdit
      , ExpressionGui.fromValueWidget dotLabel
      , bodyEdit
      ]
  where
    bodyId = WidgetIds.fromEntityId $ body ^. Sugar.rPayload . Sugar.plEntityId
