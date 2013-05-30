{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.LabeledApplyEdit
  ( make
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (mappend)
import Data.Store.Guid (Guid)
import Data.Traversable (traverse)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Data.Anchors (PresentationMode(..))
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetIds as WidgetIds

funcGuid :: Sugar.ExpressionP name m pl -> Maybe Guid
funcGuid f =
  case f ^. Sugar.rBody of
  Sugar.BodyGetVar gv -> Just $ gv ^. Sugar.gvIdentifier
  Sugar.BodyCollapsed c -> funcGuid $ c ^. Sugar.pFullExpression
  Sugar.BodyApply a -> funcGuid $ a ^. Expr.applyFunc
  _ -> Nothing

getPresentationMode :: MonadA m => Sugar.ExpressionN m -> ExprGuiM m PresentationMode
getPresentationMode func =
  case funcGuid func of
  Just guid ->
    ExprGuiM.transaction . Transaction.getP $
    Anchors.assocPresentationMode guid
  Nothing -> return Verbose

make ::
  MonadA m =>
  Sugar.LabeledApply Sugar.Name (Sugar.ExpressionN m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make (Sugar.LabeledApply func args) myId = do
  presentationMode <- getPresentationMode func
  case presentationMode of
    Verbose -> mkBoxed args []
    OO ->
      case args of
        ((_firstArgTag, firstArg):rest) -> mkBoxed rest . (: []) =<< ExprGuiM.makeSubexpresion 11 firstArg
        _ -> mkBoxed args []
  where
    mkBoxed as adjacant = boxedApply func as adjacant myId

makeTagView :: MonadA m => Widget.Id -> Sugar.TagG Sugar.Name -> ExprGuiM m (ExpressionGui m)
makeTagView myId tagG =
  TagEdit.makeView tagG . Widget.toAnimId . mappend myId .
  WidgetIds.fromGuid $ tagG ^. Sugar.tagGuid

makeArgRow ::
  MonadA m => Widget.Id ->
  (Sugar.TagG Sugar.Name, Sugar.ExpressionN m) ->
  ExprGuiM m [(Grid.Alignment, ExprGuiM.WidgetT m)]
makeArgRow myId (tagG, namedArgExpr) = do
  argTagEdit <- makeTagView myId tagG
  argValEdit <- ExprGuiM.makeSubexpresion 0 namedArgExpr
  pure $ ExpressionGui.makeRow
    [ (0, scaleTag argTagEdit)
    , (0.5, space)
    , (0, argValEdit)
    ]
  where
    scaleTag = ExpressionGui.egWidget %~ Widget.scale Config.fieldTagScale
    space = ExpressionGui.fromValueWidget BWidgets.stdSpaceWidget

boxedApply ::
  MonadA m => Sugar.ExpressionN m ->
  [(Sugar.TagG Sugar.Name, Sugar.ExpressionN m)] ->
  [ExpressionGui m] ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
boxedApply func args funcAdjacants =
  ExpressionGui.wrapExpression $ \myId ->
  ExprGuiM.assignCursor myId (WidgetIds.fromGuid (func ^. Sugar.rGuid)) $ do
    funcEdit <- ExprGuiM.makeSubexpresion 10 func
    argEdits <-
      Grid.toWidget . Grid.make <$> traverse (makeArgRow myId) args
    pure .
      ExpressionGui.withBgColor Layers.labeledApplyBG
      Config.labeledApplyBGColor (Widget.toAnimId myId ++ ["bg"]) .
      ExpressionGui.addBelow 0 [(0, argEdits)] .
      ExpressionGui.hboxSpaced $ funcEdit : funcAdjacants
