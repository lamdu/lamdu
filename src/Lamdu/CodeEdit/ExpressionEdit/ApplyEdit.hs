{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.ApplyEdit
  ( make
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (traverse, sequenceA)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui, ParentPrecedence(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.Parens as Parens
import qualified Lamdu.CodeEdit.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.Wrap as Wrap
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

infixPrecedence :: ExpressionGui.Precedence
infixPrecedence = 5

prefixPrecedence :: ExpressionGui.Precedence
prefixPrecedence = 10

make ::
  MonadA m => ParentPrecedence ->
  ExprGuiM.SugarExpr m ->
  Sugar.Apply Sugar.Name (ExprGuiM.SugarExpr m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make (ParentPrecedence parentPrecedence) exprS (Sugar.Apply func specialArgs annotatedArgs) myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    maybeOverrideHoleWrap
      | null annotatedArgs = id
      | otherwise = overrideHoleWrap
    overrideHoleWrap =
      ExpressionGui.egWidget %~ Widget.strongerEvents overrideWrapEventMap
    overrideWrapEventMap =
      maybe mempty (Wrap.eventMap config) $
      exprS ^. Sugar.rPayload . Sugar.plActions
  case specialArgs of
    Sugar.NoSpecialArgs ->
      mk Nothing $
      overrideHoleWrap <$> ExprGuiM.makeSubexpresion (if isBoxed then 0 else parentPrecedence) func
    Sugar.ObjectArg arg ->
      mk (Just prefixPrecedence) $ ExpressionGui.hboxSpaced <$> sequenceA
      [ maybeOverrideHoleWrap <$> ExprGuiM.makeSubexpresion (prefixPrecedence+1) func
      , ExprGuiM.makeSubexpresion prefixPrecedence arg
      ]
    Sugar.InfixArgs l r ->
      mk (Just infixPrecedence) $ ExpressionGui.hboxSpaced <$> sequenceA
      [ ExprGuiM.makeSubexpresion (infixPrecedence+1) l
      , -- TODO: What precedence to give when it must be atomic?:
        overrideHoleWrap <$> ExprGuiM.makeSubexpresion 20 func
      , ExprGuiM.makeSubexpresion (infixPrecedence+1) r
      ]
  where
    isBoxed = not $ null annotatedArgs
    destGuid = func ^. Sugar.rPayload . Sugar.plGuid
    mk mPrecedence mkFuncRow
      | isBoxed = mkBoxed destGuid mkFuncRow annotatedArgs myId
      | otherwise =
        mkMParened
        (ParentPrecedence parentPrecedence)
        (ExpressionGui.MyPrecedence <$> mPrecedence) destGuid mkFuncRow myId

assignCursorGuid :: MonadA m => Widget.Id -> Guid -> ExprGuiM m a -> ExprGuiM m a
assignCursorGuid myId = ExprGuiM.assignCursor myId . WidgetIds.fromGuid

makeTagView :: MonadA m => Widget.Id -> Sugar.TagG Sugar.Name -> ExprGuiM m (ExpressionGui m)
makeTagView myId tagG =
  TagEdit.makeView tagG . Widget.toAnimId . mappend myId .
  WidgetIds.fromGuid $ tagG ^. Sugar.tagGuid

makeArgRow ::
  MonadA m => Widget.Id ->
  (Sugar.TagG Sugar.Name, ExprGuiM.SugarExpr m) ->
  ExprGuiM m [(Grid.Alignment, ExprGuiM.WidgetT m)]
makeArgRow myId (tagG, namedArgExpr) = do
  argTagEdit <- makeTagView myId tagG
  argValEdit <- ExprGuiM.makeSubexpresion 0 namedArgExpr
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    scaleTag =
      ExpressionGui.egWidget %~
      Widget.scale (realToFrac <$> Config.fieldTagScaleFactor config)
  pure $ ExpressionGui.makeRow
    [ (0, scaleTag argTagEdit)
    , (0.5, space)
    , (0, argValEdit)
    ]
  where
    space = ExpressionGui.fromValueWidget BWidgets.stdSpaceWidget

mkBoxed ::
  MonadA m => Guid -> ExprGuiM m (ExpressionGui m) ->
  [(Sugar.TagG Sugar.Name, ExprGuiM.SugarExpr m)] ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
mkBoxed destGuid mkFuncRow annotatedArgs =
  ExpressionGui.wrapExpression $ \myId ->
  assignCursorGuid myId destGuid $ do
    config <- ExprGuiM.widgetEnv WE.readConfig
    argEdits <-
      Grid.toWidget . Grid.make <$> traverse (makeArgRow myId) annotatedArgs
    ExpressionGui.withBgColor Layers.labeledApplyBG
      (Config.labeledApplyBGColor config) (Widget.toAnimId myId ++ ["bg"]) .
      ExpressionGui.addBelow 0 [(0, argEdits)] <$> mkFuncRow

mkMParened ::
  MonadA m => ParentPrecedence ->
  Maybe ExpressionGui.MyPrecedence -> Guid ->
  ExprGuiM m  (ExpressionGui m) -> Widget.Id -> ExprGuiM m (ExpressionGui m)
mkMParened parentPrecedence mPrecedence destGuid mkFuncRow =
  ExpressionGui.wrapExpression . parenify $ \myId ->
  assignCursorGuid myId destGuid mkFuncRow
  where
    parenify = case mPrecedence of
      Nothing -> id
      Just precedence ->
        ExpressionGui.parenify parentPrecedence precedence
        Parens.addHighlightedTextParens
