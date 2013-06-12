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
import Lamdu.Config.Default (defaultConfig)
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
import qualified Lamdu.WidgetIds as WidgetIds

infixPrecedence :: ExpressionGui.Precedence
infixPrecedence = 5

prefixPrecedence :: ExpressionGui.Precedence
prefixPrecedence = 10

make ::
  MonadA m => ParentPrecedence ->
  Sugar.ExpressionN m ->
  Sugar.Apply Sugar.Name (Sugar.ExpressionN m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make (ParentPrecedence parentPrecedence) exprS (Sugar.Apply func specialArgs annotatedArgs) myId =
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
    maybeOverrideHoleWrap
      | null annotatedArgs = id
      | otherwise = overrideHoleWrap
    overrideHoleWrap =
      ExpressionGui.egWidget %~ Widget.strongerEvents overrideWrapEventMap
    overrideWrapEventMap =
      maybe mempty Wrap.eventMap $
      exprS ^. Sugar.rPayload . Sugar.plActions
    isBoxed = not $ null annotatedArgs
    destGuid = func ^. Sugar.rGuid
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
    scaleTag =
      ExpressionGui.egWidget %~
      Widget.scale (realToFrac <$> Config.fieldTagScaleFactor defaultConfig)
    space = ExpressionGui.fromValueWidget BWidgets.stdSpaceWidget

mkBoxed ::
  MonadA m => Guid -> ExprGuiM m (ExpressionGui m) ->
  [(Sugar.TagG Sugar.Name, Sugar.ExpressionN m)] ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
mkBoxed destGuid mkFuncRow annotatedArgs =
  ExpressionGui.wrapExpression $ \myId ->
  assignCursorGuid myId destGuid $ do
    argEdits <-
      Grid.toWidget . Grid.make <$> traverse (makeArgRow myId) annotatedArgs
    ExpressionGui.withBgColor Layers.labeledApplyBG
      (Config.labeledApplyBGColor defaultConfig) (Widget.toAnimId myId ++ ["bg"]) .
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
