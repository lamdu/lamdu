{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.ApplyEdit
  ( make
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (traverse, sequenceA)
import Lamdu.GUI.ExpressionGui (ExpressionGui, ParentPrecedence(..))
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.ExpressionEdit.Modify as Modify
import qualified Lamdu.GUI.ExpressionEdit.Parens as Parens
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

infixPrecedence :: ExpressionGui.Precedence
infixPrecedence = 5

prefixPrecedence :: ExpressionGui.Precedence
prefixPrecedence = 10

make ::
  MonadA m => ParentPrecedence ->
  Sugar.Apply Sugar.Name (ExprGuiM.SugarExpr m) ->
  Sugar.Payload Sugar.Name m a ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
make (ParentPrecedence parentPrecedence) (Sugar.Apply func specialArgs annotatedArgs) pl myId = do
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    maybeOverrideHoleWrap
      | null annotatedArgs = id
      | otherwise = overrideModifyEventMap
    overrideModifyEventMap =
      ExpressionGui.egWidget %~
      Widget.strongerEvents
      (maybe mempty (Modify.eventMap config) (pl ^. Sugar.plActions))
  case specialArgs of
    Sugar.NoSpecialArgs ->
      mk Nothing $
      overrideModifyEventMap <$> ExprGuiM.makeSubexpression (if isBoxed then 0 else parentPrecedence) func
    Sugar.ObjectArg arg ->
      mk (Just prefixPrecedence) $ ExpressionGui.hboxSpaced <$> sequenceA
      [ maybeOverrideHoleWrap <$> ExprGuiM.makeSubexpression (prefixPrecedence+1) func
      , ExprGuiM.makeSubexpression prefixPrecedence arg
      ]
    Sugar.InfixArgs l r ->
      mk (Just infixPrecedence) $ ExpressionGui.hboxSpaced <$> sequenceA
      [ ExprGuiM.makeSubexpression (infixPrecedence+1) l
      , -- TODO: What precedence to give when it must be atomic?:
        overrideModifyEventMap <$> ExprGuiM.makeSubexpression 20 func
      , ExprGuiM.makeSubexpression (infixPrecedence+1) r
      ]
  where
    isBoxed = not $ null annotatedArgs
    destGuid = func ^. Sugar.rPayload . Sugar.plGuid
    mk mPrecedence mkFuncRow
      | isBoxed = mkBoxed pl destGuid mkFuncRow annotatedArgs myId
      | otherwise =
        mkMParened pl
        (ParentPrecedence parentPrecedence)
        (ExpressionGui.MyPrecedence <$> mPrecedence) destGuid mkFuncRow myId

assignCursorGuid :: MonadA m => Widget.Id -> Guid -> ExprGuiM m a -> ExprGuiM m a
assignCursorGuid myId = ExprGuiM.assignCursor myId . WidgetIds.fromGuid

makeTagView :: MonadA m => Guid -> Sugar.TagG Sugar.Name -> ExprGuiM m (ExpressionGui m)
makeTagView tagExprGuid tagG =
  TagEdit.makeView tagG . Widget.toAnimId $
  WidgetIds.fromGuid tagExprGuid

makeArgRow ::
  MonadA m =>
  Sugar.AnnotatedArg Sugar.Name (ExprGuiM.SugarExpr m) ->
  ExprGuiM m [(Grid.Alignment, ExprGuiM.WidgetT m)]
makeArgRow arg = do
  argTagEdit <- makeTagView (arg ^. Sugar.aaTagExprGuid) (arg ^. Sugar.aaTag)
  argValEdit <- ExprGuiM.makeSubexpression 0 $ arg ^. Sugar.aaExpr
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
  MonadA m =>
  Sugar.Payload Sugar.Name m a ->
  Guid -> ExprGuiM m (ExpressionGui m) ->
  [Sugar.AnnotatedArg Sugar.Name (ExprGuiM.SugarExpr m)] ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
mkBoxed pl destGuid mkFuncRow annotatedArgs =
  ExpressionGui.wrapExpression pl $ \myId ->
  assignCursorGuid myId destGuid $ do
    config <- ExprGuiM.widgetEnv WE.readConfig
    argEdits <-
      Grid.toWidget . Grid.make <$> traverse makeArgRow annotatedArgs
    ExpressionGui.withBgColor (Config.layerLabeledApplyBG (Config.layers config))
      (Config.labeledApplyBGColor config) (Widget.toAnimId myId ++ ["bg"]) .
      ExpressionGui.addBelow 0 [(0, argEdits)] <$> mkFuncRow

mkMParened ::
  MonadA m =>
  Sugar.Payload Sugar.Name m a ->
  ParentPrecedence ->
  Maybe ExpressionGui.MyPrecedence ->
  Guid -> ExprGuiM m  (ExpressionGui m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
mkMParened pl parentPrecedence mPrecedence destGuid mkFuncRow =
  ExpressionGui.wrapExpression pl . parenify $ \myId ->
  assignCursorGuid myId destGuid mkFuncRow
  where
    parenify = case mPrecedence of
      Nothing -> id
      Just precedence ->
        ExpressionGui.parenify parentPrecedence precedence
        Parens.addHighlightedTextParens
