{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.ApplyEdit
  ( make
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Traversable (traverse, sequenceA)
import Lamdu.GUI.ExpressionGui (ExpressionGui, ParentPrecedence(..))
import Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Lamdu.Config as Config
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.Parens as Parens
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import qualified Lamdu.Sugar.Types as Sugar

infixPrecedence :: ExpressionGui.Precedence
infixPrecedence = 5

prefixPrecedence :: ExpressionGui.Precedence
prefixPrecedence = 10

make ::
  MonadA m => ParentPrecedence ->
  Sugar.Apply (Name m) (ExprGuiM.SugarExpr m) ->
  Sugar.Payload m ExprGuiM.Payload ->
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
      (maybe mempty (ExprEventMap.modifyEventMap [] config) (pl ^. Sugar.plActions))
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
    destEntityId = func ^. Sugar.rPayload . Sugar.plEntityId
    mk mPrecedence mkFuncRow
      | isBoxed = mkBoxed pl destEntityId mkFuncRow annotatedArgs myId
      | otherwise =
        mkMParened pl
        (ParentPrecedence parentPrecedence)
        (ExpressionGui.MyPrecedence <$> mPrecedence) destEntityId mkFuncRow myId

assignCursorEntityId :: MonadA m => Widget.Id -> Sugar.EntityId -> ExprGuiM m a -> ExprGuiM m a
assignCursorEntityId myId = ExprGuiM.assignCursor myId . WidgetIds.fromEntityId

makeTagView :: MonadA m => Sugar.EntityId -> Sugar.TagG (Name m) -> ExprGuiM m (ExpressionGui m)
makeTagView tagExprEntityId tagG =
  TagEdit.makeView tagG . Widget.toAnimId $
  WidgetIds.fromEntityId tagExprEntityId

makeArgRows ::
  MonadA m =>
  Sugar.AnnotatedArg (Name m) (ExprGuiM.SugarExpr m) ->
  ExprGuiM m [[(Grid.Alignment, ExprGuiM.WidgetT m)]]
makeArgRows arg = do
  argTagEdit <- makeTagView (arg ^. Sugar.aaTagExprEntityId) (arg ^. Sugar.aaTag)
  argValEdit <- ExprGuiM.makeSubexpression 0 $ arg ^. Sugar.aaExpr
  config <- ExprGuiM.widgetEnv WE.readConfig
  let
    scaleTag =
      ExpressionGui.egWidget %~
      Widget.scale (realToFrac <$> Config.fieldTagScaleFactor config)
  vspace <- ExprGuiM.widgetEnv BWidgets.verticalSpace
  pure
    [ replicate 3 (0.5, vspace)
    , ExpressionGui.makeRow
      [ (0, scaleTag argTagEdit)
      , (0.5, space)
      , (0, argValEdit)
      ]
    ]
  where
    space = ExpressionGui.fromValueWidget BWidgets.stdSpaceWidget

mkBoxed ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload ->
  Sugar.EntityId -> ExprGuiM m (ExpressionGui m) ->
  [Sugar.AnnotatedArg (Name m) (ExprGuiM.SugarExpr m)] ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
mkBoxed pl destEntityId mkFuncRow annotatedArgs =
  ExpressionGui.stdWrapParentExpr pl $ \myId ->
  assignCursorEntityId myId destEntityId $ do
    config <- ExprGuiM.widgetEnv WE.readConfig
    grid <-
      Grid.toWidget . Grid.make . concat <$> traverse makeArgRows annotatedArgs
    ExpressionGui.withBgColor (Config.layerValFrameBG (Config.layers config))
      (Config.valFrameBGColor config) (Widget.toAnimId myId ++ ["bg"]) .
      ExpressionGui.pad (realToFrac <$> Config.valFramePadding config) .
      ExpressionGui.addBelow 0 [(0, grid)] <$> mkFuncRow

mkMParened ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload ->
  ParentPrecedence ->
  Maybe ExpressionGui.MyPrecedence ->
  Sugar.EntityId -> ExprGuiM m  (ExpressionGui m) ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
mkMParened pl parentPrecedence mPrecedence destEntityId mkFuncRow =
  ExpressionGui.stdWrapParentExpr pl . parenify $ \myId ->
  assignCursorEntityId myId destEntityId mkFuncRow
  where
    parenify = case mPrecedence of
      Nothing -> id
      Just precedence ->
        ExpressionGui.parenify parentPrecedence precedence
        Parens.addHighlightedTextParens
