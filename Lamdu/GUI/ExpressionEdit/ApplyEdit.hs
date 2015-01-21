{-# LANGUAGE OverloadedStrings #-}
module Lamdu.GUI.ExpressionEdit.ApplyEdit
  ( make
  ) where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Lens.Operators
import           Control.MonadA (MonadA)
import           Data.Monoid (Monoid(..))
import           Data.Store.Transaction (Transaction)
import           Data.Traversable (traverse, sequenceA)
import           Graphics.UI.Bottle.Widget (Widget)
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid
import qualified Lamdu.GUI.BottleWidgets as BWidgets
import qualified Lamdu.GUI.ExpressionEdit.EventMap as ExprEventMap
import qualified Lamdu.GUI.ExpressionEdit.Parens as Parens
import qualified Lamdu.GUI.ExpressionEdit.TagEdit as TagEdit
import           Lamdu.GUI.ExpressionGui (ExpressionGui, ParentPrecedence(..))
import qualified Lamdu.GUI.ExpressionGui as ExpressionGui
import           Lamdu.GUI.ExpressionGui.Monad (ExprGuiM)
import qualified Lamdu.GUI.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.GUI.WidgetEnvT as WE
import qualified Lamdu.GUI.WidgetIds as WidgetIds
import           Lamdu.Sugar.AddNames.Types (Name(..))
import qualified Lamdu.Sugar.Types as Sugar

type T = Transaction

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
      sequenceA
      [ maybeOverrideHoleWrap <$> ExprGuiM.makeSubexpression (prefixPrecedence+1) func
      , ExprGuiM.makeSubexpression prefixPrecedence arg
      ]
      >>= ExpressionGui.hboxSpaced
      & mk (Just prefixPrecedence)
    Sugar.InfixArgs l r ->
      sequenceA
      [ ExprGuiM.makeSubexpression (infixPrecedence+1) l
      , -- TODO: What precedence to give when it must be atomic?:
        overrideModifyEventMap <$> ExprGuiM.makeSubexpression 20 func
      , ExprGuiM.makeSubexpression (infixPrecedence+1) r
      ]
      >>= ExpressionGui.hboxSpaced
      & mk (Just infixPrecedence)
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

makeParamTag :: MonadA m => Sugar.EntityId -> Sugar.TagG (Name m) -> ExprGuiM m (ExpressionGui m)
makeParamTag tagExprEntityId tagG =
  TagEdit.makeParamTag tagG $ Widget.toAnimId $
  WidgetIds.fromEntityId tagExprEntityId

makeArgRows ::
  MonadA m =>
  Sugar.AnnotatedArg (Name m) (ExprGuiM.SugarExpr m) ->
  ExprGuiM m [[(Grid.Alignment, Widget (T m))]]
makeArgRows arg = do
  argTagEdit <- makeParamTag (arg ^. Sugar.aaTagExprEntityId) (arg ^. Sugar.aaTag)
  argValEdit <- ExprGuiM.makeSubexpression 0 $ arg ^. Sugar.aaExpr
  vspace <- ExprGuiM.widgetEnv BWidgets.verticalSpace
  space <- BWidgets.stdSpaceWidget & ExprGuiM.widgetEnv <&> ExpressionGui.fromValueWidget
  pure
    [ replicate 3 (0.5, vspace)
    , ExpressionGui.makeRow
      [ (0, argTagEdit)
      , (0.5, space)
      , (0, argValEdit)
      ]
    ]

mkBoxed ::
  MonadA m =>
  Sugar.Payload m ExprGuiM.Payload ->
  Sugar.EntityId -> ExprGuiM m (ExpressionGui m) ->
  [Sugar.AnnotatedArg (Name m) (ExprGuiM.SugarExpr m)] ->
  Widget.Id -> ExprGuiM m (ExpressionGui m)
mkBoxed pl destEntityId mkFuncRow annotatedArgs =
  ExpressionGui.stdWrapParentExpr pl $ \myId ->
  assignCursorEntityId myId destEntityId $ do
    grid <-
      Grid.toWidget . Grid.make . concat <$> traverse makeArgRows annotatedArgs
    mkFuncRow
      <&> ExpressionGui.addBelow 0 [(0, grid)]
      >>= ExpressionGui.addValFrame myId

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
