{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit(make) where

import Control.Arrow (first)
import Control.Monad (liftM)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker(ExpressionEditMaker)
import Editor.CTransaction (CTransaction, getP)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (EventHandlers)
import qualified Data.Store.Property as Property
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Editor.CodeEdit.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Editor.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Editor.CodeEdit.ExpressionEdit.WhereEdit as WhereEdit
import qualified Editor.CodeEdit.Parens as Parens
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

data HoleResultPicker m = NotAHole | IsAHole (Maybe (HoleEdit.ResultPicker m))
foldHolePicker
  :: r -> (Maybe (HoleEdit.ResultPicker m) -> r)
  -> HoleResultPicker m -> r
foldHolePicker notHole _isHole NotAHole = notHole
foldHolePicker _notHole isHole (IsAHole x) = isHole x

make :: MonadF m => ExpressionEditMaker m
make ancestry sExpr = do
  exprI <- getP $ Sugar.rExpressionPtr sExpr
  let
    isAHole = (fmap . liftM . first) IsAHole
    notAHole = (fmap . liftM) ((,) NotAHole)
    wrapNonHoleExpr =
      notAHole .
      BWidgets.wrapDelegatedWithKeys Config.exprFocusDelegatorKeys FocusDelegator.Delegating id
    exprId = WidgetIds.fromIRef exprI
    addParens parensType mkWidget myId =
      mkWidget myId >>= Parens.addParens parensType exprId
    parenify = maybe id addParens (Sugar.rMParensType sExpr)
    makeEditor =
      case Sugar.rExpression sExpr of
      Sugar.ExpressionWhere w ->
        wrapNonHoleExpr . parenify $
          WhereEdit.makeWithBody make ancestry w
      Sugar.ExpressionFunc f ->
        wrapNonHoleExpr . parenify $ FuncEdit.make make ancestry (Sugar.rExpressionPtr sExpr) f
      Sugar.ExpressionHole holeState ->
        isAHole . maybe id (error "parens on hole?") (Sugar.rMParensType sExpr) $ HoleEdit.make ancestry holeState sExpr
      Sugar.ExpressionGetVariable varRef ->
        notAHole . parenify $ VarEdit.make varRef
      Sugar.ExpressionApply apply ->
        wrapNonHoleExpr . parenify $ ApplyEdit.make make ancestry (Sugar.rExpressionPtr sExpr) apply
      Sugar.ExpressionLiteralInteger integer ->
        notAHole . parenify $ LiteralEdit.makeInt exprI integer
  (holePicker, widget) <- makeEditor exprId
  eventMap <- expressionEventMap sExpr holePicker
  return $ Widget.weakerEvents eventMap widget

expressionEventMap
  :: MonadF m
  => Sugar.ExpressionRef m
  -> HoleResultPicker m
  -> CTransaction ViewTag m (EventHandlers (Transaction ViewTag m))
expressionEventMap sExpr holePicker = do
  return . mconcat $
    [ moveUnlessOnHole .
      Widget.actionEventMapMovesCursor
      Config.giveAsArgumentKeys "Give as argument" .
      WidgetIds.diveIn $ DataOps.giveAsArg (Sugar.rExpressionPtr sExpr)
    , moveUnlessOnHole .
      Widget.actionEventMapMovesCursor
      Config.callWithArgumentKeys "Call with argument" .
      WidgetIds.diveIn $ DataOps.callWithArg (Sugar.rExpressionPtr sExpr)

    -- Move to next arg overrides add arg's keys.
    , moveToNextArgHole
    , ifHole pickResultFirst .
      Widget.actionEventMapMovesCursor
      Config.addNextArgumentKeys (ifHole (const ("Pick result and " ++)) "Add arg") .
      liftM WidgetIds.fromGuid $ Sugar.addNextArg actions

    -- Replace has the keys of Delete if delete is not available.
    , maybe mempty
      (Widget.actionEventMapMovesCursor Config.delKeys "Delete" .
       liftM WidgetIds.fromGuid)
      (Sugar.mDelete actions)
    , maybe mempty
      (Widget.actionEventMapMovesCursor (Config.replaceKeys ++ Config.delKeys) "Replace" .
       liftM (WidgetIds.delegating . WidgetIds.fromGuid))
      (Sugar.mReplace actions)

    , Widget.actionEventMapMovesCursor
      Config.lambdaWrapKeys "Lambda wrap" .
      liftM (WidgetIds.delegating . WidgetIds.fromGuid) $ Sugar.lambdaWrap actions
    ]
  where
    actions = Sugar.rActions sExpr
    moveUnlessOnHole = ifHole $ (const . fmap . liftM . Widget.atECursor . const) Nothing
    pickResultFirst = maybe id (fmap . joinEvents)
    ifHole whenHole = foldHolePicker id whenHole holePicker
    joinEvents x y = do
      r <- liftM Widget.eAnimIdMapping x
      (liftM . Widget.atEAnimIdMapping) (. r) y
    moveToNextArgHole =
      case Sugar.mNextArg actions of
      Nothing -> mempty
      Just nextArg ->
        case Sugar.rExpression nextArg of
        Sugar.ExpressionHole{} ->
          ifHole pickResultFirst .
          Widget.actionEventMapMovesCursor
          Config.addNextArgumentKeys (ifHole (const ("Pick result and " ++)) "Move to next arg") .
          liftM WidgetIds.fromIRef . Property.get $ Sugar.rExpressionPtr nextArg
        _ -> mempty
