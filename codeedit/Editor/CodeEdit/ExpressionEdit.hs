{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit(make) where

import Control.Arrow (first)
import Control.Monad (liftM)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker(ExpressionEditMaker)
import Editor.CTransaction (CTransaction)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (EventHandlers)
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Editor.CodeEdit.ExpressionEdit.BindingFuncTypeEdit as BindingFuncTypeEdit
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Editor.CodeEdit.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Editor.CodeEdit.ExpressionEdit.SectionEdit as SectionEdit
import qualified Editor.CodeEdit.ExpressionEdit.SimpleFuncTypeEdit as SimpleFuncTypeEdit
import qualified Editor.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Editor.CodeEdit.ExpressionEdit.WhereEdit as WhereEdit
import qualified Editor.CodeEdit.Parens as Parens
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
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
make sExpr = do
  let
    parenify mkParens hasParens mkWidget myId =
      mkWidget myId >>=
      case hasParens of
      Sugar.HaveParens -> mkParens myId
      Sugar.DontHaveParens -> return
    isAHole = (fmap . liftM . first) IsAHole
    notAHole = (fmap . liftM) ((,) NotAHole)
    wrapNonHoleExpr =
      notAHole .
      BWidgets.wrapDelegatedWithKeys Config.exprFocusDelegatorKeys FocusDelegator.Delegating id
    exprId = WidgetIds.fromGuid . Sugar.guid . Sugar.rActions $ sExpr
    textParenify = parenify Parens.addHighlightedTextParens
    squareParenify = parenify Parens.addSquareParens
    makeEditor =
      case Sugar.rExpression sExpr of
      Sugar.ExpressionWhere hasParens w ->
        wrapNonHoleExpr . squareParenify hasParens $
          WhereEdit.makeWithBody make w
      Sugar.ExpressionFunc hasParens f ->
        wrapNonHoleExpr . textParenify hasParens $ FuncEdit.make make f
      Sugar.ExpressionHole hole ->
        isAHole . HoleEdit.make hole . Sugar.guid $ Sugar.rActions sExpr
      Sugar.ExpressionGetVariable varRef ->
        notAHole {- TODO: May need parenification -} $ VarEdit.make varRef
      Sugar.ExpressionApply hasParens apply ->
        wrapNonHoleExpr . textParenify hasParens $ ApplyEdit.make make apply
      Sugar.ExpressionSimpleFuncType hasParens funcType ->
        wrapNonHoleExpr . textParenify hasParens $ SimpleFuncTypeEdit.make make funcType
      Sugar.ExpressionBindingFuncType hasParens funcType ->
        wrapNonHoleExpr . textParenify hasParens $ BindingFuncTypeEdit.make make funcType
      Sugar.ExpressionSection hasParens section ->
        wrapNonHoleExpr . textParenify hasParens $ SectionEdit.make make section
      Sugar.ExpressionLiteralInteger integer ->
        notAHole $ LiteralEdit.makeInt integer
      Sugar.ExpressionPi ->
        notAHole $ BWidgets.makeFocusableTextView "π"
  (holePicker, widget) <- makeEditor exprId
  eventMap <- expressionEventMap sExpr holePicker
  return $ Widget.weakerEvents eventMap widget

expressionEventMap
  :: MonadF m
  => Sugar.ExpressionRef m
  -> HoleResultPicker m
  -> CTransaction ViewTag m (EventHandlers (Transaction ViewTag m))
expressionEventMap sExpr holePicker =
  return . mconcat $
    [ moveUnlessOnHole .
      Widget.actionEventMapMovesCursor
      Config.giveAsArgumentKeys "Give as argument" .
      liftM WidgetIds.fromGuid $ Sugar.giveAsArg actions
    , moveUnlessOnHole .
      Widget.actionEventMapMovesCursor
      Config.callWithArgumentKeys "Call with argument" .
      liftM WidgetIds.fromGuid $ Sugar.callWithArg actions

    -- Move to next arg overrides add arg's keys.
    , moveToNextArgHole
    , withPickResultFirst Config.addNextArgumentKeys "Add arg" .
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
      liftM (WidgetIds.delegating . WidgetIds.paramId) $ Sugar.lambdaWrap actions
    ]
  where
    withPickResultFirst keys doc action=
      ifHole pickResultFirst .
      Widget.actionEventMapMovesCursor
      keys (ifHole (const ("Pick result and " ++)) doc) $ action
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
          withPickResultFirst Config.addNextArgumentKeys "Move to next arg" .
          return . WidgetIds.fromGuid . Sugar.guid . Sugar.rActions $ nextArg
        _ -> mempty
