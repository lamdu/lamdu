{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit(make) where

import Control.Arrow (first, second)
import Control.Monad (liftM)
import Data.Monoid (Monoid(..))
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (CTransaction, getP, transaction)
import Editor.CodeEdit.Ancestry(AncestryItem(..))
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker(ExpressionEditMaker)
import Editor.MonadF (MonadF)
import Graphics.UI.Bottle.Widget (EventHandlers)
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Ancestry as Ancestry
import qualified Editor.CodeEdit.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Editor.CodeEdit.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Editor.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Editor.CodeEdit.ExpressionEdit.WhereEdit as WhereEdit
import qualified Editor.CodeEdit.NextArg as NextArg
import qualified Editor.CodeEdit.Parens as Parens
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Data as Data
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
make ancestry exprPtr = do
  sExpr <- transaction $ Sugar.getExpression exprPtr
  exprI <- getP exprPtr
  let
    notAHole = (fmap . liftM) ((,) NotAHole)
    wrapNonHole keys isDelegating f =
      notAHole . BWidgets.wrapDelegatedWithKeys keys isDelegating f
    makeEditor =
      case sExpr of
      Sugar.ExpressionWhere w ->
        wrapNonHole Config.exprFocusDelegatorKeys
            FocusDelegator.Delegating id $
          WhereEdit.makeWithBody make ancestry w
      Sugar.ExpressionFunc f ->
        wrapNonHole Config.exprFocusDelegatorKeys
          FocusDelegator.Delegating id $
          FuncEdit.make make ancestry exprPtr f
      Sugar.ExpressionHole holeState ->
        (fmap . liftM . first) IsAHole .
        BWidgets.wrapDelegatedWithKeys
          FocusDelegator.defaultKeys FocusDelegator.Delegating second $
          HoleEdit.make ancestry holeState exprPtr
      Sugar.ExpressionGetVariable varRef -> notAHole (VarEdit.make varRef)
      Sugar.ExpressionApply apply ->
        wrapNonHole Config.exprFocusDelegatorKeys
          FocusDelegator.Delegating id $
        ApplyEdit.make make ancestry exprPtr apply
      Sugar.ExpressionLiteralInteger integer ->
        wrapNonHole FocusDelegator.defaultKeys
          FocusDelegator.NotDelegating id $
        LiteralEdit.makeInt exprI integer
    exprId = WidgetIds.fromIRef exprI
  (holePicker, widget) <- makeEditor exprId
  eventMap <- expressionEventMap ancestry exprPtr holePicker

  liftM (Widget.weakerEvents eventMap) $
    Parens.addParens exprId sExpr ancestry widget

expressionEventMap
  :: MonadF m
  => Ancestry.ExpressionAncestry m
  -> Transaction.Property ViewTag m (IRef Data.Expression)
  -> HoleResultPicker m
  -> CTransaction ViewTag m (EventHandlers (Transaction ViewTag m))
expressionEventMap ancestry exprPtr holePicker = do
  (addArgDoc, addArgHandler) <-
    transaction $ NextArg.makeAddArgHandler ancestry exprPtr
  return . mconcat $
    [ moveUnlessOnHole .
      Widget.actionEventMapMovesCursor
      Config.giveAsArgumentKeys "Give as argument" .
      WidgetIds.diveIn $ DataOps.giveAsArg exprPtr
    , moveUnlessOnHole .
      Widget.actionEventMapMovesCursor
      Config.callWithArgumentKeys "Call with argument" .
      WidgetIds.diveIn $ DataOps.callWithArg exprPtr
    , pickResultFirst $
      Widget.actionEventMapMovesCursor
      Config.addNextArgumentKeys addArgDoc addArgHandler
    , ifHole (const mempty) $ replaceEventMap ancestry exprPtr
    , Widget.actionEventMapMovesCursor
      Config.lambdaWrapKeys "Lambda wrap" . WidgetIds.diveIn $
      DataOps.lambdaWrap exprPtr
    ]
  where
    moveUnlessOnHole = ifHole $ (const . fmap . liftM . Widget.atECursor . const) Nothing
    pickResultFirst = ifHole $ maybe id (fmap . joinEvents)
    ifHole whenHole = foldHolePicker id whenHole holePicker
    joinEvents x y = do
      r <- liftM Widget.eAnimIdMapping x
      (liftM . Widget.atEAnimIdMapping) (. r) y

replaceEventMap
  :: (Monad m, Functor m)
  => Ancestry.ExpressionAncestry m
  -> Transaction.Property ViewTag m (IRef Data.Expression)
  -> Widget.EventHandlers (Transaction ViewTag m)
replaceEventMap ancestry exprPtr =
  Widget.actionEventMapMovesCursor
  (Config.replaceKeys ++ extraReplaceKeys) "Replace" . WidgetIds.diveIn $
  DataOps.replaceWithHole exprPtr
  where
    extraReplaceKeys =
      case ancestry of
        (AncestryItemApply _ : _) -> []
        _ -> Config.delKeys
