{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit(make) where

import Control.Arrow (first, second)
import Control.Monad (liftM, liftM2)
import Data.Monoid (Monoid(..))
import Data.Store.IRef (IRef)
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, transaction, subCursor)
import Editor.CodeEdit.Types(AncestryItem(..), ApplyParent(..), ApplyRole(..))
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.Anchors as Anchors
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Editor.CodeEdit.ExpressionEdit.FuncEdit as FuncEdit
import qualified Editor.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Editor.CodeEdit.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Editor.CodeEdit.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Editor.CodeEdit.ExpressionEdit.VarEdit as VarEdit
import qualified Editor.CodeEdit.ExpressionEdit.WhereEdit as WhereEdit
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.CodeEdit.Types as ETypes
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

data HighlightParens = DoHighlightParens | DontHighlightParens

makeCondParensId
  :: Monad m
  => Bool -> ETypes.ExpressionAncestry m
  -> Transaction ViewTag m (Maybe Widget.Id)
makeCondParensId False = const $ return Nothing
makeCondParensId True = liftM Just . ETypes.makeParensId

getParensInfo
  :: Monad m
  => Sugar.Expression m -> ETypes.ExpressionAncestry m
  -> Transaction ViewTag m (Maybe (HighlightParens, Widget.Id))
getParensInfo (Sugar.ExpressionPlain exprI) ancestry = do
  expr <- Transaction.readIRef exprI
  case (expr, ancestry) of
    (Data.ExpressionApply _, AncestryItemApply (ApplyParent ApplyArg ETypes.Prefix _ _) : _) ->
      setDoHighlight . liftM Just $ ETypes.makeParensId ancestry
    (Data.ExpressionApply (Data.Apply funcI _), AncestryItemApply (ApplyParent ApplyArg _ _ _) : _) -> do
      isInfix <-
        liftM2 (||)
        (ETypes.isInfixFunc funcI) (ETypes.isApplyOfInfixOp funcI)
      setDoHighlight $ makeCondParensId isInfix ancestry
    (Data.ExpressionApply (Data.Apply funcI _), AncestryItemApply (ApplyParent ApplyFunc _ _ _) : _) -> do
      isInfix <- ETypes.isApplyOfInfixOp funcI
      setDoHighlight $ makeCondParensId isInfix ancestry
    (Data.ExpressionApply (Data.Apply funcI _), _) -> do
      isInfix <- ETypes.isInfixFunc funcI
      setDoHighlight $ makeCondParensId isInfix ancestry
    (Data.ExpressionGetVariable _, AncestryItemApply (ApplyParent ApplyFunc _ _ _) : _) ->
      return Nothing
    (Data.ExpressionGetVariable var, _) -> do
      name <- Property.get $ Anchors.variableNameRef var
      setDontHighlight $ makeCondParensId (ETypes.isInfixName name) ancestry
    (Data.ExpressionLambda _, AncestryItemApply _ : _) ->
      setDoHighlight $ liftM Just $ ETypes.makeParensId ancestry
    _ -> return Nothing
  where
    setDoHighlight = (liftM . fmap) ((,) DoHighlightParens)
    setDontHighlight = (liftM . fmap) ((,) DontHighlightParens)
getParensInfo (Sugar.ExpressionWhere _) ancestry =
  if ETypes.isAncestryRHS ancestry
  then return Nothing
  else liftM (Just . (,) DontHighlightParens) $ ETypes.makeParensId ancestry
getParensInfo _ _ =
  return Nothing

make :: MonadF m => ETypes.ExpressionEditMaker m
make ancestry exprPtr = do
  sExpr <- transaction $ Sugar.getExpression exprPtr
  let
    notAHole = (fmap . liftM) ((,) NotAHole)
    wrapNonHole keys isDelegating f =
      notAHole . BWidgets.wrapDelegatedWithKeys keys isDelegating f
    makeEditor =
      case sExpr of
      Sugar.ExpressionWhere w ->
        return .
          wrapNonHole Config.exprFocusDelegatorKeys
            FocusDelegator.Delegating id $
          WhereEdit.make make ancestry w
      Sugar.ExpressionFunc f ->
        return .
          wrapNonHole Config.exprFocusDelegatorKeys
            FocusDelegator.Delegating id $
          FuncEdit.make make ancestry exprPtr f
      Sugar.ExpressionPlain exprI -> do
        expr <- transaction $ Transaction.readIRef exprI
        return $ case expr of
          Data.ExpressionHole holeState ->
            (fmap . liftM . first) IsAHole .
            BWidgets.wrapDelegatedWithKeys
              FocusDelegator.defaultKeys FocusDelegator.Delegating second $
              HoleEdit.make ancestry holeState exprPtr
          Data.ExpressionGetVariable varRef -> notAHole (VarEdit.make varRef)
          Data.ExpressionLambda lambda ->
            wrapNonHole Config.exprFocusDelegatorKeys
              FocusDelegator.Delegating id $
            LambdaEdit.make make ancestry exprPtr lambda
          Data.ExpressionApply apply ->
            wrapNonHole Config.exprFocusDelegatorKeys
              FocusDelegator.Delegating id $
            ApplyEdit.make make ancestry exprPtr apply
          Data.ExpressionLiteralInteger integer ->
            wrapNonHole FocusDelegator.defaultKeys
              FocusDelegator.NotDelegating id $
            LiteralEdit.makeInt exprI integer
  exprI <- getP exprPtr
  let exprId = WidgetIds.fromIRef exprI
  (holePicker, widget) <- ($ exprId) =<< makeEditor
  (addArgDoc, addArgHandler) <-
    transaction $ ETypes.makeAddArgHandler ancestry exprPtr
  let
    eventMap = mconcat
      [ moveUnlessOnHole .
        Widget.actionEventMapMovesCursor
        Config.giveAsArgumentKeys "Give as argument" .
        ETypes.diveIn $ DataOps.giveAsArg exprPtr
      , moveUnlessOnHole .
        Widget.actionEventMapMovesCursor
        Config.callWithArgumentKeys "Call with argument" .
        ETypes.diveIn $ DataOps.callWithArg exprPtr
      , pickResultFirst $
        Widget.actionEventMapMovesCursor
        Config.addNextArgumentKeys addArgDoc addArgHandler
      , ifHole (const mempty) $ replaceEventMap ancestry exprPtr
      , Widget.actionEventMapMovesCursor
        Config.lambdaWrapKeys "Lambda wrap" . ETypes.diveIn $
        DataOps.lambdaWrap exprPtr
      ]
    moveUnlessOnHole = ifHole $ (const . fmap . liftM . Widget.atECursor . const) Nothing
    pickResultFirst = ifHole $ maybe id (fmap . joinEvents)
    ifHole isHole = foldHolePicker id isHole holePicker
    joinEvents x y = do
      r <- liftM Widget.eAnimIdMapping x
      (liftM . Widget.atEAnimIdMapping) (. r) y

  mParenInfo <- transaction $ getParensInfo sExpr ancestry
  liftM (Widget.weakerEvents eventMap) $
    addParens exprId mParenInfo widget

replaceEventMap
  :: (Monad m, Functor m)
  => ETypes.ExpressionAncestry m
  -> Transaction.Property ViewTag m (IRef Data.Expression)
  -> Widget.EventHandlers (Transaction ViewTag m)
replaceEventMap ancestry exprPtr =
  Widget.actionEventMapMovesCursor
  (Config.replaceKeys ++ extraReplaceKeys) "Replace" . ETypes.diveIn $
  DataOps.replaceWithHole exprPtr
  where
    extraReplaceKeys =
      case ancestry of
        (AncestryItemApply _ : _) -> []
        _ -> Config.delKeys

highlightExpression :: Widget.Widget f -> Widget.Widget f
highlightExpression =
  Widget.backgroundColor WidgetIds.parenHighlightId Config.parenHighlightColor

addParens
  :: (Monad m, Functor m)
  => Widget.Id
  -> Maybe (HighlightParens, Widget.Id)
  -> Widget.Widget (Transaction t m)
  -> TWidget t m
addParens _ Nothing widget = return widget
addParens myId (Just (needHighlight, parensId)) widget = do
  mInsideParenId <- subCursor rParenId
  widgetWithParens <- ETypes.addParens id doHighlight parensId widget
  return $ maybe id (const highlightExpression) mInsideParenId widgetWithParens
  where
    rParenId = Widget.joinId myId [")"]
    doHighlight =
      case needHighlight of
        DoHighlightParens -> (>>= BWidgets.makeFocusableView rParenId)
        DontHighlightParens -> id
