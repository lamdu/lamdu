{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.ApplyEdit(make) where

import Control.Monad (liftM, liftM2)
import Data.ByteString.Char8 (pack)
import Data.Store.Property (Property(Property))
import Data.Store.Transaction (Transaction)
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, subCursor, assignCursor, transaction)
import Editor.CodeEdit.Types(ApplyParent(..), ApplyRole(..))
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

needParen
  :: Monad m
  => Data.Apply -> ETypes.ExpressionAncestry m
  -> Transaction ViewTag m Bool
needParen _ (ApplyParent { apRole = ApplyArg, apFuncType = ETypes.Prefix } : _) =
  return True
needParen
  (Data.Apply funcI _)
  (ApplyParent { apRole = ApplyArg } : _) =
    liftM2 (||)
    (ETypes.isInfixFunc funcI) (ETypes.isApplyOfInfixOp funcI)
needParen (Data.Apply funcI _) [] =
  ETypes.isInfixFunc funcI
needParen (Data.Apply funcI _)
  (ApplyParent { apRole = ApplyFunc } : _) =
  ETypes.isApplyOfInfixOp funcI

make ::
  (MonadF m) =>
  (ETypes.ExpressionAncestry m
   -> ETypes.ExpressionPtr m
   -> TWidget ViewTag m)
  -> ETypes.ExpressionAncestry m
  -> ETypes.ExpressionPtr m
  -> Data.Apply
  -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit ancestry expressionPtr apply@(Data.Apply funcI argI) myId = do
  expressionI <- getP expressionPtr
  assignCursor myId (WidgetIds.fromIRef argI) $ do
    isInfix <- transaction $ ETypes.isInfixFunc funcI
    isApplyOfInfix <- transaction $ ETypes.isApplyOfInfixOp funcI
    let
      funcType
        | isInfix = ETypes.InfixLeft
        | isApplyOfInfix = ETypes.InfixRight
        | otherwise = ETypes.Prefix
      expressionRef = Transaction.fromIRef expressionI
      delEventMap = Widget.actionEventMapMovesCursor Config.delKeys "Delete" . setExpr
      funcIPtr = Property (return funcI) $ Property.set expressionRef . Data.ExpressionApply . (`Data.Apply` argI)
      argIPtr = Property (return argI) $ Property.set expressionRef . Data.ExpressionApply . (funcI `Data.Apply`)
      setExpr newExprI = do
        Property.set expressionPtr newExprI
        return $ WidgetIds.fromIRef newExprI

      addDelEventMap =
        liftM . Widget.weakerEvents . delEventMap

    let
      makeAncestry role =
        ApplyParent {
          apRole = role,
          apFuncType = funcType,
          apApply = apply,
          apParentPtr = expressionPtr
          }
        : ancestry
    funcEdit <-
      addDelEventMap argI $
      makeExpressionEdit (makeAncestry ETypes.ApplyFunc) funcIPtr

    argEdit <-
      addDelEventMap funcI $
      makeExpressionEdit (makeAncestry ETypes.ApplyArg) argIPtr

    exprNeedParen <- transaction $ needParen apply ancestry
    let
      makeParensId (ad : _) = do
        parentI <- getP $ apParentPtr ad
        return $
          Widget.joinId (WidgetIds.fromIRef parentI)
          [pack . show $ apRole ad]
      makeParensId [] = return $ Widget.Id ["root parens"]
      rParenId = Widget.joinId myId [")"]
      addParens widget
        | exprNeedParen = do
          mInsideParenId <- subCursor rParenId
          parensId <- makeParensId ancestry
          widgetWithParens <- ETypes.addParens id
            (>>= BWidgets.makeFocusableView rParenId)
            parensId widget
          return $ maybe id (const highlightExpression) mInsideParenId widgetWithParens
        | otherwise = return widget

      highlightExpression =
        Widget.backgroundColor WidgetIds.parenHighlightId Config.parenHighlightColor

    addParens . BWidgets.hbox $
      (if isInfix then reverse else id)
      [funcEdit, BWidgets.spaceWidget, argEdit]
