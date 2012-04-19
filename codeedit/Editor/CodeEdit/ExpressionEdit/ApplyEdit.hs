{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.ApplyEdit(make) where

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.Store.Property (Property(Property))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, assignCursor, transaction)
import Editor.CodeEdit.Ancestry (AncestryItem(..), ApplyParent(..))
import Editor.CodeEdit.ExpressionEdit.ExpressionMaker (ExpressionEditMaker)
import Editor.DataOps (ExpressionPtr)
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Ancestry as Ancestry
import qualified Editor.CodeEdit.Infix as Infix
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => ExpressionEditMaker m
  -> Ancestry.ExpressionAncestry m
  -> ExpressionPtr m
  -> Data.Apply
  -> Widget.Id
  -> TWidget ViewTag m
make makeExpressionEdit ancestry expressionPtr apply@(Data.Apply funcI argI) myId = do
  expressionI <- getP expressionPtr
  assignCursor myId (WidgetIds.fromIRef argI) $ do
    isInfix <- transaction $ Infix.isInfixFunc funcI
    isApplyOfInfix <- transaction $ Infix.isApplyOfInfixOp funcI
    mInfixOpOfRArg <- transaction $ Infix.infixFuncOfRArg funcI
    let
      funcType
        | isInfix = Ancestry.InfixLeft
        | isApplyOfInfix = Ancestry.InfixRight
        | otherwise = Ancestry.Prefix
      expressionRef = Transaction.fromIRef expressionI
      delArgTarget = fromMaybe funcI mInfixOpOfRArg
      addDelEventMap target =
        liftM . Widget.weakerEvents .
        Widget.actionEventMapMovesCursor Config.delKeys "Delete" .
        liftM WidgetIds.fromIRef .
        (>> return target) . DataOps.replace expressionPtr
      funcIPtr = Property (return funcI) $ Property.set expressionRef . Data.ExpressionApply . (`Data.Apply` argI)
      argIPtr = Property (return argI) $ Property.set expressionRef . Data.ExpressionApply . (funcI `Data.Apply`)

    let
      makeAncestry role =
        AncestryItemApply (ApplyParent role funcType apply expressionPtr) : ancestry
    funcEdit <-
      addDelEventMap argI argI $
      makeExpressionEdit (makeAncestry Ancestry.ApplyFunc) funcIPtr

    argEdit <-
      addDelEventMap delArgTarget funcI $
      makeExpressionEdit (makeAncestry Ancestry.ApplyArg) argIPtr

    return . BWidgets.hbox $
      (if isInfix then reverse else id)
      [funcEdit, BWidgets.spaceWidget, argEdit]
