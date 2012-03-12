{-# LANGUAGE OverloadedStrings #-}
module Editor.CodeEdit.ExpressionEdit.ApplyEdit(make) where

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.Store.Property (Property(Property))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, getP, assignCursor, transaction)
import Editor.CodeEdit.Types(AncestryItem(..), ApplyParent(..))
import Editor.MonadF (MonadF)
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Types as ETypes
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.DataOps as DataOps
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget

make
  :: MonadF m
  => ETypes.ExpressionEditMaker m
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
    mInfixOpOfRArg <- transaction $ ETypes.infixFuncOfRArg funcI
    let
      funcType
        | isInfix = ETypes.InfixLeft
        | isApplyOfInfix = ETypes.InfixRight
        | otherwise = ETypes.Prefix
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
      makeExpressionEdit (makeAncestry ETypes.ApplyFunc) funcIPtr

    argEdit <-
      addDelEventMap delArgTarget funcI $
      makeExpressionEdit (makeAncestry ETypes.ApplyArg) argIPtr

    return . BWidgets.hbox $
      (if isInfix then reverse else id)
      [funcEdit, BWidgets.spaceWidget, argEdit]
